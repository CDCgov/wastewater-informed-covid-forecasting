#' Create hub submission files
#'
#' @description
#' This function takes in the combined set of wastewater and hospital admissions
#' quantiles by forecast date, date, and location, creates a dataframe
#' formatted as it would have been for submission to the COVID forecast Hub,
#' and saves this to disk if specified. It returns the metadata on the number
#' of locations total and the number of locations using the wastewater model
#' for each forecast date submission saved.
#' It assumes the wastewater-informed model output quantiles passed in have already been
#' filtered to remove ones that we would wish to exclude (for convergence
#' diagnostics or other reasons)
#'
#' @param hosp_quantiles_ww a dataframe of the probabilistic estimates of
#' hospital admissions formatted as quantiles, from the wastewater model,
#' for multiple forecast dates if present for a particular location
#' @param hosp_quantiles_hosp a dataframe of the probabilistic estimates of
#' hospital admissions formatted as quantiles, from the hospital admissions
#' model, for multiple forecast dates
#' @param forecast_dates vector of forecast dates that we want to create
#' mock submissions for
#' @param hub_subdir character string of the outer directory specifying where to
#' write the submission files to (which will be named by forecast date)
#' @param model_name character string indicating the name of the model. This will
#' be used to determine both the name of the submission `.csv` file and the name of
#' its enclosing directory, per COVID-19 Forecast Hub formatting.
#' @param scenario string indicating which wastewater availability scenario to use when creating
#' the hub submission file. Default `"status_quo"` (all actually available wastewater data).
#' @param save_files Save the created submission data frame to disk as a `.csv`
#'  file? Boolean, default `TRUE`.
#'
#' @return metadata_df a dataframe that has one row per forecast date indicating
#' thenumber of wastewater submissions in that date and the number of total
#' locations
#' @export
#'
create_hub_submissions <- function(hosp_quantiles_ww,
                                   hosp_quantiles_hosp,
                                   forecast_dates,
                                   hub_subdir,
                                   model_name,
                                   scenario = "status_quo",
                                   save_files = TRUE) {
  hosp_quantiles_ww <- hosp_quantiles_ww |>
    dplyr::filter(scenario == {{ scenario }})
  metadata_df <- data.frame()
  for (i in seq_along(forecast_dates)) {
    forecast_date <- forecast_dates[i]
    first_target_date <- forecast_date + lubridate::days(1)
    ww_quantiles <- hosp_quantiles_ww |>
      dplyr::filter(
        forecast_date == !!forecast_date,
        date >= !!first_target_date
      )
    hosp_quantiles <- hosp_quantiles_hosp |>
      dplyr::filter(
        forecast_date == !!forecast_date,
        date >= !!first_target_date
      )

    all_locs <- unique(hosp_quantiles$location)

    # For each location in the hospital admissions data, use the
    # wastewater model unless it is missing, then use the
    # hospital admissions model.
    full_quantiles <- data.frame()
    for (j in seq_along(all_locs)) {
      if (all_locs[j] %in% c(unique(ww_quantiles$location))) {
        this_loc_quantiles <- ww_quantiles |>
          dplyr::filter(location == all_locs[j])
      } else { # get from the hosp quantiles
        this_loc_quantiles <- hosp_quantiles |>
          dplyr::filter(location == all_locs[j])
      }
      full_quantiles <- dplyr::bind_rows(full_quantiles, this_loc_quantiles)
    }

    # Format for the hub
    submission_df <- format_for_hub(full_quantiles)

    # A few quality checks
    n_models_ww <- full_quantiles |>
      dplyr::select(location, model_type) |>
      unique() |>
      dplyr::filter(model_type == "ww") |>
      nrow()
    message("Number of locations submitting wastewater model:", n_models_ww)
    n_locs <- full_quantiles |>
      dplyr::select(location) |>
      unique() |>
      nrow()
    message("Number of locations in submission:", n_locs)
    metadata_df <- dplyr::bind_rows(
      metadata_df,
      data.frame(forecast_date, n_models_ww, n_locs)
    )


    if (isTRUE(save_files)) {
      stopifnot(
        "Don't have forecasts for all locations, not writing to disk" =
          n_locs >= 51 # temporarily relax bc model convergence flags can
        # lead to missing models for hosp model as well, in which case we wouldn't
        # have submitted
      )

      cfaforecastrenewalww::create_dir(file.path(hub_subdir, model_name))

      readr::write_csv(submission_df, file.path(
        hub_subdir, model_name,
        glue::glue("{forecast_date}-{model_name}.csv")
      ))
    }
  } # end loop over forecast dates

  return(metadata_df)
}

#' Format quantiles for the current COVID Forecast Hub
#' @description This function takes in a dataframe of quantiles and their
#' values and the names of the columns specifying the date, location,
#' value, quantile, and forecast date, and returns a hub formatted dataframe
#'
#' @param quantiles dataframe of quantiles containing columns with the specified
#' names for date, value, location, forecast date, and quantile
#' @param date_col_name string indicating the column name for date of the
#' forecasted quantity, default is `date`
#' @param value_col_name string indicating the column name for the value of the
#' prediction at the date, default is `value`
#' @param loc_col_name string indicating the column name for the state
#' abbreviation corresponding to the forecast location, default `location`
#' @param forecast_date_col_name string indicating the column name for the forecast
#' date, default is `forecast_date`
#' @param quantile_col_name string indicating the column name for the quantile
#' value, default is `quantile`
#'
#' @return a dataframe in Hub formatting
#' @export
#'
format_for_hub <- function(quantiles,
                           date_col_name = "date",
                           value_col_name = "value",
                           loc_col_name = "location",
                           forecast_date_col_name = "forecast_date",
                           quantile_col_name = "quantile") {
  formatted_quantiles <- quantiles |>
    dplyr::rename(
      target_end_date = {{ date_col_name }},
      value = {{ value_col_name }},
      location = {{ loc_col_name }},
      forecast_date = {{ forecast_date_col_name }},
      quantile = {{ quantile_col_name }}
    ) |>
    dplyr::mutate(
      location = cfaforecastrenewalww::loc_abbr_to_flusight_code(location),
      quantile = round(quantile, 4),
    ) |>
    dplyr::filter(
      target_end_date >= lubridate::ymd(forecast_date) + lubridate::days(1)
    ) |>
    dplyr::mutate(days_ahead = as.numeric(target_end_date - forecast_date)) |>
    dplyr::mutate(
      target = glue::glue("{days_ahead} day ahead inc hosp"),
      type = "quantile"
    ) |>
    dplyr::select(
      target, location, forecast_date, target_end_date,
      quantile, value, type
    )

  return(formatted_quantiles)
}

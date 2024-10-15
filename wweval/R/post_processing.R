#' Get model draws combined with input and evaluation data
#'
#' @param fit_obj_wwinference wwinference_fit object that is returned when
#' wwinference::wwinference() is run.
#' @param model_output the type of model expected observation you want,
#' options are "hosp" and "ww"
#' @param model_type The type of model, options are "ww" and "hosp"
#' @param forecast_date The date the forecast was made
#' @param scenario A name for the scenario that the input
#' data represents, as a string.
#' @param location The location for which the model is being run.
#' @param eval_data The retrospective dataset used to evaluate the model
#' (should have data beyond the forecast date)
#' @return a dataframe of model draws subsetted to only the specified output
#' type, joined with the evaluation data and the input calibration data
#' @export
new_get_model_draws_w_data <- function(fit_obj_wwinference,
                                       model_output,
                                       model_type = c("ww", "hosp"),
                                       forecast_date,
                                       scenario,
                                       location,
                                       eval_data) {
  model_type <- arg_match(model_type)
  eval_data <- eval_data |>
    dplyr::filter(location == !!location)
  stopifnot(
    "More than one location in eval data that is getting joined" =
      eval_data |> dplyr::pull(location) |> unique() |> length() == 1
  )


  # Dataframe with columns
  if (model_output == "hosp") {
    new_hosp_draws <- wwinference::get_draws(
      fit_obj_wwinference,
      what = "predicted_counts"
    )$predicted_counts

    draws_w_data <- new_hosp_draws |>
      dplyr::mutate(
        "name" = "pred_hosp",
        "forecast_date" = lubridate::ymd(!!forecast_date),
        "model_type" = !!model_type,
        "location" = !!location,
        "scenario" = !!scenario
      ) |>
      dplyr::rename(
        "value" = "pred_value",
        "calib_data" = "observed_value",
        "pop" = "total_pop"
      ) |>
      dplyr::left_join(
        eval_data |>
          dplyr::select(-"total_pop", -"location"),
        by = c("date")
      ) |>
      dplyr::rename("eval_data" = "count") |>
      dplyr::ungroup()
  }

  if (model_output == "ww") {
    new_ww_draws <- wwinference::get_draws(
      fit_obj_wwinference,
      what = "predicted_ww"
    )$predicted_ww


    draws_w_data <- new_ww_draws |>
      dplyr::left_join(
        eval_data |>
          dplyr::select(
            "date",
            "log_genome_copies_per_ml",
            "lab",
            "site",
            "exclude"
          ) |>
          unique(),
        by = c("date", "lab", "site")
      ) |>
      dplyr::rename(
        "ww_pop" = "subpop_pop",
        "site_lab_name" = "lab_site_name",
        "flag_as_ww_outlier" = "exclude",
        "below_LOD" = "below_lod"
      ) |>
      dplyr::mutate(
        "name" = "pred_ww",
        "value" = exp(.data$pred_value),
        "calib_data" = exp(.data$observed_value),
        "eval_data" = exp(.data$log_genome_copies_per_ml),
        "lod_sewage" = exp(.data$log_lod),
        "forecast_date" = lubridate::ymd(!!forecast_date),
        "model_type" = !!model_type,
        "scenario" = !!scenario,
        "location" = !!location
      ) |>
      dplyr::ungroup() |>
      dplyr::select(
        "name", "lab_site_index", "value", "draw", "date", "site", "lab",
        "location", "ww_pop", "calib_data", "below_LOD", "lod_sewage",
        "flag_as_ww_outlier", "eval_data", "forecast_date", "model_type",
        "scenario", "site_lab_name"
      )
  }

  return(draws_w_data)
}



#' Get quantiles for state-level generated quantities
#'
#' @param draws a dataframe containing all the draws from the model estimated
#' state-level quantities
#'
#' @return a dataframe containing the quantile value for the quantiles
#' required for the Hub submission
#' @export
get_state_level_quantiles <- function(draws) {
  quantiles <- trajectories_to_quantiles(
    draws,
    timepoint_cols = "date",
    value_col = "value",
    id_cols = c("location", "name", "scenario", "model_type")
  ) |>
    dplyr::rename(
      quantile = quantile_level,
      value = quantile_value
    ) |>
    dplyr::left_join(
      draws |>
        select(-draw, -value) |>
        unique(),
      by = c("date", "name", "location", "scenario", "model_type")
    ) |>
    dplyr::mutate(
      period = dplyr::case_when(
        !is.na(.data$calib_data) ~ "calibration",
        date <= .data$forecast_date ~ "nowcast",
        TRUE ~ "forecast"
      ),
      quantile = round(quantile, 4)
    )

  return(quantiles)
}

#' Get quantiles for site-lab level wastewater
#'
#' @param ww_draws a dataframe containing all the draws from the model estimated
#' site-lab level cocnentrations
#'
#' @return a dataframe containing the quantile value for the quantiles
#' required for the Hub submission for each site lab in the state
#' @export
get_state_level_ww_quantiles <- function(ww_draws) {
  quantiles <- trajectories_to_quantiles(
    ww_draws,
    timepoint_cols = "date",
    value_col = "value",
    id_cols = c("location", "name", "scenario", "model_type", "site_lab_name")
  ) |>
    dplyr::rename(
      quantile = quantile_level,
      value = quantile_value
    ) |>
    dplyr::left_join(
      ww_draws |>
        select(-draw, -value) |>
        unique(),
      by = c(
        "date", "name", "location", "scenario",
        "model_type", "site_lab_name"
      )
    ) |>
    dplyr::mutate(
      period = dplyr::case_when(
        date <= forecast_date ~ "calibration",
        TRUE ~ "forecast"
      ),
      quantile = round(quantile, 4)
    )

  return(quantiles)
}


#' Save table
#' @description This helper function is a wrapper to save intermediate outputs
#' running within the model fit loop as tsvs in the following file strucuture:
#' scenario > forecast_date > model_type > location > type_of_output
#'
#' @param data_to_save The dataframe/tibble to save
#' @param type_of_output The name of the type of output e.g. scores, quantiles,
#' etc.
#' @param output_dir The upper level directory to save these outputs in
#' @param scenario A  string indicating the scenario under which the
#' model was run
#' @param forecast_date A string indicating the date of the forecast,
#' in YYYY-MM-DD
#' @param model_type A string indicating the type of model (either `ww` or `hosp`)
#' @param location A string indicating the location (e.g. 2 letter abbreviation
#' for the state)
#'
#' @return NULL
#' @export
#'
save_table <- function(data_to_save,
                       type_of_output,
                       output_dir,
                       scenario,
                       forecast_date,
                       model_type = c("ww", "hosp"),
                       location) {
  model_type <- arg_match(model_type)
  if (!is.null(data_to_save)) {
    full_dir <- file.path(
      output_dir,
      scenario,
      forecast_date,
      model_type,
      location
    )

    fp <- get_filepath(
      output_dir,
      scenario,
      forecast_date,
      model_type,
      location,
      glue::glue("{type_of_output}"),
      "tsv"
    )



    wwinference::create_dir(full_dir)

    readr::write_tsv(as_tibble(data_to_save),
      file = fp
    )
  }
  return(NULL)
}

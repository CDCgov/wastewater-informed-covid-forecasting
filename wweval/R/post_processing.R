#' Get model draws combined with input and evaluation data
#'
#' @param model_output the type of model expected observation you want,
#' options are "hosp" and "ww"
#' @param model_type The type of model, options are "ww" and "hosp"
#' @param draws The raw draws dataframe
#' @param forecast_date The date the forecast was made
#' @param scenario A name for the scenario that the input
#' data represents, as a string.
#' @param location The location for which the model is being run.
#' @param input_data The input dataset used for fitting the model.
#' @param eval_data The retrospective dataset used to evaluate the model
#' (should have data beyond the forecast date)
#' @param last_hosp_data_date The date of the last hospital admissions data point
#' that the model is calibrated to
#' @param ot An integer indicating the number of days the model is calibrated
#' to hospital admissions data
#' @param forecast_time  An integer indicate the time in days of the forecast
#'
#' @return a dataframe of model draws subsetted to only the specified output
#' type, joined with the evaluation data and the input calibration data
#' @export
get_model_draws_w_data <- function(model_output,
                                   model_type = c("ww", "hosp"),
                                   draws,
                                   forecast_date,
                                   scenario,
                                   location,
                                   input_data,
                                   eval_data,
                                   last_hosp_data_date,
                                   ot,
                                   forecast_time = 28) {
  model_type <- arg_match(model_type)
  nowcast_time <- as.integer(ymd(forecast_date) - ymd(last_hosp_data_date))
  ht <- nowcast_time + forecast_time
  # Date spine for joining data
  date_df <- tibble::tibble(date = seq(
    from = lubridate::ymd(forecast_date) - lubridate::days(ot) -
      lubridate::days(nowcast_time) + lubridate::days(1),
    to = lubridate::ymd(forecast_date) + lubridate::days(forecast_time),
    by = "days"
  )) |>
    mutate(t = row_number())

  eval_data <- eval_data |>
    dplyr::filter(location == !!location)
  stopifnot(
    "More than one location in eval data that is getting joined" =
      eval_data |> dplyr::pull(location) |> unique() |> length() == 1
  )


  # Dataframe with columns
  if (model_output == "hosp") {
    pop <- input_data |>
      dplyr::pull(pop) |>
      unique()

    draws_w_data <- draws |>
      tidybayes::spread_draws(pred_hosp[t]) |>
      dplyr::rename(value = pred_hosp) |>
      dplyr::mutate(
        draw = `.draw`,
        name = "pred_hosp"
      ) |>
      dplyr::select(name, t, value, draw) |>
      dplyr::left_join(date_df, by = "t") |>
      dplyr::left_join(input_data |> select(-pop, -location),
        by = c("date")
      ) |>
      dplyr::rename(calib_data = daily_hosp_admits) |>
      dplyr::left_join(eval_data |> select(-pop, -location),
        by = c("date")
      ) |>
      dplyr::rename(eval_data = daily_hosp_admits) |>
      dplyr::mutate(
        forecast_date = lubridate::ymd(!!forecast_date),
        model_type = !!model_type,
        location = !!location,
        pop = !!pop,
        scenario = !!scenario
      ) |>
      dplyr::ungroup()
  }

  if (model_output == "ww") {
    # Then we also want to output the wastewater predictions
    lab_site_map <- input_data |>
      dplyr::distinct(lab_site_index, site, lab, location)
    # Get mean population in the site over the calibration period, this
    # is the same pop size we use in the model fitting
    site_pop_map <- input_data |>
      dplyr::group_by(site) |>
      dplyr::summarise(ww_pop = mean(ww_pop))

    draws_w_data <- draws |>
      tidybayes::spread_draws(pred_ww[lab_site_index, t]) |>
      dplyr::rename(value = pred_ww) |>
      dplyr::mutate(
        draw = `.draw`,
        name = "pred_ww",
        value = exp(value)
      ) |>
      dplyr::select(name, lab_site_index, t, value, draw) |>
      dplyr::left_join(date_df, by = "t") |>
      dplyr::left_join(lab_site_map, by = "lab_site_index") |>
      dplyr::left_join(site_pop_map, by = c("site")) |>
      dplyr::left_join(
        input_data |> select(
          date, lab_site_index,
          ww, below_LOD,
          lod_sewage, flag_as_ww_outlier
        ),
        by = c("date", "lab_site_index")
      ) |>
      dplyr::ungroup() |>
      dplyr::rename(calib_data = ww) |>
      dplyr::left_join(
        eval_data |>
          dplyr::select(date, ww, lab, site) |>
          unique(),
        by = c("date", "lab", "site")
      ) |>
      dplyr::rename(eval_data = ww) |>
      dplyr::mutate(
        forecast_date = ymd(!!forecast_date),
        model_type = !!model_type,
        scenario = !!scenario,
        site_lab_name = glue::glue("Site: {site}, Lab: {lab}"),
        location = !!location
      ) |>
      dplyr::ungroup()
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
  quantiles <- cfaforecastrenewalww::trajectories_to_quantiles(
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
        !is.na(calib_data) ~ "calibration",
        date <= forecast_date ~ "nowcast",
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
  quantiles <- cfaforecastrenewalww::trajectories_to_quantiles(
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



    cfaforecastrenewalww::create_dir(full_dir)

    readr::write_tsv(as_tibble(data_to_save),
      file = fp
    )
  }
  return(NULL)
}

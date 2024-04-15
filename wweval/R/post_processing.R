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
                                   model_type,
                                   draws,
                                   forecast_date,
                                   scenario,
                                   location,
                                   input_data,
                                   eval_data,
                                   last_hosp_data_date,
                                   ot,
                                   forecast_time = 28) {
  nowcast_time <- as.integer(ymd(forecast_date) - ymd(last_hosp_data_date))
  ht <- nowcast_time + forecast_time
  # Date spine for joining data
  date_df <- tibble::tibble(date = seq(
    from = min(input_data$date),
    to = min(input_data$date) + ot + ht,
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
      pull(pop) |>
      unique()

    draws_w_data <- draws |>
      spread_draws(pred_hosp[t]) |>
      rename(value = pred_hosp) |>
      mutate(
        draw = `.draw`,
        name = "pred_hosp"
      ) |>
      select(name, t, value, draw) |>
      left_join(date_df, by = "t") |>
      left_join(input_data |> select(-pop, -location),
        by = c("date")
      ) |>
      rename(calib_data = daily_hosp_admits) |>
      left_join(eval_data |> select(-pop, -location),
        by = c("date")
      ) |>
      rename(eval_data = daily_hosp_admits) |>
      mutate(
        forecast_date = lubridate::ymd(!!forecast_date),
        model_type = !!model_type,
        location = !!location,
        pop = !!pop,
        scenario = !!scenario
      ) |>
      ungroup()
  }

  if (model_output == "ww") {
    # Then we also want to output the wastewater predictions
    lab_site_map <- input_data |>
      select(lab_site_index, site, lab, location) |>
      distinct()
    # Get mean population in the site over the calibration period, this
    # is the same pop size we use in the model fitting
    site_pop_map <- input_data |>
      select(site, ww_pop, date) |>
      group_by(site) |>
      summarise(ww_pop = mean(ww_pop))

    draws_w_data <- draws |>
      spread_draws(pred_ww[lab_site_index, t]) |>
      rename(value = pred_ww) |>
      mutate(
        draw = `.draw`,
        name = "pred_ww",
        value = exp(value)
      ) |>
      select(name, lab_site_index, t, value, draw) |>
      left_join(date_df, by = "t") |>
      left_join(lab_site_map, by = "lab_site_index") |>
      left_join(site_pop_map, by = c("site")) |>
      left_join(
        input_data |> select(
          date, lab_site_index,
          ww, below_LOD,
          lod_sewage, flag_as_ww_outlier
        ),
        by = c("date", "lab_site_index")
      ) |>
      rename(calib_data = ww) |>
      left_join(eval_data |> select(date, ww, lab, site),
        by = c("date", "lab", "site")
      ) |>
      rename(eval_data = ww) |>
      mutate(
        forecast_date = ymd(!!forecast_date),
        model_type = !!model_type,
        scenario = !!scenario,
        site_lab_name = glue::glue("Site: {site}, Lab: {lab}"),
        location = !!location
      ) |>
      ungroup()
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

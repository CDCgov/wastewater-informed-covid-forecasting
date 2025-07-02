#' Compute the the posterior of the difference between model predictions.
#'
#' @param forecast_date forecast_date for which to fits recent data
#' trends. Data for an actual forecast must already have been produced
#' and exist in `raw_output_dir`.
#' @param location Location for which to fit trends.
#' @param scenario Wastewater data availability scenario to analyze.
#' @param raw_output_dir Directory containing raw output `.rds` files.
#' Used to obtain the admissions and wastewater data used in fitting
#' the forecasting model.
#' @return Posterior of the difference in total incidence
#' across the full nowcast/forecast period. Saves
#' the incidence differences by day and the total
#' incidence difference to disk as a side effect.
#' @export
compute_forecast_differences <- function(
  forecast_date,
  location,
  scenario,
  raw_output_dir
) {
  exists_hosp_object <- get_object_existence_checker(
    location,
    forecast_date,
    "no_wastewater",
    raw_output_dir
  )
  exists_ww_object <- get_object_existence_checker(
    location,
    forecast_date,
    scenario,
    raw_output_dir
  )

  load_object_hosp <- get_object_loader(
    location,
    forecast_date,
    "no_wastewater",
    raw_output_dir
  )
  load_object_ww <- get_object_loader(
    location,
    forecast_date,
    scenario,
    raw_output_dir
  )

  save_object <- get_object_saver(
    location,
    forecast_date,
    scenario,
    raw_output_dir
  )

  message("Loading posteriors...")

  filter_to_hosp_forecasts <- function(tbl) {
    return(dplyr::filter(
      tbl,
      .data$name == "pred_hosp",
      is.na(.data$calib_data)
      ## this is how things are filtered to the nowcast/forecast period
      ## elsewhere in the codebase
    ))
  }

  if (
    !all(c(
      exists_hosp_object("hosp_draws"),
      exists_ww_object("hosp_draws")
    ))
  ) {
    warning(glue::glue(
      "Posterior draws not found for ",
      "location {location}, forecast date ",
      "{forecast_date}, and scenario ",
      "{scenario}"
    ))

    return(NULL)
  }

  preds_hosp <- load_object_hosp("hosp_draws") |>
    filter_to_hosp_forecasts() |>
    dplyr::select("date", "draw", hosp_model_pred = "value")
  preds_ww <- load_object_ww("hosp_draws") |>
    filter_to_hosp_forecasts() |>
    dplyr::rename(ww_model_pred = "value") |>
    dplyr::select(-c("pop", "name", "model_type", "calib_data"))

  message("Joining posteriors...")
  joined_preds <- dplyr::inner_join(
    preds_hosp,
    preds_ww,
    by = c("date", "draw")
  )

  total_preds <- joined_preds |>
    dplyr::summarise(
      dplyr::across(c("hosp_model_pred", "ww_model_pred", "eval_data"), sum),
      .by = c("forecast_date", "draw")
    )

  diffs <- purrr::map(list(joined_preds, total_preds), \(df) {
    dplyr::mutate(
      df,
      log_diff_ww_hosp = log(.data$ww_model_pred) -
        log(.data$hosp_model_pred)
    )
  })

  save_object(diffs[[1]], "forecast_posterior_diffs")
  save_object(diffs[[2]], "forecast_posterior_total_diffs")

  return(diffs[[2]])
}

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
#' @return The posterior of the difference, as a table..
#' @export
compute_forecast_difference <- function(
  forecast_date,
  location,
  scenario,
  raw_output_dir
) {
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

  preds_hosp <- load_object_hosp("hosp_draws") |>
    dplyr::filter(is.na(.data$calib_data)) |>
    dplyr::select("date", "draw", hosp_model_pred = "value")
  preds_ww <- load_object_ww("hosp_draws") |>
    dplyr::filter(is.na(.data$calib_data)) |>
    dplyr::rename(ww_model_pred = "value") |>
    dplyr::select(-c("pop", "name", "model_type", "calib_data"))

  message("Joining posteriors...")
  preds <- dplyr::inner_join(
    preds_hosp,
    preds_ww,
    by = c("date", "draw")
  ) |>
    dplyr::mutate(
      log_diff_pred = log(ww_model_pred) -
        log(hosp_model_pred)
    )

  return(preds)
}

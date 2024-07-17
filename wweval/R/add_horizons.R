#' Add columns indicating the horizon to the scores
#'
#' @description
#' This function takes in a tibble of scores on the nowcasts/forecasts
#' containing the columns `date` and `forecast_date` and adds the following
#' columns: `horizon_days` (an integer) and `horizon` (a string to be used for
#' categorical grouping of horizons)
#'
#'
#' @param df A tibble containing either forecasts or scores (or both)
#' and the following required columns: `date`and `forecast_date`
#' @param max_nowcast_days An integer indicating the expected maximum number
#' of weeks where date < forecast date. If the output contains values beyond
#' this threshold, an error will be thrown which may indicate that the scores
#' contain scores for calibrated data.
#'
#' @return a tibble containing the same columns as `df` plus
#' `horizon_days` and `horizon`
#' @export
add_horizons <- function(df,
                         max_nowcast_days) {
  df_w_horizons <- df |>
    dplyr::mutate(
      horizon_days = as.numeric(date - forecast_date)
    ) |>
    dplyr::mutate(
      horizon = dplyr::case_when(
        horizon_days <= -!!max_nowcast_days ~ "calibration",
        horizon_days > -max_nowcast_days & horizon_days <= 0 ~ "nowcast",
        horizon_days > 0 & horizon_days <= 7 ~ "1 wk",
        horizon_days > 7 & horizon_days <= 14 ~ "2 wks",
        horizon_days > 14 & horizon_days <= 21 ~ "3 wks",
        horizon_days > 21 & horizon_days <= 28 ~ "4 wks",
        TRUE ~ NA_character_
      )
    )

  return(df_w_horizons)
}

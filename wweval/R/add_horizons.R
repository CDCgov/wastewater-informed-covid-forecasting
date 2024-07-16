#' Add columns indicating the horizon to the quantiles
#'
#' @description
#' This function takes in a tibble of quantiles containing the columns
#' `date`, `forecast_date` and `period` and adds the following columns:
#' `horizon_days` (an integer) and `horizon` (a string to be used for
#' categorical grouping of horizons)
#'
#'
#' @param quantiles A tibble containing the quantiled values and the following
#' required columns: `date`, `forecast_date`, and `period`
#'
#' @return a tibble containing the same columns as `quantiles` plus
#' `horizon_days` and `horizon`
#' @export
add_horizons_to_quantiles <- function(quantiles) {
  quantiles_w_horizons <- quantiles |>
    dplyr::mutate(
      horizon_days = as.numeric(date - forecast_date)
    ) |>
    dplyr::mutate(
      horizon = dplyr::case_when(
        period == "calibration" ~ "calibration",
        period != "calibration" & horizon_days < 0 ~ "nowcast",
        horizon_days > 0 & horizon_days <= 7 ~ "1 wk",
        horizon_days > 7 & horizon_days <= 14 ~ "2 wks",
        horizon_days > 14 & horizon_days <= 21 ~ "3 wks",
        horizon_days > 21 & horizon_days <= 28 ~ "4 wks",
        TRUE ~ NA_character_
      )
    )
  return(quantiles_w_horizons)
}

#' Add columns indicating the horizon to the scores
#'
#' @description
#' This function takes in a tibble of scores on the nowcasts/forecasts
#' containing the columns `date` and `forecast_date` and adds the following
#' columns: `horizon_days` (an integer) and `horizon` (a string to be used for
#' categorical grouping of horizons)
#'
#'
#' @param scores A tibble containing the quantiled values and the following
#' required columns: `date`and `forecast_date`
#' @param max_days_nowcast An integer indicating the expected maximum number
#' of weeks where date < forecast date. If the output contains values beyond
#' this threshold, an error will be thrown which may indicate that the scores
#' contain scores for calibrated data.
#'
#' @return a tibble containing the same columns as `scores` plus
#' `horizon_days` and `horizon`
#' @exportmax_days_nowscast
add_horizons_to_scores <- function(scores,
                                   max_days_nowcast = 14) {
  scores_w_horizons <- scores |>
    dplyr::mutate(
      horizon_days = as.numeric(date - forecast_date)
    ) |>
    dplyr::mutate(
      horizon = dplyr::case_when(
        horizon_days <= -max_days_nowcast ~ "calibration",
        horizon_days > -max_days_nowcast & horizon_days <= 0 ~ "nowcast",
        horizon_days > 0 & horizon_days <= 7 ~ "1 wk",
        horizon_days > 7 & horizon_days <= 14 ~ "2 wks",
        horizon_days > 14 & horizon_days <= 21 ~ "3 wks",
        horizon_days > 21 & horizon_days <= 28 ~ "4 wks",
        TRUE ~ NA_character_
      )
    )

  return(scores_w_horizons)
}

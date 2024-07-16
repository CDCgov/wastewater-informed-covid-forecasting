#' Add columns indicating the horizon to the quantiles
#'
#' @description
#' This function takes in a tibble of quantiles containing the columns
#' `date`, `forecast_date` and `period` and adds the following columns:
#' `horizon_weeks` (an integer) and `horizon` (a string to be used for
#' categorical grouping of horizons)
#'
#'
#' @param quantiles A tibble containing the quantiled values and the following
#' required columns: `date`, `forecast_date`, and `period`
#'
#' @return a tibble containing the same columns as `quantiles` plus
#' `horizon_weeks` and `horizon`
#' @export
add_horizons_to_quantiles <- function(quantiles) {
  quantiles_w_horizons <- quantiles |>
    dplyr::mutate(
      # As written, this generates negative integer weeks for days before the
      # forecast date, and +1 for days after the forecast date, and a 0
      # on the forecast date
      horizon_weeks = ifelse(date > forecast_date,
        ceiling(as.numeric(date - forecast_date) / 7),
        -ceiling(abs(as.numeric(date - forecast_date) / 7))
      )
    ) |>
    dplyr::mutate(
      horizon = dplyr::case_when(
        period == "calibration" ~ "calibration",
        period != "calibration" & horizon_weeks < 0 ~ "nowcast",
        horizon_weeks == 1 ~ "1 wk",
        horizon_weeks == 2 ~ "2 wks",
        horizon_weeks == 3 ~ "3 wks",
        horizon_weeks == 4 ~ "4 wks",
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
#' columns: `horizon_weeks` (an integer) and `horizon` (a string to be used for
#' categorical grouping of horizons)
#'
#'
#' @param scores A tibble containing the quantiled values and the following
#' required columns: `date`and `forecast_date`
#' @param max_weeks_nowcast An integer indicating the expected maximum number
#' of weeks where date < forecast date. If the output contains values beyond
#' this threshold, an error will be thrown which may indicate that the scores
#' contain scores for calibrated data.
#'
#' @return a tibble containing the same columns as `scores` plus
#' `horizon_weeks` and `horizon`
#' @export
add_horizons_to_scores <- function(scores,
                                   max_weeks_nowcast = 2) {
  scores_w_horizons <- scores |>
    dplyr::mutate(
      # As written, this generates negative integer weeks for days before the
      # forecast date, and +1 for days after the forecast date, and a 0
      # on the forecast date
      horizon_weeks = ifelse(date > forecast_date,
        ceiling(as.numeric(date - forecast_date) / 7),
        -ceiling(abs(as.numeric(date - forecast_date) / 7))
      )
    ) |>
    dplyr::mutate(
      horizon = dplyr::case_when(
        horizon_weeks < 0 ~ "nowcast",
        horizon_weeks == 1 ~ "1 wk",
        horizon_weeks == 2 ~ "2 wks",
        horizon_weeks == 3 ~ "3 wks",
        horizon_weeks == 4 ~ "4 wks",
        TRUE ~ NA_character_
      )
    )

  return(scores_w_horizons)
}

#' Add columns indicating the horizon to the scores
#'
#' @description
#' This function takes in a tibble of scores on the nowcasts/forecasts
#' containing the columns `date`, `forecast_date`, and `last_hosp_data_date`,
#' and adds the following columns: `horizon_days` (an integer) and
#'  `horizon` (a string to be used for categorical grouping of horizons)
#'
#'
#' @param df A tibble containing either forecasts or scores (or both)
#' and the following required columns: `date`,`forecast_date`,
#' `last_hosp_data_date`
#'
#' @return a tibble containing the same columns as `df` plus
#' `horizon_days` and `horizon`
#' @export
add_horizons <- function(df) {
  df_w_horizons <- df |>
    dplyr::mutate(
      horizon_days = as.numeric(date - forecast_date)
    ) |>
    dplyr::mutate(
      horizon = dplyr::case_when(
        date <= last_hosp_data_date & horizon_days <= 0 ~ "calibration",
        date > last_hosp_data_date & horizon_days <= 0 ~ "nowcast",
        horizon_days > 0 & horizon_days <= 7 ~ "1 wk",
        horizon_days > 7 & horizon_days <= 14 ~ "2 wks",
        horizon_days > 14 & horizon_days <= 21 ~ "3 wks",
        horizon_days > 21 & horizon_days <= 28 ~ "4 wks",
        TRUE ~ NA_character_
      )
    )

  return(df_w_horizons)
}

#' Get a map of the location, forecast date, and last hospital admissions data
#' date
#'
#' @param df A tibble containing the following columns: `forecast_date`,
#' `location`,`date`, `calib_data`.
#' `calib_data` should be `NA` for any dates for which there was
#'  not hospital admissions data to fit to.
#'
#' @return A tibble with that maps the unique combinations of `location` and
#' `forecast` date to the last hospital admissions data date
#' `last_hosp_data_date`
#' @export
get_last_hosp_data_date_map <- function(df) {
  map <- df |>
    dplyr::group_by(forecast_date, location) |>
    dplyr::filter(!is.na(calib_data)) |>
    dplyr::summarise(
      last_hosp_data_date = max(date)
    ) |>
    dplyr::ungroup()

  return(map)
}

#' Get the order of the categorical horizons for plotting
#'
#' @param df a dataframe containing the column name `horizon` which will
#' contain character strings indicating the horizon (by week, nowcast period,
#' or overall) of the score/quantile/sample.
#'
#' @return a dataframe containing the same columns as `df` but with the
#' addition of `fig_order` and reordered in terms of `fig_order` for plotting.
#' @export
order_horizons <- function(df) {
  horizon_order <- c(
    "overall",
    "calibration",
    "nowcast",
    "1 wk",
    "2 wks",
    "3 wks",
    "4 wks"
  )

  df_w_order <- df |>
    dplyr::mutate(
      horizon = factor(horizon, ordered = TRUE, levels = horizon_order)
    )

  return(df_w_order)
}

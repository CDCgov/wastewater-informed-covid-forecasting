#' Add columns indicating the horizon to forecasts or scores
#'
#' @description
#' This function takes in a tibble of forecasts or scores and
#' adds the following columns: `horizon_days` (an integer) and
#'  `horizon` (a string to be used for categorical grouping of horizons)
#'
#' @param df A tibble containing either forecasts or scores (or both)
#' and columns corresponding to the target end date, the forecast_date,
#' and the last data date (which defines the boundary between the
#' calibration period and the nowcast period.
#' @param target_end_date_col Name of the column containing target end
#' dates. Default `"target_end_date"`.
#' @param forecast_date_col Name of the column containing forecast dates.
#' Default `"forecast_date"`.
#' @param last_data_date_col column containing last data dates. Default
#' `"last_hosp_data_date"`
#'
#' @return a tibble containing the same columns as `df` plus
#' `horizon_days` and `horizon`
#' @export
add_horizons <- function(
  df,
  target_end_date_col = "target_end_date",
  forecast_date_col = "forecast_date",
  last_data_date_col = "last_hosp_data_date"
) {
  df_w_horizons <- df |>
    dplyr::mutate(
      horizon_days = as.integer(as.numeric(
        as.Date(.data[[target_end_date_col]]) -
          as.Date(.data[[forecast_date_col]])
      ))
    ) |>
    dplyr::mutate(
      horizon = dplyr::case_when(
        .data[[target_end_date_col]] <= .data[[last_data_date_col]] &
          .data$horizon_days <= 0 ~
          "calibration",
        .data[[target_end_date_col]] > .data[[last_data_date_col]] &
          .data$horizon_days <= 0 ~
          "nowcast",
        .data$horizon_days > 0 & .data$horizon_days <= 7 ~ "1 wk",
        .data$horizon_days > 7 & .data$horizon_days <= 14 ~ "2 wks",
        .data$horizon_days > 14 & .data$horizon_days <= 21 ~ "3 wks",
        .data$horizon_days > 21 & .data$horizon_days <= 28 ~ "4 wks",
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
    dplyr::group_by(.data$forecast_date, .data$location) |>
    dplyr::filter(!is.na(.data$calib_data)) |>
    dplyr::summarise(
      last_hosp_data_date = max(.data$date)
    ) |>
    dplyr::ungroup()

  return(map)
}

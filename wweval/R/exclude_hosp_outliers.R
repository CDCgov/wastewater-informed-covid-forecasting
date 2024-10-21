#' Exclude hospital outliers
#'
#' @param raw_input_hosp_data A dataframe containing the location, the date of
#' admission, and the number of hospital admissions, in that location
#' @param forecast_date The forecast date as a character string
#' @param table_of_exclusions A table containing the combinations of
#' locations, forecast dates, and dates to exclude. These will be treated
#' as missing by the model
#' @param col_name_dates_to_exclude The name of the column in the table of
#' exclusions that corresponds to the date of hospital admissions to exclude,
#' default is `dates_to_exclude`
#' @return A dataframe in the same format as the `raw_input_hosp_data` but
#' with the location-forecast-date-date admissions rows excluded if indicated
#' in the `table_of_exclusions`
#' @export
#'
exclude_hosp_outliers <- function(raw_input_hosp_data,
                                  forecast_date,
                                  table_of_exclusions,
                                  col_name_dates_to_exclude = "dates_to_exclude") {
  # Filter table of exclusions to the relevant forecast date and location

  loc <- raw_input_hosp_data |>
    dplyr::pull(location) |>
    unique()

  stopifnot("Only one location passed in" = length(loc) == 1)

  exclusions <- table_of_exclusions |>
    dplyr::filter(
      location == loc,
      forecast_date == forecast_date
    )

  if (nrow(exclusions) == 0) {
    input_hosp_data <- raw_input_hosp_data
  } else {
    dates_to_exclude <- exclusions |>
      dplyr::pull({{ col_name_dates_to_exclude }})
    input_hosp_data <- raw_input_hosp_data |>
      dplyr::filter(!date %in% c(dates_to_exclude))
  }

  return(input_hosp_data)
}

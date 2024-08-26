#' Get table of location-forecast dates with sufficient wastewater
#'
#' @description
#' This function takes in a large tibble with all the combined wastewater data
#' flags from each of the model runs, and produces a table with a row for each
#' model run indicating whether the wastewater data is sufficient, based
#' on the thresholds defined in [get_ww_data_flags()]
#'
#' @param combined_ww_data_flags A large tibble that consists of row bound
#' ww data flags for each individual run
#'
#' @return table_of_loc_dates_w_ww a tibble containing the location,
#' forecast_date, and column stating the wastewater data was sufficient for all
#' locations and forecast dates where ww data was deemed sufficient
#' @export
#'
get_table_sufficient_ww <- function(combined_ww_data_flags) {
  # Ensure all `values` are boolean
  stopifnot(
    "In diagnostic table checking for sufficent wastewater data flags, not all values are boolean" =
      is.logical(combined_ww_data_flags$value)
  )

  table_of_loc_dates_w_ww <- combined_ww_data_flags |>
    dplyr::group_by(location, forecast_date) |>
    dplyr::summarise(ww_sufficient = !any(value))

  return(table_of_loc_dates_w_ww)
}

#' Get table of location-forecast dates with sufficient wastewater
#'
#' @description
#' This function takes in a the input wastewater data from an individual
#' location, forecast date, and scenario and produces a table with metadata
#' about the quality of the wastewater data, including values of the
#' number of data points, the mean value of the wastewater concentraiton,
#' and the proportion of data below the LOD. The thresholds passed to this
#' function are used to generate the boolean flags, indicated in the `name`
#' and `value` columns.
#'
#' @param input_ww_data A tibble with the wastewater data used to fit the
#' model for a particular location, forecast date, and scenario. Required
#' columns are `ww`, `date`, `below_lod`, and `location`
#' @param forecast_date A string indicating the date of the forecast for
#' this particular fit in ISO8 format (YYYY-MM-DD)
#' @param delay_thres The maximum number of days of delay between the last
#' wastewater data point and the forecast date, before we would flag a state as
#' having insufficient wastewater data to inform a forecast. Default is 21
#' @param n_dps_thres The threshold number of data points within a single site
#' within a state before we would flag the state as having insufficient
#' wastewater data to inform a forecast. Default is 5
#' @param prop_below_lod_thres The threshold proportion of wastewater data
#' points that can be below the LOD. If greater than this proportion of points
#' are below the LOD, we flag the state as having insufficient wastewater data.
#' Default is 0.5
#' @param sd_thres The minimum standard deviation between wastewater data points
#' within a site. This is intended to catch when a site reports all the same
#' values. Default is 0.1
#' @param mean_log_ww_value_thres The minimum value of the log of the ww
#' concentration, default is -4
#'
#' @return a tibble containing the location,
#' forecast_date, and 5 rows indicating the flags for the input wastewater data
#' associated with this individual run
#' @export
#'
get_ww_data_flags <- function(input_ww_data,
                              forecast_date,
                              delay_thres = 21,
                              n_dps_thres = 5,
                              prop_below_lod_thres = 0.5,
                              sd_thres = 0.1,
                              mean_log_ww_value_thres = -4) {
  this_location <- input_ww_data |>
    dplyr::select(location) |>
    dplyr::pull(location)

  diagnostic_table <- input_ww_data |>
    dplyr::summarize(
      last_date = max(date),
      n_dps = dplyr::n(),
      prop_below_lod = sum(below_LOD == 1) / dplyr::n(),
      sd = sd(ww),
      mean_log_ww = mean(log(ww))
    ) |>
    dplyr::mutate(
      location = this_location,
      forecast_date = lubridate::ymd(!!forecast_date),
      flag_delay = as.integer(forecast_date - last_date) > !!delay_thres,
      flag_n_dps = n_dps < !!n_dps_thres,
      flag_lod = prop_below_lod > !!prop_below_lod_thres,
      flag_sd = sd < !!sd_thres,
      flag_low_val = mean_log_ww < !!mean_log_ww_value_thres
    )

  flag_table_long <- diagnostic_table |>
    dplyr::ungroup() |>
    tidyr::pivot_longer(starts_with("flag"))

  # Ensure all `values` are boolean
  stopifnot(
    "In diagnostic table checking for sufficent wastewater data flags, not all values are boolean" =
      is.logical(flag_table_long$value)
  )

  return(flag_table_long)
}

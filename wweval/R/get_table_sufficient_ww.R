#' Get table of location-forecast dates with sufficient wastewater
#'
#' @description
#' This function takes in a large dataframe containing the quantiled estimated
#' and forecasted wastewater concentrations for each site and lab in each
#' location and forecast date, joined with the observed data on each day, in
#' each site and lab, from each forecast date. We will use the data to
#' get a table of location-forecast-dates where wastewater data was considered
#' sufficient to inform a forecast, based on the critera that was used for
#' the Hub submissions.
#'
#' @param path_to_ww_vintaged_data A character string indicating the path
#' to where vintaged wastewater data are saved
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
#' @return table_of_loc_dates_w_ww a tibble containing the location,
#' forecast_date, and column stating the wastewater data was sufficient for all
#' locations and forecast dates where ww data was deemed sufficient
#' @export
#'
get_table_sufficient_ww <- function(path_to_ww_vintaged_data,
                                    delay_thres = 21,
                                    n_dps_thres = 5,
                                    prop_below_lod_thres = 0.5,
                                    sd_thres = 0.1,
                                    mean_log_ww_value_thres = -4) {
  calib_data <- readr::read_csv(path_to_ww_vintaged_data)

  diagnostic_table <- calib_data |>
    dplyr::group_by(location, forecast_date) |>
    dplyr::summarize(
      last_date = max(date),
      n_dps = dplyr::n(),
      prop_below_lod = sum(below_LOD == 1) / dplyr::n(),
      sd = sd(calib_data),
      mean_log_ww = mean(log(calib_data))
    ) |>
    dplyr::mutate(
      flag_delay = as.integer(forecast_date - last_date) > delay_thres,
      flag_n_dps = n_dps < n_dps_thres,
      flag_lod = prop_below_lod > prop_below_lod_thres,
      flag_sd = sd < sd_thres,
      flag_low_val = mean_log_ww < mean_log_ww_value_thres
    )

  flag_table_long <- diagnostic_table |>
    dplyr::ungroup() |>
    tidyr::pivot_longer(starts_with("flag"))

  # Ensure all `values` are boolean
  stopifnot(
    "In diagnostic table checking for sufficent wastewater data flags, not all values are boolean" =
      is.logical(flag_table_long$value)
  )

  table_of_loc_dates_w_ww <- flag_table_long |>
    dplyr::group_by(location, forecast_date) |>
    dplyr::summarise(ww_sufficient = !any(value))

  return(table_of_loc_dates_w_ww)
}


#' Save only the wastewater data from the output quantiles
#'
#' @param path_to_ww_quantiles Path to where the ww quantiles are saved
#' @param ww_data_dir directory where wastewater data is to be saved
#'
#' @return path to only the data
#' @export
save_only_ww_data <- function(path_to_ww_quantiles,
                              ww_data_dir) {
  calib_data <- arrow::read_parquet(path_to_ww_quantiles) |>
    dplyr::distinct(
      location, site, lab, lab_site_index,
      date, forecast_date, below_LOD, calib_data
    ) |>
    dplyr::filter(!is.na(calib_data))

  path_to_data <- file.path(ww_data_dir, "ww_vintaged_data.csv")

  write.csv(calib_data, path_to_data, row.names = FALSE)
  return(path_to_data)
}



#' Either get the path to the ww quantiles or regenerate them
#'
#' @param scenarios vector of character strings indicating scenarios to load in
#' @param forecast_dates vector of character strings indicating forecast dates
#' to load in
#' @param locations vector of character strings indicating states to load in
#' @param eval_output_subdir string indicating the subdir to find the things
#' to load in if needed
#' @param ww_quantiles_path string indicating the full path to where the
#' ww_quantiles either are saved or should be saved
#' @param rerun_ww_postprocess Boolean indicating whether or not to force
#' rerun the ww quantiles. If FALSE, then grab the quantiles in the path
#' @param model_type string indicating the model type, default is `ww`
#' @param output_type string indicating the output type, default is `ww`
#'
#' @return a character string indicating the path to the wastewater quantiles
#' @export
get_path_to_ww_quants_or_rerun <- function(scenarios,
                                           forecast_dates,
                                           locations,
                                           eval_output_subdir,
                                           ww_quantiles_path,
                                           rerun_ww_postprocess,
                                           model_type = "ww",
                                           output_type = "ww_quantiles") {
  if (!file.exists(file.path(ww_quantiles_path)) || isTRUE(rerun_ww_postprocess)) {
    ww_quantiles <-
      combine_outputs(
        output_type,
        scenarios,
        forecast_dates,
        locations,
        eval_output_subdir,
        model_type
      )

    arrow::write_parquet(
      x = ww_quantiles,
      sink = ww_quantiles_path
    )
  } else {
    ww_quantiles_path
  }

  return(ww_quantiles_path)
}

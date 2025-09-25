#' Get table of location-forecast dates with available and
#' sufficient wastewater
#'
#' @description
#' This function takes in a large tibble with all the combined wastewater
#' data flags from each of the model runs, and produces a table with a
#' row for each model run indicating the wastewater data status for that
#' run.
#'
#' @param ww_quality_table Data frame of flags for each run,
#' as the row-bound output of one or more calls to
#' [compute_ww_data_quality_table()].
#'
#' @return a summarized table with locations, forecast dates, a
#' `wwsufficient` boolean column indicating whether there is valid
#' wastewater data and a `status` column indicating the reason
#' for data (in)validity.
#' #' @export
summarize_ww_data_quality <- function(ww_quality_table) {
  insufficiency_flags <- c(
    "flag_delay",
    "flag_n_dps",
    "flag_lod",
    "flag_sd",
    "flag_no_data"
  )
  checkmate::check_data_frame(
                 ww_quality_table[insufficiency_flags],
                 types = "logical")
  tbl <- quality_table |>
    dplyr::group_by(.data$location, .data$forecast_date) |>
    dplyr::summarise(
      ww_sufficient = !any(dplyr::across(insufficiency_flags)),
      ww_present = !.data$flag_no_data
    ) |>
    dplyr::mutate(
      status = dplyr::case_when(
        ww_sufficient ~ "sufficient_ww",
        !ww_sufficient & ww_present ~ "insufficient_ww",
        !ww_present ~ "absent_ww",
        TRUE ~ NA
      )
    )
  return(tbl)
}

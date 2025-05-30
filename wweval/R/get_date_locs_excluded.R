#' Get forecast date locations that we excluded in the model run
#'
#' @param dates vector of character strings indicating the forecast dates that
#' we want to pull from our repository to check for exclusions
#'
#' @return a table of locations and forecast dates to exclude because we
#' excluded them in real-time
#' @export
#'
get_date_locs_excluded <- function(dates) {
  table_of_exclusions <- tibble::tibble()
  for (i in seq_along(dates)) {
    date <- dates[i]

    yaml_path <- file.path("output", "forecasts", date, "metadata.yaml")

    metadata <- yaml::read_yaml(yaml_path)

    locs_to_exclude <- unlist(
      metadata$`States we chose to use hospital admissions only model on`
    )

    this_dates_exclusions <- tibble::tibble(
      location = locs_to_exclude,
      forecast_date = rep(date, length(locs_to_exclude))
    )

    table_of_exclusions <- rbind(table_of_exclusions, this_dates_exclusions)
  }
  return(table_of_exclusions)
}

get_date_locs_hosp_used <- function(dates) {
  table_of_replacements <- tibble::tibble()
  for (i in seq_along(dates)) {
    date <- dates[i]

    yaml_path <- file.path("output", "forecasts", date, "metadata.yaml")

    metadata <- yaml::read_yaml(yaml_path)

    locs_to_exclude <- unlist(
      metadata$`States we chose to use hospital admissions only model on`
    )
    locs_wout_ww <- unlist(metadata$`States without wastewaster data`)
    this_dates_replacements <- tibble::tibble(
      location = c(locs_to_exclude, locs_wout_ww),
      forecast_date = rep(date, length(c(locs_to_exclude, locs_wout_ww)))
    )

    table_of_replacements <- rbind(
      table_of_replacements,
      this_dates_replacements
    )
  }
  return(table_of_replacements)
}

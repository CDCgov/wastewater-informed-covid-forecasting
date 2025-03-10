#' Pull and save the current state of NHSN data on HealthData.gov
#'
#' Pull data using [pull_nhsn()], format it for pipeline ingestion,
#' and write it to disk as a date-stamped `.csv` file.
#'
#' @param location_data_path Path to a csv file containing data
#' about constituent locations, with columns `"abbreviation"` and
#' `"population"`
#' @param output_dir Directory in which to save the output.
#' @param force Overwrite an existing `.csv` with the same date-stamp?
#' Boolean, default `FALSE`.
#' @return NULL, saving the data to disk as a side effect.
#' @examples
#'
#' #'
#' \dontrun{
#' # this will write a file named `<today's date>.csv` to a directory named
#' # `input/hosp_data/vintage_datasets`.
#' pull_and_write_hosp_data(
#'   file.path("input", "locations.csv"),
#'   file.path("input", "hosp_data", "vintage_datasets")
#' )
#' }
#'
#' @export
pull_and_write_hosp_data <- function(location_data_path,
                                     output_dir,
                                     force = FALSE) {
  pull_date <- lubridate::today()

  location_data <- readr::read_csv(location_data_path) |>
    dplyr::select(
      state = "abbreviation",
      pop = "population"
    )

  raw_data <- wweval::pull_nhsn(
    start_date = "2023-01-01",
    columns = c(
      "previous_day_admission_adult_covid_confirmed",
      "previous_day_admission_pediatric_covid_confirmed"
    )
  )

  data <- raw_data |>
    dplyr::mutate(
      daily_hosp_admits = as.numeric(
        .data$previous_day_admission_adult_covid_confirmed
      ) +
        as.numeric(
          .data$previous_day_admission_pediatric_covid_confirmed
        ),
      date = as.Date(.data$date) - lubridate::ddays(1)
    ) |>
    ## convert from previous day to date-of-event indexing,
    ## following covidcast/epidatr
    dplyr::inner_join(location_data, by = "state") |>
    dplyr::select(
      "date",
      ABBR = "state",
      "daily_hosp_admits",
      "pop"
    )

  output_path <- fs::path(output_dir, pull_date, ext = "csv")

  if (fs::file_exists(output_path) && !force) {
    cli::cli_abort(c(
      "File {output_path} already exists. Run with force = TRUE",
      "to overwrite it"
    ))
  } else {
    cli::cli_inform("Saving output to {output_path}...")
    readr::write_csv(data, output_path)
  }
}

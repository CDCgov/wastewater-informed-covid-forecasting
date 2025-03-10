library(argparser)
pull_and_write <- function(location_data_path,
                           output_dir,
                           force = FALSE) {
  pull_date <- lubridate::today()

  location_data <- readr::read_csv(location_data_path) |>
    dplyr::select(
      state = abbreviation,
      pop = population
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
      date,
      ABBR = state,
      daily_hosp_admits,
      pop
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


p <- arg_parser("Pull NHSN data and save it to disk as a date-stamped csv.") |>
  add_argument(
    "location_data_path",
    help = "Path to a csv file containing location data."
  ) |>
  add_argument(
    "output_dir",
    help = "Directory in which to save the pulled data."
  ) |>
  add_argument(
    "--force",
    help = paste0(
      "If provided, overwrite existing data with the same datestamp, if any"
    ),
    flag = TRUE
  )


argv <- parse_args(p)
pull_and_write(
  location_data_path = argv$location_data_path,
  output_dir = argv$output_dir,
  force = argv$force
)

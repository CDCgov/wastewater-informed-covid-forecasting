library(argparser)

compute_diff <- function(
  forecast_date,
  location,
  scenario,
  raw_output_dir,
  processed_output_dir,
  log_diff_offset
) {
  wweval::compute_forecast_differences(
    forecast_date = forecast_date,
    location = location,
    scenario = scenario,
    raw_output_dir = raw_output_dir,
    log_diff_offset = log_diff_offset
  )
}

parsed <- arg_parser(
  paste0(
    "Compute diffs for a single forecast problem between a target scenario ",
    "and a baseline scenario (no wastewater)"
  )
) |>
  add_argument(
    "forecast_date",
    help = "As-of date for the forecast."
  ) |>
  add_argument(
    "location",
    help = "Location for the forecast."
  ) |>
  add_argument(
    "scenario",
    help = "Wastewater data availability scenario to analyze."
  ) |>
  add_argument(
    "raw_output_dir",
    help = paste0(
      "Path to a directory in which to save ",
      "raw output."
    )
  ) |>
  add_argument(
    "processed_output_dir",
    help = paste0(
      "Path to a directory in which to save ",
      "processed output."
    )
  ) |>
  add_argument(
    "log_diff_offset",
    help = "Offset for computing log forecast differences",
    type = "numeric"
  ) |>
  parse_args()


message(glue::glue(
  "Starting a compute diff task for location {parsed$location} ",
  "and forecast date {parsed$as_of_date}"
))

compute_diff(
  forecast_date = parsed$forecast_date,
  location = parsed$location,
  scenario = parsed$scenario,
  raw_output_dir = parsed$raw_output_dir,
  processed_output_dir = parsed$processed_output_dir,
  log_diff_offset = parsed$log_diff_offset
)

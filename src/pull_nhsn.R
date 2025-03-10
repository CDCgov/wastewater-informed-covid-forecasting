library(argparser)

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
wweval::pull_and_write_hosp_data(
  location_data_path = argv$location_data_path,
  output_dir = argv$output_dir,
  force = argv$force
)

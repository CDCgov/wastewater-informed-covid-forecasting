# Load in the raw table from NNH
raw_table <- arrow::read_parquet(file.path(
  "input",
  "hosp_data", "exclusions.parquet"
))

table <- raw_table |>
  dplyr::filter(
    pathogen == "COVID-19"
  ) |>
  dplyr::select(
    state_abb, report_date,
    reference_date
  ) |>
  dplyr::rename(
    location = state_abb,
    date = reference_date
  ) |>
  dplyr::select(
    location, report_date,
    date
  )

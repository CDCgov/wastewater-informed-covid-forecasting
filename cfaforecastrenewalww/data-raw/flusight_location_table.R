nation <- tibble::tibble(
  location_code = "US",
  short_name = "US",
  long_name = "United States"
)

states <- readr::read_delim(
  "https://www2.census.gov/geo/docs/reference/state.txt",
  delim = "|"
) |>
  dplyr::select(
    location_code = STATE,
    short_name = STUSAB,
    long_name = STATE_NAME
  )

flusight_location_table <- dplyr::bind_rows(
  nation,
  states
)

usethis::use_data(flusight_location_table,
  overwrite = TRUE,
  ascii = TRUE
)

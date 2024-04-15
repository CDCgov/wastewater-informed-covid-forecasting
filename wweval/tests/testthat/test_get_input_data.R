test_that("Test data_of_ww_data returns the correct date to pull the wastewater data", {
  create_fake_ww_files <- function(temp_dir, dates) {
    file_paths <- file.path(temp_dir, paste0(dates, ".csv"))
    file.create(file_paths)
  }
  # Create fake wastewater data files for specific dates
  sample_dates <- c("2024-03-16", "2024-03-17", "2024-03-18")
  temp_dir <- tempdir()
  create_fake_ww_files(temp_dir, sample_dates)


  # Test case where ww_data_mapping is NULL
  forecast_date <- "2024-03-18"
  expect_equal(
    date_of_ww_data(forecast_date, NULL, temp_dir),
    "2024-03-17" # Most recent date
  )

  # Test case where ww_data_mapping has a specific rule (Monday: Monday, Wednesday: Monday)
  forecast_date <- "2024-03-20" # Wednesday
  expect_equal(
    date_of_ww_data(forecast_date, "Monday: Monday, Wednesday: Monday", temp_dir),
    "2024-03-18" # Previous Monday
  )

  forecast_date <- "2024-03-18" # Monday
  expect_equal(
    date_of_ww_data(forecast_date, "Monday: Monday, Wednesday: Monday", temp_dir),
    "2024-03-18" # Same day (Monday)
  )

  # Test case where ww_data_mapping is something else and should return NA
  forecast_date <- "2024-03-18"
  expect_error(
    date_of_ww_data(forecast_date, "Some other mapping", temp_dir),
    "Need to write case to specify which wastewater data to pull"
  )

  # Test case where forecast date is not a Monday or a Wednesday
  forecast_date <- "2024-03-19"
  expect_error(
    date_of_ww_data(forecast_date, "Monday: Monday, Wednesday: Monday", temp_dir),
    "Forecast date is not a Monday or Wednesday"
  )
})


test_that("Last hospital admissions data is returned properly", {
  fake_df <- data.frame(
    date = seq(
      from = ymd("2024-03-01"),
      to = ymd("2024-03-10"),
      by = "days"
    ),
    daily_hosp_admits = sample.int(10,
      size = 10,
      replace = TRUE
    )
  )
  returned_last_hosp_data_date <- get_last_hosp_data_date(fake_df)
  # Test the fake df returns what we'd expect
  testthat::expect_equal(
    returned_last_hosp_data_date,
    ymd("2024-03-10")
  )
})


test_that("clean_ww_data correctly cleans and renames columns", {
  fake_nwss_subset <- tibble::tibble(
    sample_collect_date = as.Date(c("2021-01-01", "2021-01-02")),
    pcr_target_avg_conc = c(100, 200),
    population_served = c(50000, 60000),
    wwtp_jurisdiction = c("ak", "al"),
    wwtp_name = c(132, 142),
    lab_id = c(5, 5),
    lab_wwtp_unique_id = c(1, 2),
    below_LOD = c(FALSE, FALSE),
    lod_sewage = c(10, 20)
  )

  # Expected output after cleaning
  expected_nwss_output <- tibble::tibble(
    date = as.Date(c("2021-01-01", "2021-01-02")),
    location = c("AK", "AL"),
    ww = c(100, 200),
    site = c(132, 142),
    lab = c(5, 5),
    lab_wwtp_unique_id = c(1, 2),
    ww_pop = c(50000, 60000),
    below_LOD = c(FALSE, FALSE),
    lod_sewage = c(10, 20)
  )
  # Apply the cleaning function to our sample data
  cleaned_data <- clean_ww_data(fake_nwss_subset)

  # Check if the cleaned data matches our expected output
  expect_equal(cleaned_data, expected_nwss_output)

  # Check if all expected columns are present
  expect_true(all(names(expected_nwss_output) %in% names(cleaned_data)))

  # Check if 'location' and 'site' columns are correctly transformed to uppercase
  expect_true(all(toupper(fake_nwss_subset$wwtp_jurisdiction) == cleaned_data$location))
  expect_true(all(cleaned_data$site == fake_nwss_subset$wwtp_name))

  # Check if the date column is renamed correctly
  expect_equal(cleaned_data$date, fake_nwss_subset$sample_collect_date)

  # Check if the ww (wastewater) column is renamed correctly
  expect_equal(cleaned_data$ww, fake_nwss_subset$pcr_target_avg_conc)

  # Check if the ww_pop (population served) column is renamed correctly
  expect_equal(cleaned_data$ww_pop, fake_nwss_subset$population_served)

  # Check for correct renaming of lab_id to lab
  expect_equal(cleaned_data$lab, fake_nwss_subset$lab_id)

  # Ensure no extra columns are present in the cleaned data
  expected_colnames <- c(
    "date", "location", "ww", "site", "lab",
    "lab_wwtp_unique_id", "ww_pop", "below_LOD", "lod_sewage"
  )
  expect_equal(sort(names(cleaned_data)), sort(expected_colnames))
})

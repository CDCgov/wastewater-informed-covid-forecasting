test_that("Make sure we can read hospitalization data from NHSN API.", {
  # Cannot run test without internet connection
  testthat::skip_if_offline()
  skip_if_missing_secrets(c(
    "NHSN_API_KEY_ID",
    "NHSN_API_KEY_SECRET"
  ))

  tf <- tempfile()

  testthat::expect_no_error(
    !!{
      hosp_df <- get_state_level_hosp_data(
        hosp_data_source = "NHSN",
        forecast_date = lubridate::today(),
        hosp_data_dir = tf,
        population_data_path =
          system.file("testdata", "locations.csv", package = "cfaforecastrenewalww")
      )
    }
  )

  testthat::expect_setequal(
    names(hosp_df),
    c("date", "ABBR", "daily_hosp_admits", "pop")
  )

  out_csv <- file.path(
    tf, glue::glue(as.character(lubridate::today()), ".csv")
  )

  testthat::expect_true(
    file.exists(out_csv)
  )

  # No need to see "true" printed to screen
  true <- file.remove(out_csv)
})

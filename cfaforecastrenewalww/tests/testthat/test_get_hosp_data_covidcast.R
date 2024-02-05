test_that("Make sure we can read hospitalization data from covidcast API.", {
  # Cannot run test without internet connection
  testthat::skip_if_offline()

  skip_if_missing_secrets(c("covidcast_api_key"))

  tf <- tempfile()

  testthat::expect_no_error(
    !!{
      hosp_df <- get_state_level_hosp_data(
        hosp_data_source = "covidcast",
        hosp_data_dir = tf
      )
    }
  )

  testthat::expect_setequal(
    names(hosp_df),
    c("date", "ABBR", "daily_hosp_admits", "pop")
  )
})

test_that("Make sure we can read the NWSS data in, if we have what we need.", {
  # Cannot run test without internet connection
  testthat::skip_if_offline()

  skip_if_missing_secrets(c("nwss_data_token", "data_rid"))

  tf <- tempfile()

  testthat::expect_no_error(
    !!{
      ww_fp <- save_timestamped_nwss_data(tf)
    }
  )

  testthat::expect_true(
    file.exists(ww_fp)
  )

  file.remove(ww_fp)
})

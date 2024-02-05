test_that("Make sure we can find and load files we need for other tests.", {
  # Compiled model object should exist in the workspace, with functions exposed
  testthat::expect_true(
    exists("compiled_site_inf_model")
  )

  testthat::expect_true(
    "CmdStanModel" %in% class(compiled_site_inf_model)
  )

  testthat::expect_no_error(
    compiled_site_inf_model$functions$convert_to_logmean(1.0, 1.0)
  )

  fake_hosp_path <- system.file(
    "testdata",
    "2023-11-06.csv",
    package = "cfaforecastrenewalww"
  )
  testthat::expect_true(file.exists(fake_hosp_path))
  quiet({
    hosp_df <- get_state_level_hosp_data(
      hosp_data_source = "NHSN",
      forecast_date = "2023-11-06",
      hosp_data_dir = system.file(
        "testdata",
        package = "cfaforecastrenewalww"
      ),
      population_data_path = system.file(
        "testdata", "locations.csv",
        package = "cfaforecastrenewalww"
      ),
      pull_from_local = TRUE
    )
  })
})

test_that("Make sure we can find and load files we need for other tests.", {
  model <- compiled_site_inf_model

  shedding_pdf <- model$functions$get_vl_trajectory(
    tpeak = 5,
    viral_peak = 5,
    duration_shedding = 17,
    n = 100
  )

  testthat::expect_equal(sum(shedding_pdf), 1.0)
})

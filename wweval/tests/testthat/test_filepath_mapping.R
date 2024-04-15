test_that("get_model_path returns correct paths", {
  # Assume stan_models_dir is a directory containing your .stan files
  stan_models_dir <- tempdir()

  # Create fake .stan files for testing purposes
  file.create(file.path(stan_models_dir, "renewal_ww_hosp_site_level_inf_dynamics.stan"))
  file.create(file.path(stan_models_dir, "renewal_ww_hosp.stan"))


  # Test for 'ww' model type
  expect_equal(
    get_model_path("ww", stan_models_dir),
    file.path(stan_models_dir, "renewal_ww_hosp_site_level_inf_dynamics.stan")
  )

  # Test for 'hosp' model type
  expect_equal(
    get_model_path("hosp", stan_models_dir),
    file.path(stan_models_dir, "renewal_ww_hosp.stan")
  )
})

test_that("get_model_path throws an error for invalid model types", {
  # Test for invalid model type
  expect_error(
    get_model_path("invalid_type", stan_models_dir),
    "Model type is not specified properly"
  )

  # Test for NULL (missing) model type
  expect_error(
    get_model_path(NULL, stan_models_dir),
    "Model type is empty"
  )
})

mock_priors <- list(
  log10_g_prior_mean = 20,
  log10_g_prior_sd = 50,
  infection_feedback_prior_logmean = -1,
  infection_feedback_prior_logsd = 2.5,
  sd_log_sigma_ww_site_prior_mode = 0.5,
  sd_log_sigma_ww_site_prior_sd = 0.75,
  ww_site_mod_sd_sd = 100.1,
  eta_sd_mean = 52.3,
  eta_sd_sd = 0.01
)

test_that("get_parameterized_prior_dist returns expected distributions, parameterized properly", {
  expect_identical(
    get_parameterized_prior_dist("log10_g", mock_priors),
    distributional::dist_normal(20, 50)
  )
  expect_identical(
    get_parameterized_prior_dist("infection_feedback", mock_priors),
    distributional::dist_lognormal(-1, 2.5)
  )
  expect_identical(
    get_parameterized_prior_dist("sd_log_sigma_ww_site", mock_priors),
    distributional::dist_normal(0.5, 0.75) |>
      distributional::dist_truncated(lower = 0)
  )
  expect_identical(
    get_parameterized_prior_dist("ww_site_mod_sd", mock_priors),
    distributional::dist_normal(0, 100.1) |>
      distributional::dist_truncated(lower = 0)
  )
  expect_identical(
    get_parameterized_prior_dist("eta_sd", mock_priors),
    distributional::dist_normal(52.3, 0.01) |>
      distributional::dist_truncated(lower = 0)
  )
})

test_that("get_parameterized_prior_dist errors informatively on an unknown distribution name", {
  expect_error(
    get_parameterized_prior_dist("autoreg_rt_subpop", mock_priors),
    "parameter_name"
  )
})

test_that("get_parameterized_prior_dist errors informatively when a needed parameter is missing from the prior param list", {
  expect_error(
    get_parameterized_prior_dist("log10_g", mock_priors[-1]),
    "log10_g_prior_mean"
  )
})

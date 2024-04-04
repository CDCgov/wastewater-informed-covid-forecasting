test_that("Test AR(1) function in stan.", {
  model <- compiled_site_inf_model

  withr::with_seed(42, {
    z <- rnorm(10)

    stan_ar <- model$functions$ar1(
      mu = rep(0, length(z)),
      ac = 0.73,
      sd = 1.26,
      z = z,
      is_stat = TRUE
    )
    r_ar <- ar1_from_z_scores(z, 1.26, 0.73, TRUE)

    testthat::expect_equal(
      stan_ar,
      r_ar
    )

    stan_ar_nonstat <- model$functions$ar1(
      mu = rep(0, length(z)),
      ac = 0.73,
      sd = 1.26,
      z = z,
      is_stat = FALSE
    )

    testthat::expect_true(
      all(abs(stan_ar - stan_ar_nonstat) > testthat::testthat_tolerance())
    )

    r_ar_nonstat <- ar1_from_z_scores(z, 1.26, 0.73, FALSE)

    testthat::expect_equal(
      stan_ar_nonstat,
      r_ar_nonstat
    )
  })
})

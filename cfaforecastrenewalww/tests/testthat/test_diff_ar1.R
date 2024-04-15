test_that("Test differenced AR(1) Stan function agrees with R implementation.", {
  model <- compiled_site_inf_model

  withr::with_seed(42, {
    z <- rnorm(100)
    ar <- runif(1)
    sd <- exp(rnorm(1, 0, 0.5))
    x0 <- rnorm(1, 0, 5)
  })

  for (stat in c(TRUE, FALSE)) {
    stan_ar_diff <- model$functions$diff_ar1(
      x0 = x0,
      ar = ar,
      sd = sd,
      z = z,
      is_stat = stat
    )

    r_ar_diff <- diff_ar1_from_z_scores(
      x0 = x0,
      ar = ar,
      sd = sd,
      z = z,
      stationary = stat
    )
    r_ar_diff_alt <- diff_ar1_from_z_scores_alt(
      x0 = x0,
      ar = ar,
      sd = sd,
      z = z,
      stationary = stat
    )

    expect_equal(
      stan_ar_diff,
      r_ar_diff
    )
    expect_equal(
      stan_ar_diff,
      r_ar_diff_alt
    )
  }
})

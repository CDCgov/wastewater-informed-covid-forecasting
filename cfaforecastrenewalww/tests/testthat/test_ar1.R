test_that("Test AR(1) function in stan.", {
  model <- compiled_site_inf_model

  ar1ify <- function(z, sd, ac, stationary = FALSE) {
    x <- rep(NA, length(z))

    x[1] <- z[1] * sd
    if (stationary) {
      stat_sd <- sd / sqrt(1 - ac * ac)
      x[1] <- z[1] * stat_sd
    }

    for (i in 2:length(z)) {
      x[i] <- ac * x[i - 1] + z[i] * sd
    }

    return(x)
  }

  withr::with_seed(42, {
    z <- rnorm(10)

    stan_ar <- model$functions$ar1(
      mu = rep(0, length(z)),
      ac = 0.73,
      sd = 1.26,
      z = z,
      is_stat = TRUE
    )
    r_ar <- ar1ify(z, 1.26, 0.73, TRUE)

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

    r_ar_nonstat <- ar1ify(z, 1.26, 0.73, FALSE)

    testthat::expect_equal(
      stan_ar_nonstat,
      r_ar_nonstat
    )
  })
})

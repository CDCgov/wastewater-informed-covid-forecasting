test_explanation <- paste0(
  "When running a stationary AR(1) process initialized from the stationary variance, ",
  "the marginal variances should be larger than when compared to an AR(1) process ",
  "initialized from the (smaller) variance of the residuals. (At least, until stationarity ",
  "is reached.)"
)

test_that(test_explanation, {
  model <- compiled_site_inf_model

  withr::with_seed(42, {
    stationary <- sapply(1:1e3, function(i) {
      z <- rnorm(10)

      model$functions$ar1(
        mu = rep(0, length(z)),
        ac = 0.95,
        sd = 0.15,
        z = z,
        is_stat = TRUE
      )
    })

    nonstationary <- sapply(1:1e3, function(i) {
      z <- rnorm(10)

      model$functions$ar1(
        mu = rep(0, length(z)),
        ac = 0.95,
        sd = 0.15,
        z = z,
        is_stat = FALSE
      )
    })

    testthat::expect_true(
      all(apply(stationary, 1, var) > apply(nonstationary, 1, var))
    )
  })
})

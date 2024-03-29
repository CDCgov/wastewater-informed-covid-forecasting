test_that("Test logit-scale random walk on probability of being hospitalized in stan works", {
  model <- compiled_site_inf_model

  days_weeks <- dim(toy_stan_data_id$p_hosp_m)
  ndays <- days_weeks[1]
  nweeks <- days_weeks[2]

  # Make sure we cover a wide range
  sigma <- 0.5
  ac <- 0.1
  std_normal <- rnorm((nweeks))
  mu <- rep(logit_fn(0.01), (nweeks))

  # Build the vector ourselves using the AR1 function from stan
  p_hosp_r <- model$functions$ar1(mu, ac, sigma, std_normal, 1)
  p_hosp_r <- rep(p_hosp_r, each = 7) # In R, expand weekly to daily
  p_hosp_r <- inv_logit_fn(p_hosp_r) # convert to natural scale
  p_hosp_r <- p_hosp_r[1:ndays] # Trim to size


  # Get vector from stan and compare
  p_hosp_stan <- model$functions$assemble_p_hosp(
    toy_stan_data_id$p_hosp_m, # matrix to expand from weekly to daily
    mu[1], # intercept to regress back to
    sigma, # SD
    ac, # autocorrelation factor
    std_normal,
    nweeks,
    1
  )

  testthat::expect_equal(
    p_hosp_stan,
    p_hosp_r
  )
})

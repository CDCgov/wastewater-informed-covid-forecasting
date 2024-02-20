test_that("Test logit-scale random walk on probability of being hospitalized in stan works", {
  model <- compiled_site_inf_model

  days_weeks <- dim(toy_stan_data_id$p_hosp_m)
  ndays <- days_weeks[1]
  nweeks <- days_weeks[2]

  # Make sure we cover a wide range
  sigma <- 0.5
  lower <- -20
  upper <- 20
  delta <- ((upper - lower) / (nweeks - 1))

  # Build the vector ourselves
  p_hosp_r <- numeric(nweeks)
  p_hosp_r <- lower + c(0, cumsum(rep(delta, nweeks - 1)))
  p_hosp_r <- inv_logit_fn(p_hosp_r)
  p_hosp_r <- sort(rep(p_hosp_r, 7)) # Make daily, will overshoot
  p_hosp_r <- p_hosp_r[1:ndays] # Trim to size

  # Get vector from stan and compare
  delta <- delta / sigma # make sure stan SD != 1, but canceling them out makes comparing easy
  p_hosp_stan <- model$functions$assemble_p_hosp(
    toy_stan_data_id$p_hosp_m, # cum sum and expand matrix
    lower, # intercept
    sigma, # SD
    rep(delta, nweeks - 1) # noise terms
  )

  testthat::expect_equal(
    p_hosp_stan,
    p_hosp_r
  )
})

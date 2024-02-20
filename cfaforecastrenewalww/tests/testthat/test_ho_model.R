test_that("Test the state-level hospitalization-only model on simulated data.", {
  #######
  # run model briefly on the simulated data
  #######
  # We intentionally use default stan initialization for modularity of the test
  quiet({
    model <- compiled_hosp_only_model
    fit <- model$sample(
      data = toy_stan_data_ho,
      seed = 123,
      iter_sampling = 25,
      iter_warmup = 25,
      chains = 1
    )
  })

  obs_last_draw <- posterior::subset_draws(fit$draws(), draw = 25)

  # Check all parameters (ignoring their dimensions) are in both fits
  # But in a way that makes error messages easy to understand
  obs_par_names <- get_nonmatrix_names_from_draws(obs_last_draw)
  exp_par_names <- get_nonmatrix_names_from_draws(toy_stan_fit_last_draw_ho)

  expect_true(
    all(!!obs_par_names %in% !!exp_par_names)
  )

  expect_true(
    all(!!exp_par_names %in% !!obs_par_names)
  )

  # Check dims
  obs_par_lens <- get_par_dims_flat(obs_last_draw)
  exp_par_lens <- get_par_dims_flat(toy_stan_fit_last_draw_ho)

  agg_names <- c(names(obs_par_lens), names(exp_par_lens)) %>% unique()
  for (param in agg_names) {
    expect_equal(
      obs_par_lens[!!param],
      exp_par_lens[!!param]
    )
  }
  expect_mapequal(
    obs_par_lens,
    exp_par_lens
  )

  # Check the parameters we care most about
  model_params <- get_model_param_df(fit) %>% dplyr::pull(param_name)
  for (param in model_params) {
    expect_equal(
      posterior::subset_draws(obs_last_draw, variable = !!param),
      posterior::subset_draws(toy_stan_fit_last_draw_ho, variable = !!param)
    )
  }

  # Compare everything, with numerical tolerance
  expect_equal(
    obs_last_draw,
    toy_stan_fit_last_draw_ho
  )
})

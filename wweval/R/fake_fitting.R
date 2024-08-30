generate_data <- function(model_type, location, forecast_date,
                          input_ww_data,
                          input_hosp_data,
                          params,
                          n = 10) {
  true_beta <- stats::rnorm(n = 1, mean = 0, sd = 1)
  x <- seq(from = -1, to = 1, length.out = n)
  y <- stats::rnorm(n, x * true_beta, 1)

  return(list(n = n, x = x, y = y, true_beta = true_beta))
}


orig_sample_model <- function(standata, compiled_model, init_lists,
                              iter_warmup = 250,
                              iter_sampling = 250,
                              max_treedepth = 12,
                              adapt_delta = 0.95,
                              n_chains = 4,
                              seed = 123) {
  fit_model <- function(compiled_model, standata) {
    fit <- compiled_model$sample(
      data = standata,
      init = init_lists,
      iter_warmup = iter_warmup,
      iter_sampling = iter_sampling,
      max_treedepth = max_treedepth,
      adapt_delta = adapt_delta,
      num_chains = n_chains,
      seed = seed
    )
    return(fit)
  }

  safe_fit_model <- purrr::safely(fit_model)
  fit <- safe_fit_model(compiled_model, standata)
  obj <- fit$result
  return(obj)
}




get_draws_for_key_pars <- function(fit, pars) {
  raw_draws <- tidybayes::spread_draws(fit, !!!syms(pars))
  return(raw_draws)
}

get_summary <- function(fit) {
  summary <- fit$summary()
  return(summary)
}

get_model_diagnostics <- function(fit) {
  diagnostics <- fit$sampler_diagnostics(format = "df")
  return(diagnostics)
}

get_draws <- function(fit) {
  draws <- fit$draws()
  return(draws)
}

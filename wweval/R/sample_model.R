#' Fit the model
#'
#' @param standata a list of elements to pass to stan
#' @param compiled_model the compiled model object
#' @param init_lists nested list of initial parameter values for each chain
#' @param iter_warmup number of iterations to save in MCMC sampling,
#' default = 250
#' @param iter_sampling number of iterations to save in MCMC sampling,
#' default = 250
#' @param max_treedepth maximum treedepth of MCMC sampling, defauly = 12
#' @param adapt_delta MCMC accaptance probability, default = 0.95
#' @param n_chains number of independent MCMC chains to run, default = 4
#' @param seed seed of random number generator default = 123
#'
#'
#' @return a list containing draws, diagnostics, and summary_diagnostics
#' @export
sample_model <- function(standata,
                         compiled_model,
                         init_lists,
                         iter_warmup = 250,
                         iter_sampling = 250,
                         max_treedepth = 12,
                         adapt_delta = 0.95,
                         n_chains = 4,
                         seed = 123) {
  if (!inherits(compiled_model, "CmdStanModel")) {
    cli::cli_abort(paste0(
      "Argument `compiled_model` must be a ",
      "cmdstanr::CmdStanModel object; got a ",
      "{class(compiled_model)} object instead"
    ))
  }
  fit <- compiled_model$sample(
    data = standata,
    init = init_lists,
    iter_warmup = iter_warmup,
    iter_sampling = iter_sampling,
    max_treedepth = max_treedepth,
    adapt_delta = adapt_delta,
    chains = n_chains,
    seed = seed
  )

  draws <- fit$draws()
  diagnostics <- fit$sampler_diagnostics(format = "df")
  summary_diagnostics <- fit$diagnostic_summary()

  draws_and_diagnostics <- list(
    draws = draws,
    diagnostics = diagnostics,
    summary_diagnostics = summary_diagnostics
  )
  return(draws_and_diagnostics)
}

#' Fit the model
#'
#' @description
#' This code was adapted from code written
#' (under an MIT license) as part of the `epidist`
#' paper (https://github.com/parksw3/epidist-paper).
#' It is a wrapper function that fits a stan model to data
#' and returns a list containing the draws, diagnostics, and
#' summary if the model runs successfully, or returns an error.
#'
#'
#' @param standata a list of elements to pass to stan
#' @param stan_model_path the path to the main stan model
#' @param stan_models_dir the path to the folder with the stan models
#' needed for the include paths
#' @param init_lists nested list of initial parameter values for each chain
#' @param target_dir Directory in which to save the compiled
#' stan model binary. Passed as the `target_dir` keyword argument to
#' [wwinference::compile_model()].
#' Defaults to a temporary directory for the R session
#' (the output of [tempdir()]).
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
#' @return a list containing draws, diagnostics, and summary_diagnostics  +
#' flags if the model failed to pass convergence tests. If the model errored,
#' just contains a list with errors
#' @export

sample_model <- function(standata,
                         stan_model_path,
                         stan_models_dir,
                         init_lists,
                         target_dir = tempdir(),
                         iter_warmup = 250,
                         iter_sampling = 250,
                         max_treedepth = 12,
                         adapt_delta = 0.95,
                         n_chains = 4,
                         seed = 123) {
  compiled_model <- wwinference::compile_model(
    model_filepath = stan_model_path,
    include_paths = stan_models_dir,
    target_dir = target_dir
  )

  if (!inherits(compiled_model, "CmdStanModel")) {
    cli::cli_abort(paste0(
      "Argument `compiled_model` must be a ",
      "cmdstanr::CmdStanModel object; got a ",
      "{class(compiled_model)} object instead"
    ))
  }

  # Set up failure tolerant model fitting
  fit_model <- function(compiled_model,
                        standata,
                        init_lists,
                        iter_warmup,
                        iter_sampling,
                        max_treedepth,
                        adapt_delta,
                        n_chains,
                        seed) {
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
    return(fit)
  }

  safe_fit_model <- purrr::safely(fit_model)
  # This returns the cmdstan object if the model runs, and result = NULL if
  # the model errors
  fit <- safe_fit_model(
    compiled_model,
    standata,
    init_lists,
    iter_warmup,
    iter_sampling,
    max_treedepth,
    adapt_delta,
    n_chains,
    seed
  )


  if (!is.null(fit$error)) { # If the model errors, return a list with the
    # error and everything else NULL
    out <- list(
      error = fit$error[[1]]
    )
  } else {
    # Get the diagnostics using thresholds set in production pipeline
    flag_df <- get_diagnostic_flags(fit$result, n_chains, iter_sampling)

    draws <- fit$result$draws()
    diagnostics <- fit$result$sampler_diagnostics(format = "df")
    summary_diagnostics <- fit$result$diagnostic_summary()
    summary <- fit$result$summary()

    out <- list(
      draws = draws,
      diagnostics = diagnostics,
      summary_diagnostics = summary_diagnostics,
      summary = summary,
      flags = list(flag_df)
    )
  }
  return(out)
}

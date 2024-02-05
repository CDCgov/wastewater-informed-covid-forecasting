#' Runs posterior-predictive analyses and returns summaries of performance
#' @description
#' This function takes in a model object, the observed data, and the configuration file used to
#' produce the real-data stan data object. It then fits the model, extracts generated quantities
#' from some number of those fits, and runs new analyses on the simulated data. It then returns
#' summaries of estimation performance.
#'
#' @param model a CmdStanModel object defining the model
#' @param real_data observed data, formated for stan
#' @param config the output of get_config_vals()
#' @param draw vector, which draws from the MCMC object do we want to run PPS analyses for?
#'  If NULL, runs one analysis per sample. This can take a long time and a lot of space!
#' @param real_data_fit CmdStanMCMC object, optional argument providing a
#'  previous real-data model fit
#' @param pps_mcmc_fits list of CmdStanMCMC objects, optional argument providing
#'  previously-run pps analyses (for re-summarizing)
#' @param mclapply_cores integer specifying number of threads to use for
#' posterior-predictive-analysis-level parallelization, separate from stan parallelization
#'
#' @param ... further arguments passed to model$sample(), e.g. # chains
#'
#' @return a list, each of which is a list of formatted stan data
#' @export
posterior_predictive_analysis <- function(model,
                                          real_data,
                                          config,
                                          draw = NULL,
                                          real_data_fit = NA,
                                          ci_width = 0.89,
                                          mclapply_cores = NA,
                                          ...) {
  alpha <- 1 - ci_width
  ci <- c(alpha / 2, 1 - alpha / 2)

  # Fit any and all models we need to fit
  if (!("CmdStanMCMC" %in% class(real_data_fit))) {
    real_data_fit <- model$sample(data = real_data, ...)
  }

  real_data_draws <- posterior::subset_draws(real_data_fit$draws(), draw = draw)

  stan_predictive_data <- make_pps_data(
    real_data_fit,
    real_data,
    config,
    draw
  )

  pps_mcmc_fits <- NULL

  if (any(is.numeric(mclapply_cores))) {
    pps_mcmc_fits <- parallel::mclapply(seq_along(stan_predictive_data), function(i) {
      .do_1_pps_analysis(i, model, real_data_draws, stan_predictive_data, ci, ...)
    }, mc.cores = mclapply_cores)
  } else {
    pps_mcmc_fits <- lapply(seq_along(stan_predictive_data), function(i) {
      .do_1_pps_analysis(i, model, real_data_draws, stan_predictive_data, ci, ...)
    })
  }

  # Smuggling this around like this avoids having to track/pass objects later
  # but it may be overkill
  attr(pps_mcmc_fits, "model_param_df") <- get_model_param_df(model)

  return(pps_mcmc_fits)
}

#' Internal helper function to do a single analysis of PPS data
#' @description
#' Takes in index, parameter values from which data was simulated,
#' list of simulated data, the model to run, and other arguments for either
#' the model running or summarizing.
#' Runs stan on the simulated data with this index returns a summary of performance.
#'
#' @param i index of simulated data to analyze
#'  the parameters from which the data was simulated
#' @param model CmdStanModel to fit
#' @param real_data_draws draws object containing MCMC samples from real-data model fit
#' @param stan_predictive_data list of simulated datasets, in same order as real_data_draws
#' @param ci length-2 numeric vector of [lower, upper] values for CI
#' @param ... further arguments passed to model$sample(), e.g. # chains
#'
#' @return a summary of the performance of the
#' @keywords internal
.do_1_pps_analysis <- function(i,
                               model,
                               real_data_draws,
                               stan_predictive_data,
                               ci,
                               ...) {
  true_params <- real_data_draws %>%
    posterior::subset_draws(draw = i)

  fit_pp <- quiet(
    model$sample(data = stan_predictive_data[[i]], ...)
  )

  fit_summary <- .summarize_performance(
    true_params,
    fit_pp$draws(),
    ci = ci
  )
  return(fit_summary)
}


#' Internal helper function to summarize how well an analysis of posterior
#' predictive data performed
#' @description
#' Takes in true parameter values and MCMC samples and computes summaries of the
#' samples, possibly wrt the truth.
#'
#' @param true_param_draws draws-family object containing a single draw with
#'  the parameters from which the data was simulated
#' @param fit_draws draws-family object containing the MCMC
#' @param ci length-2 numeric vector of [lower, upper] values for CI
#'
#' @return a list, each of which is a list of formatted stan data
#' @keywords internal
.summarize_performance <- function(true_param_draws,
                                   fit_draws,
                                   ci) {
  post_summary <- posterior::summarize_draws(true_param_draws, mean) %>%
    rename(true_value = mean)

  post_summary <- post_summary %>%
    left_join(
      posterior::summarize_draws(
        fit_draws,
        mean,
        median,
        sd,
        mad,
        ~ quantile(., probs = ci)
      )
    )

  return(post_summary)
}

#' Reformats output of posterior_predictive_analysis() into something more easily usable
#'
#' @param x output of posterior_predictive_analysis()
#' @param keep_generated boolean indicating whether to keep generated quantities or not
#'
#' @return long-format tibble which is per-parameter, per-replicate-analysis
#' @export
get_ppa_long <- function(x, keep_generated = FALSE) {
  stats <- names(x[[1]])[-1]
  nstats <- length(stats)
  nrep <- length(x)

  # Get information about the model's parameters
  param_df <- attr(x, "model_param_df")
  if (isFALSE(keep_generated)) {
    param_df <- param_df %>% filter(!generated_quantity)
  }

  # Hack into a draws format so we can handle same as get_param_samples_long_df() does
  all_draws <- lapply(x, function(pp_rep) {
    cnames <- pp_rep %>% pull(variable)
    pp_rep <- pp_rep %>%
      select(-variable) %>%
      t()
    colnames(pp_rep) <- cnames
    return(posterior::as_draws(pp_rep))
  }) %>%
    bind_draws(along = "draw")

  # Get in long format
  full_param_df <- lapply(param_df$param_name, function(p_name) {
    .get_1_param(
      param_df %>% filter(param_name == p_name),
      all_draws
    )
  }) %>%
    bind_rows() %>%
    mutate(rep = 1 + floor((draw - 1) / nstats)) %>%
    mutate(stat_index = 1 + (draw - 1) %% nstats) %>%
    mutate(stat = stats[stat_index]) %>%
    select(-draw, -stat_index)

  return(full_param_df)
}

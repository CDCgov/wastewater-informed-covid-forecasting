#' Draws data under the model's prior.
#'
#' @description
#' This function takes in a model object, observed data, and the configuration file used to
#' produce the real-data stan data object. It then runs the model under the prior and
#' returns the observable (generated quantities) for some number of draws.
#'
#' @param model a CmdStanModel object defining the model
#' @param real_data observed data, formated for stan, needed to match, for example, the number
#' of wastewater sites
#' @param config the output of get_config_vals()
#' @param ndraws integer, number of draws from the prior predictive distribution to return
#' @param mclapply_cores integer specifying number of threads to use for
#' posterior-predictive-analysis-level parallelization, separate from stan parallelization
#'
#' @param ... further arguments passed to model$sample(), e.g. # chains
#'
#' @return a list, each of which is a list of formatted stan data
#' @export
prior_predictive_check <- function(model,
                                   real_data,
                                   config,
                                   ndraws,
                                   ...) {
  real_data$compute_likelihood <- FALSE
  prior_fit <- model$sample(data = real_data, ...)

  par_df <- get_model_param_df(model)

  prior_draws <- posterior::as_draws_list(prior_fit$draws())
  n_prior_samples <- length(prior_draws[[1]][[1]]) * length(prior_draws)
  draw <- round(seq(1, n_prior_samples, length.out = ndraws))

  prior_predictive_data <- make_pps_data(
    prior_fit,
    real_data,
    config,
    draw
  )

  return(prior_predictive_data)
}

#' Observables extraction from prior predictive samples
#'
#' @param prior_pred output of prior_predictive_check, stan data object
#'
#' @return a tibble, with columns time (the time),
#' hosp (the hospitalization count), and
#' draw (the draw this came from)
#' @export
get_prior_pred_hosps_df <- function(prior_pred) {
  lapply(seq_along(prior_pred), function(i) {
    pp <- prior_pred[[i]]
    dplyr::bind_cols(time = pp$hosp_times, hosp = pp$hosp, draw = i)
  }) |>
    dplyr::bind_rows()
}

#' Observables extraction from prior predictive samples
#'
#' @param prior_pred output of prior_predictive_check, stan data object
#' @param site_index which site to extract
#'
#' @return a tibble, with columns time (the time),
#' log_conc (the ww log-concentration), and
#' draw (the draw this came from)
#' @export
get_prior_pred_ww_df <- function(prior_pred, site_index) {
  lapply(seq_along(prior_pred), function(i) {
    pp <- prior_pred[[i]]
    ww <- pp$log_conc[pp$ww_sampled_sites == site_index]
    times <- pp$ww_sampled_times[pp$ww_sampled_sites == site_index]
    dplyr::bind_cols(time = times, log_conc = ww, draw = i)
  }) |>
    dplyr::bind_rows()
}

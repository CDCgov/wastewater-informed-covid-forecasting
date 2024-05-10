#' Get diagnostic flags
#'
#' @description
#' This function takes in the output from a cmdstanr$sample() function (the
#' fit object) and a series of diagnostic toleraances and returns
#' a dataframe containing flags for whether any of the diagnostic thresholds
#' were exceeded, which would indicate that the model did not properly
#' converge
#'
#'
#' @param stan_fit_object The R6 Cmdstan Object fit object
#' @param n_chains number of chains fun
#' @param iter_sampling number of sampling iterations
#' @param ebmfi_tolerance Tolerance for EBMFI (bayesian missing information)
#' @param divergences_tolerance tolerance for proportion of sampling iterations
#' that are divergent
#' @param p_high_rhat_tolerance tolerance for proportion of parameters rhats>1.05
#' @param max_tree_depth_tol tolerance for proportion of iterations that exceed
#' the maximum tree depth
#'
#' @return flag_df: dataframe containing columns for each of the flags,
#' if any flags are TRUE that indicates some model issue
#' @export
#'
get_diagnostic_flags <- function(stan_fit_object,
                                 n_chains,
                                 iter_sampling,
                                 ebmfi_tolerance = 0.2,
                                 divergences_tolerance = 0.01,
                                 p_high_rhat_tolerance = 0.05,
                                 max_tree_depth_tol = 0.01) {
  diagnostic_summary <- stan_fit_object$diagnostic_summary(quiet = TRUE)


  # Summary is a large dataframe with diagnostics for each parameters
  summary <- stan_fit_object$summary()

  flag_low_embfi <- mean(diagnostic_summary$ebfmi) <= ebmfi_tolerance
  max_n_divergences <- n_chains * iter_sampling * divergences_tolerance
  flag_too_many_divergences <- any(diagnostic_summary$num_divergent >= max_n_divergences)
  p_high_rhat <- as.numeric(mean(summary[, "rhat"]$rhat > 1.05, na.rm = TRUE))
  flag_high_rhat <- p_high_rhat >= p_high_rhat_tolerance
  max_n_max_treedepth <- n_chains * iter_sampling * max_tree_depth_tol
  flag_high_max_treedepth <- any(diagnostic_summary$num_max_tree_depth >= max_n_max_treedepth)

  flag_df <- data.frame(
    flag_high_max_treedepth,
    flag_too_many_divergences,
    flag_high_rhat,
    flag_low_embfi
  )
  return(flag_df)
}

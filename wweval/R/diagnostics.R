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
#' @param ebfmi_tolerance Tolerance for EBFMI (bayesian missing information)
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
get_diagnostic_flags <- function(
  stan_fit_object,
  ebfmi_tolerance = 0.2,
  divergences_tolerance = 0.01,
  p_high_rhat_tolerance = 0.05,
  max_tree_depth_tol = 0.01
) {
  meta <- stan_fit_object$metadata()
  n_draws <- meta$iter_sampling * meta$num_chains
  diagnostic_summary <- stan_fit_object$diagnostic_summary(quiet = TRUE)
  # Summary is a large dataframe with diagnostics for each parameters
  summary <- stan_fit_object$summary()

  flag_low_ebfmi <- mean(diagnostic_summary$ebfmi) <= ebfmi_tolerance
  max_n_divergences <- n_draws * divergences_tolerance
  flag_too_many_divergences <- any(
    diagnostic_summary$num_divergent >= max_n_divergences
  )
  p_high_rhat <- as.numeric(mean(
    summary[, "rhat"]$rhat > 1.05,
    na.rm = TRUE
  ))
  flag_high_rhat <- p_high_rhat >= p_high_rhat_tolerance
  max_n_max_treedepth <- n_draws * max_tree_depth_tol
  flag_high_max_treedepth <- any(
    diagnostic_summary$num_max_tree_depth >= max_n_max_treedepth
  )

  flag_df <- data.frame(
    flag_high_max_treedepth,
    flag_too_many_divergences,
    flag_high_rhat,
    flag_low_ebfmi
  )
  return(flag_df)
}

#' Compute the maximum rhat and minimum bulk and tail ESS from an array of parameters
#'
#' @param stanfit CmdStanR fit object
#' @param parameter name of the array-valued parameter
#' @param index_subject optional subset of indices to consider,
#' e.g. `10:15`
#' from the overall array
#' @return tibble with the summary
#' @export
extract_parameter_diagnostics <- function(stanfit, parameter, index_subset) {
  param_summary <- stanfit$summary(variables = parameter)

  if (!is.null(index_subset)) {
    param_summary <- param_summary[index_subset, ]
  }

  diagnostics <- param_summary |>
    tibble::as_tibble() |>
    dplyr::summarise(
      max_rhat = max(.data$rhat),
      which_max_rhat = .data$variable[which.max(.data$rhat)],

      min_ess_bulk = min(.data$ess_bulk),
      which_min_ess_bulk = .data$variable[which.min(.data$ess_bulk)],
      min_ess_tail = min(.data$ess_tail),
      which_min_ess_tail = .data$variable[which.min(.data$ess_tail)]
    )

  return(diagnostics)
}


#' Get convergence dataframe
#' @description This function takes the larger dataframe of convergence
#' flags for each location, forecast date, and scenario and checks if any of
#' the flags are TRUE, and returns a dataframe with just a column indicating
#' whether any flags are true
#'
#' @param all_flags a dataframe containing the flags for each location,
#' forecast_date, and scenario
#' @param scenario The scenario to filter to, since some eval output will include multiple
#' scenarios
#'
#' @return a dataframe with a column `any_flags` indicating whether any of the
#' flags in the original full descriptive set of congerence flags are TRUE.
#' @export
#'
get_convergence_df <- function(all_flags, scenario) {
  convergence_df <- all_flags |>
    dplyr::filter(.data$scenario == {{ scenario }}) |>
    dplyr::summarise(
      any_flags = dplyr::if_any(tidyselect::starts_with("flag")),
      .by = c("location", "forecast_date", "scenario", "model_type")
    ) |>
    dplyr::select(
      "location",
      "forecast_date",
      "any_flags"
    )

  return(convergence_df)
}

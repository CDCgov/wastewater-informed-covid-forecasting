#' Gets generated quantites from a fitted ww model and formats for stan
#' @description
#' This function takes in a fitted wastewater model and some of the inputs which produced
#' it and returns data which can be used to fit the model to the posterior
#' predictive distribution of datasets.
#'
#' @param mcmc_fit CmdStanMCMC object
#' @param real_data the stan data object which produced the mcmc_fit
#' @param config the output of get_config_vals() which produced the mcmc_fit
#' @param draw vector, which draws from the MCMC object do we want in the result?
#'  If NULL, uses all. This can be a very large data frame!
#'
#' @return a list, each of which is a list of formatted stan data
#' @export
make_pps_data <- function(mcmc_fit,
                          real_data,
                          config,
                          draw = NULL) {
  all_draws <- posterior::subset_draws(
    # list is supposedly more RAM-friendly format
    mcmc_fit$draws(format = "draws_list"),
    draw = draw
  )

  # Get information about the model's generated quantities
  param_df <- get_model_param_df(mcmc_fit) %>%
    filter(generated_quantity)

  pred_hosp_data <- .get_1_param(
    param_df %>% filter(param_name == "pred_hosp"),
    all_draws
  )

  pred_ww_data <- .get_1_param(
    param_df %>% filter(param_name == "pred_ww"),
    all_draws
  )

  pps <- lapply(seq_along(draw), function(new_draw_idx) {
    .make_1_pps_data(
      pred_hosp_data %>% filter(draw == new_draw_idx),
      pred_ww_data %>% filter(draw == new_draw_idx),
      real_data,
      config
    )
  })

  return(pps)
}

#' Internal helper for make_pps_data()
#' @description
#' Takes in pre-processed predictive hospital and wastewater data, plus a real-data
#' stan input and a config. Outputs a posterior predictive dataset.
#'
#' @param pps_hosp_data one draw's worth of pps hospital data
#' @param pps_ww_data one draw's worth of pps ww data
#' @param real_data the stan data object
#' @param config the output of get_config_vals() for the original (real) data
#'
#' @return a list, as real_data but with the pps hospital and ww data instead
#' of the real observations
#' @keywords internal

.make_1_pps_data <- function(pps_hosp_data,
                             pps_ww_data,
                             real_data,
                             config) {
  stan_data <- real_data

  y_df <- dplyr::bind_rows(
    pps_hosp_data,
    pps_ww_data
  )

  hosp_reporting_delay <- config$hosp_reporting_delay
  length_generated_hosp <- y_df %>%
    dplyr::filter(name == "pred_hosp") %>%
    nrow()
  calibration_time <- config$calibration_time
  length_input_hosp <- calibration_time

  # Reallocate hosp vector to be the same length as input hosp vector
  hosp <- y_df %>%
    dplyr::filter(
      name == "pred_hosp",
      day <= calibration_time
    ) %>%
    dplyr::pull(value)

  # Reallocate wastewater vector to be the same parameter as
  # the input wastewater vector
  ww_sampled_times <- stan_data$ww_sampled_times
  ww_sampled_lab_sites <- stan_data$ww_sampled_lab_sites
  owt <- stan_data$owt

  # Make into a matrix with each row a ww_lab_site and each column a time
  ww_conc_matrix <- y_df %>%
    dplyr::filter(name == "pred_ww") %>%
    dplyr::select(value, ww_lab_site, day) %>%
    tidyr::pivot_wider(
      names_from = "day",
      values_from = "value"
    ) %>%
    dplyr::select(-ww_lab_site)
  ww_conc_matrix <- data.matrix(ww_conc_matrix)

  # Get the vector of generated  wastewater concentrations using the same
  # sampling as in the "real" data
  log_conc <- rep(0, owt)
  for (i in 1:owt) {
    log_conc[i] <- ww_conc_matrix[ww_sampled_lab_sites[i], ww_sampled_times[i]]
  }


  stan_data$log_conc <- log_conc
  stan_data$hosp <- hosp

  return(stan_data)
}

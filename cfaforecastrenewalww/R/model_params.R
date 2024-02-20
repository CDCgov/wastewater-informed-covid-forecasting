#' Information about a ww model's parameters and generated quantities
#'
#' @description
#' Accepts a string describing the model name, a fitted model (output of model$sample()),
#' or a model object.
#' Returns information about the model parameters.
#'
#' @param x CmdStanModel or CmdStanFit object
#'
#' @return a dataframe containing the name of each parameter (and/or generated quantity)
#' and information about how it varies in time and across sites/labs.
#'
#' @export
get_model_param_df <- function(x) {
  model <- x
  if ("CmdStanModel" %in% class(x)) {
    model <- basename(x$stan_file()) %>%
      gsub(".stan", "", .)
  } else if ("CmdStanFit" %in% class(x)) {
    model <- x$metadata()$model_name %>%
      gsub("_model$", "", .)
  } else if (class(x) != "character") {
    stop("Input to 'get_model_param_df' must be a character, a CmdStanModel, or a CmdStanFit.")
  }

  static_params <- NA
  daily_params <- NA
  weekly_params <- NA
  day_of_week_params <- NA

  site_static_params <- NA
  site_daily_params <- NA
  site_weekly_params <- NA
  site_day_of_week_params <- NA

  lab_static_params <- NA
  lab_daily_params <- NA
  lab_weekly_params <- NA
  lab_day_of_week_params <- NA

  lab_site_static_params <- NA
  lab_site_daily_params <- NA
  lab_site_weekly_params <- NA
  lab_site_day_of_week_params <- NA

  static_gq <- NA
  daily_gq <- NA
  weekly_gq <- NA
  day_of_week_gq <- NA

  site_static_gq <- NA
  site_daily_gq <- NA
  site_weekly_gq <- NA
  site_day_of_week_gq <- NA

  lab_static_gq <- NA
  lab_daily_gq <- NA
  lab_weekly_gq <- NA
  lab_day_of_week_gq <- NA

  lab_site_static_gq <- NA
  lab_site_daily_gq <- NA
  lab_site_weekly_gq <- NA
  lab_site_day_of_week_gq <- NA

  # renewal_ww_hosp_site_level_phi_varying_C
  if (model %in% c(
    "renewal_ww_hosp_site_level_phi_varying_C",
    "site-level time-varying concentration"
  )) {
    stop("This model has been deprecated.")
    static_params <- c(
      "eta_sd", "autoreg_rt", "autoreg_conc", "log_R",
      "sigma_log_conc", "i0_over_n", "initial_growth", "inv_sqrt_phi_h",
      "sigma_ww_site_mean", "sigma_ww_site_sd", "p_hosp_int", "p_hosp_w_sd",
      "t_peak", "viral_peak", "dur_shed", "log10_g",
      "ww_site_mod_sd", "infection_feedback"
    )
    daily_params <- c("R", "new_I", "p_hosp")
    weekly_params <- c("w", "p_hosp_w")
    day_of_week_params <- c("hosp_wday_effect")

    lab_site_static_params <- c(
      "sigma_ww_site_raw", "ww_site_mod_raw",
      "ww_site_mod", "sigma_ww_site"
    )
    lab_site_daily_params <- c("error_conc_site", "log_conc_site")

    daily_gq <- c("pred_hosp")
    lab_site_daily_gq <- c("pred_ww")
  } else if (model %in% c(
    "renewal_ww_hosp_site_level_inf_dynamics",
    "site-level infection dynamics"
  )) {
    static_params <- c(
      "eta_sd", "autoreg_rt", "log_r_mu_intercept", "sigma_rt",
      "autoreg_rt_site", "i0_over_n", "sigma_i0", "sigma_growth",
      "initial_growth", "inv_sqrt_phi_h", "sigma_ww_site_mean", "sigma_ww_site_sd",
      "p_hosp_w_sd", "t_peak", "dur_shed", "ww_site_mod_sd",
      "infection_feedback"
    )
    daily_params <- c("rt", "new_i", "p_hosp")
    weekly_params <- c("w", "p_hosp_w")
    day_of_week_params <- c("hosp_wday_effect")

    site_static_params <- c("eta_i0", "eta_growth")
    site_daily_params <- c("r_site_t")

    lab_site_static_params <- c("ww_site_mod_raw", "sigma_ww_site_raw")

    daily_gq <- c("pred_hosp")
    lab_site_daily_gq <- c("pred_ww")
  } else if (model %in% c(
    "renewal_ww_hosp",
    "hospital admissions only"
  )) {
    static_params <- c(
      "eta_sd", "autoreg_rt", "log_r",
      "i0_over_n",
      "initial_growth", "inv_sqrt_phi_h", "sigma_ww",
      "p_hosp_w_sd", "t_peak", "dur_shed",
      "infection_feedback"
    )
    daily_params <- c("rt", "new_i", "p_hosp")
    weekly_params <- c("w", "p_hosp_w")
    day_of_week_params <- c("hosp_wday_effect")

    daily_gq <- c("pred_hosp", "pred_conc")
  } else if (model == "renewal_ww_hosp_hierarchical_w_dispersion") {
    stop("Not yet implemented.")
  } else {
    stop("Unrecognized model: ", model)
  }

  param_list <- list(
    static_params = static_params,
    daily_params = daily_params,
    weekly_params = weekly_params,
    day_of_week_params = day_of_week_params,
    site_static_params = site_static_params,
    site_daily_params = site_daily_params,
    site_weekly_params = site_weekly_params,
    site_day_of_week_params = site_day_of_week_params,
    lab_static_params = lab_static_params,
    lab_daily_params = lab_daily_params,
    lab_weekly_params = lab_weekly_params,
    lab_day_of_week_params = lab_day_of_week_params,
    lab_site_static_params = lab_site_static_params,
    lab_site_daily_params = lab_site_daily_params,
    lab_site_weekly_params = lab_site_weekly_params,
    lab_site_day_of_week_params = lab_site_day_of_week_params,
    static_gq = static_gq,
    daily_gq = daily_gq,
    weekly_gq = weekly_gq,
    day_of_week_gq = day_of_week_gq,
    site_static_gq = site_static_gq,
    site_daily_gq = site_daily_gq,
    site_weekly_gq = site_weekly_gq,
    site_day_of_week_gq = site_day_of_week_gq,
    lab_static_gq = lab_static_gq,
    lab_daily_gq = lab_daily_gq,
    lab_weekly_gq = lab_weekly_gq,
    lab_day_of_week_gq = lab_day_of_week_gq,
    lab_site_static_gq = lab_site_static_gq,
    lab_site_daily_gq = lab_site_daily_gq,
    lab_site_weekly_gq = lab_site_weekly_gq,
    lab_site_day_of_week_gq = lab_site_day_of_week_gq
  )

  df <- data.frame(
    daily              = rep(c(FALSE, TRUE, FALSE, TRUE), 8),
    weekly             = rep(c(FALSE, FALSE, TRUE, FALSE), 8),
    cyclic             = rep(c(FALSE, FALSE, FALSE, TRUE), 8),
    per_site           = rep(c(rep(FALSE, 4), rep(TRUE, 4), rep(FALSE, 4), rep(FALSE, 4)), 2),
    per_lab            = rep(c(rep(FALSE, 4), rep(FALSE, 4), rep(TRUE, 4), rep(FALSE, 4)), 2),
    per_lab_site       = rep(c(rep(FALSE, 4), rep(FALSE, 4), rep(FALSE, 4), rep(TRUE, 4)), 2),
    generated_quantity = c(rep(FALSE, 16), rep(TRUE, 16))
  )
  df$param_list <- param_list

  df <- df %>%
    dplyr::relocate(param_list) %>%
    tidyr::unnest(cols = c(param_list)) %>%
    dplyr::filter(!is.na(param_list)) %>%
    dplyr::rename(param_name = param_list) %>%
    dplyr::mutate(time_varying = weekly | daily) %>%
    dplyr::mutate(lab_or_site_varying = per_lab | per_site | per_lab_site)

  return(df)
}

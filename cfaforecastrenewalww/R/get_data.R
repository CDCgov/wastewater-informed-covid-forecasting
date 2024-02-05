## Get model parameters (priors and set params of set distributions)------------
#' @title Get parameters for model run
#'
#' @return a dataframe with numeric values for parameter values passed to the
#'  model
#' @export
#'
#' @examples
get_params <- function() {
  ml_of_ww_per_person_day <- 22.7e4

  # Time to estimate back to for i0
  uot <- 50

  # Generation Interval
  # From: Park, Sang Woo, et al. "Inferring the differences in incubation-period
  # and generation-interval distributions of the Delta and Omicron variants of
  # SARS-CoV-2." Proceedings of the National Academy of Sciences 120.22 (2023):
  # e2221887120.
  # from the object in Fig 4F corresponding to between household transmission
  # in Omicron https://github.com/parksw3/omicron-generation/blob/d36d4568bfd3b3d389b30282758b9c322cfe2b9f/figure/compare.R#L175 #nolint
  # Mean of 2.9 days
  mu_gi <- 0.92877
  sigma_gi <- 0.526 # (using lognormal CDF and Park CIs of 2.7 and 3.2)

  # Incubation period parameters
  # From: Park, Sang Woo, et al. "Inferring the differences in incubation-period
  # and generation-interval distributions of the Delta and Omicron variants of
  # SARS-CoV-2." Proceedings of the National Academy of Sciences 120.22 (2023):
  # e2221887120.
  r <- 0.15
  backward_shape <- 1.5
  backward_scale <- 3.6

  # Symptom onset to hospital admission delay parameters
  # From Danache et al
  # https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0261428#:~:text=Multiple%20analysis%20with%20correction%20for%20multiple%20testing%20showed,a%20long%20delay%20%28aOR%3A%201.84%2095%25%20CI%20%281.32%E2%80%932.55%29%29. #nolint
  # That Zack fit and described in cfa-parameter-estimates github repo
  neg_binom_mu <- 6.98665
  neg_binom_size <- 2.490848


  gt_max <- 15
  dur_inf <- 7 # used for drift calculation of number currently infected

  # Hospitalization parameters (informative priors)
  # IHR estimate from: https://www.nature.com/articles/s41467-023-39661-5
  p_hosp_mean <- 0.031 # mean =3.1%, 0.031
  p_hosp_sd <- 0.0015 # CI = 2.7-3.3%, 0.027, 0.031, so 1.96*sd = 0.003

  # Infection feedback priors
  infection_feedback_prior_mean <- 500
  infection_feedback_prior_sd <- 200

  # Wastewater skedding kinetics (informative priors)
  t_peak_mean <- 5 # Pretend we know this with uncertainty
  t_peak_sd <- 1
  viral_peak_mean <- 5.1
  viral_peak_sd <- 0.5
  duration_shedding_mean <- 17
  duration_shedding_sd <- 3

  # Uninformed priors
  inv_sqrt_phi_prior_mean <- 1 / sqrt(100)
  inv_sqrt_phi_prior_sd <- 1 / sqrt(50)
  phi_w_prior_mean <- 100
  phi_w_prior_sd <- 30
  sigma_ww_site_prior_mean_mean <- 0.5
  sigma_ww_site_prior_mean_sd <- 1
  sigma_ww_site_prior_sd_mean <- 0
  sigma_ww_site_prior_sd_sd <- 1
  r_prior_mean <- 1
  r_prior_sd <- 1
  sigma_rt_prior <- 0.3
  log10_g_prior_mean <- 12
  log10_g_prior_sd <- 2
  log_g_prior_mean <- 12 * log(10)
  log_g_prior_sd <- 2 * log(10)
  wday_effect_prior_mean <- 1 / 7
  wday_effect_prior_sd <- 0.05
  initial_growth_prior_mean <- 0
  initial_growth_prior_sd <- 0.01
  autoreg_rt_a <- 2 # shape1 parameter of autoreg term on Rt trend
  autoreg_rt_b <- 40 # shape2 parameter of autoreg on Rt trend
  autoreg_conc_a <- 1 # shape1 parameter of autoreg term on Ct trend
  autoreg_conc_b <- 2 # shape 2 parameter of autoreg term on Ct trend
  # mean = a/(a+b) = 0.05, stdv = sqrt(a)/b = sqrt(2)/40 = 0.035
  eta_sd_sd <- 0.01
  p_hosp_sd_logit <- 0.3
  p_hosp_w_sd_sd <- 0.01
  ww_site_mod_sd_sd <- 0.25
  log_phi_g_prior_mean <- log(0.1) # prior mean in individual level dispersion
  # in fecal shedding
  log_phi_g_prior_sd <- 5 # wide std

  # Priors for initial growth and drift term (which we will eventually fit)
  drift <- 1
  initial_growth <- 0

  params <- data.frame(
    ml_of_ww_per_person_day, uot, mu_gi, sigma_gi,
    r, backward_shape, backward_scale,
    neg_binom_mu, neg_binom_size,
    gt_max, p_hosp_mean, p_hosp_sd,
    t_peak_mean, t_peak_sd, viral_peak_mean, viral_peak_sd,
    duration_shedding_mean, duration_shedding_sd,
    inv_sqrt_phi_prior_mean, inv_sqrt_phi_prior_sd,
    phi_w_prior_mean, phi_w_prior_sd,
    r_prior_mean, r_prior_sd, log10_g_prior_mean,
    log10_g_prior_sd, log_g_prior_mean,
    log_g_prior_sd,
    wday_effect_prior_mean,
    wday_effect_prior_sd, initial_growth_prior_mean, initial_growth_prior_sd,
    drift, initial_growth, dur_inf,
    autoreg_rt_a, autoreg_rt_b,
    autoreg_conc_a, autoreg_conc_b,
    sigma_ww_site_prior_mean_mean,
    sigma_ww_site_prior_mean_sd,
    sigma_ww_site_prior_sd_mean,
    sigma_ww_site_prior_sd_sd,
    eta_sd_sd,
    p_hosp_sd_logit,
    p_hosp_w_sd_sd,
    ww_site_mod_sd_sd,
    sigma_rt_prior,
    log_phi_g_prior_mean,
    log_phi_g_prior_sd,
    infection_feedback_prior_mean,
    infection_feedback_prior_sd
  )

  return(params)
}


## Get stan data for the different models -------------------------------------

#' @title Get stan data
#' @export
#' @description Stan data needed for the model where we aggregate the ww data
#' so that we have at most one WW osbservation per day (but likely a weekly avg)
#'
#'
#' @param train_data
#' @param params
#' @param forecast_date
#' @param forecast_time
#' @param include_hosp
#' @param compute_likelihood
#' @param generation_interval vector of discretized probability mass indexed
#' starting at day 1 after infection, describing the probability of onwards
#' infection on each day
#' after infection
#' @param inf_to_hosp vector of discretized probability mass describing the
#' probability of hopsital admissions on each day of infection, indexed
#' starting at 0
#' @param infection_feedback_pmf vector of discretized probability mass
#' used to enforce the delay distribution on infection feedback
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
get_stan_data <- function(train_data, params, forecast_date,
                          forecast_time, include_hosp,
                          compute_likelihood,
                          generation_interval,
                          inf_to_hosp,
                          infection_feedback_pmf, ...) {
  # Assign parmeter names
  par_names <- colnames(params)
  for (i in seq_along(par_names)) {
    assign(par_names[i], as.double(params[i]))
  }


  # Train data is only for a single targroup,
  include_ww <- unique(train_data$include_ww)
  stopifnot(
    "Multiple model types getting passed into model" =
      length(include_ww) == 1
  )

  last_obs_hosp_date <- max(train_data$date[!is.na(
    train_data$daily_hosp_admits
  )])
  last_obs_ww_date <- train_data %>%
    filter(!is.na(ww), period != "forecast") %>%
    select(date) %>%
    pull(date) %>%
    max()
  last_date <- max(last_obs_hosp_date, last_obs_ww_date)

  nowcast_time <- train_data %>%
    select(date, period) %>%
    filter(period == "nowcast") %>%
    nrow()

  # Get indices
  ot <- train_data %>%
    filter(period == "calibration") %>%
    nrow() # all observed time
  # number of days with which we observe WW
  owt <- train_data %>%
    filter(period != "forecast", !is.na(ww)) %>%
    nrow()
  # hospital admissions times
  oht <- train_data %>%
    filter(period != "forecast", !is.na(daily_hosp_admits)) %>%
    nrow()

  # horizon time (forecast, though this could in reality include a nowcast)
  ht <- nowcast_time + forecast_time
  # vector of obsherved WW times (starting from
  # day 1 = first observed hospital admissions day)
  ww_sampled_times <- train_data %>%
    filter(period != "forecast", !is.na(ww)) %>%
    pull(t)
  # vector of hospitalized times
  hosp_times <- train_data %>%
    filter(period != "forecast", !is.na(daily_hosp_admits)) %>%
    pull(t)

  t <- seq(1, ht + ot)
  dates <- seq(
    from = min(train_data$date), to =
      (min(train_data$date) + days(ht + ot - 1)), by = "days"
  )
  day_of_week <- lubridate::wday(dates, week_start = 1)
  n_weeks <- ceiling((ot + ht) / 7)
  tot_weeks <- ceiling((ot + uot + ht) / 7)

  # matrix to convert R(t) from weekly to daily
  ind_m <- get_ind_m(ot + ht, n_weeks)

  # matrix to transform p_hosp RW from weekly to daily
  p_hosp_m <- get_ind_m_cum_sum(uot + ot + ht, tot_weeks)

  # Get other variables needed from data
  pop <- train_data %>%
    select(pop) %>%
    unique() %>%
    pull(pop)
  stopifnot("More than one population size in training data" = length(pop) == 1)

  # Estimate of number of initial infections
  i0 <- mean(train_data$daily_hosp_admits[1:7], na.rm = TRUE) / p_hosp_mean


  # package up parameters for stan data object
  viral_shedding_pars <- c(
    t_peak_mean, t_peak_sd, viral_peak_mean, viral_peak_sd,
    duration_shedding_mean, duration_shedding_sd
  )


  hosp_delay_max <- length(inf_to_hosp)
  hosp_train <- train_data %>% filter(
    period == "calibration",
    !is.na(daily_hosp_admits)
  )
  ww_train <- train_data %>% filter(period != "forecast", !is.na(ww))

  data_renewal <- list(
    gt_max = gt_max,
    hosp_delay_max = hosp_delay_max,
    inf_to_hosp = inf_to_hosp,
    dur_inf = dur_inf, # this is used bc drift approach needs currently infected
    mwpd = ml_of_ww_per_person_day,
    ot = ot,
    owt = owt,
    oht = oht,
    uot = uot,
    ht = ht,
    n_weeks = n_weeks,
    ind_m = ind_m,
    tot_weeks = tot_weeks,
    p_hosp_m = p_hosp_m,
    generation_interval = generation_interval,
    ts = 1:gt_max,
    n = pop,
    hosp_times = hosp_times,
    ww_sampled_times = ww_sampled_times,
    hosp = hosp_train$daily_hosp_admits,
    day_of_week = day_of_week,
    log_conc = log(ww_train$ww + 1e-8),
    compute_likelihood = compute_likelihood, #
    include_ww = include_ww,
    include_hosp = include_hosp,
    if_l = length(infection_feedback_pmf),
    infection_feedback_pmf = infection_feedback_pmf,
    # Priors
    viral_shedding_pars = viral_shedding_pars, # tpeak, viral peak,
    # duration shedding
    autoreg_rt_a = autoreg_rt_a,
    autoreg_rt_b = autoreg_rt_b,
    inv_sqrt_phi_prior_mean = inv_sqrt_phi_prior_mean,
    inv_sqrt_phi_prior_sd = inv_sqrt_phi_prior_sd,
    r_prior_mean = r_prior_mean,
    r_prior_sd = r_prior_sd,
    log10_g_prior_mean = log10_g_prior_mean,
    log10_g_prior_sd = log10_g_prior_sd,
    log_i0_prior_mean = log(i0 / pop),
    log_i0_prior_sd = 1,
    wday_effect_prior_mean = wday_effect_prior_mean,
    wday_effect_prior_sd = wday_effect_prior_sd,
    initial_growth_prior_mean = initial_growth_prior_mean,
    initial_growth_prior_sd = initial_growth_prior_sd,
    sigma_ww_prior_mean = sigma_ww_site_prior_mean_mean,
    eta_sd_sd = eta_sd_sd,
    p_hosp_mean = p_hosp_mean,
    p_hosp_sd_logit = p_hosp_sd_logit,
    p_hosp_w_sd_sd = p_hosp_w_sd_sd,
    infection_feedback_prior_mean = infection_feedback_prior_mean,
    infection_feedback_prior_sd = infection_feedback_prior_sd
  )

  return(data_renewal)
}

#' @title Get stan data site level model
#' @description
#' Get the formating of the stan data needed for the site level model, where
#' we can observe WW observations on any day and multiple per day across
#' different sites
#'
#'
#' @param train_data
#' @param params
#' @param forecast_date
#' @param forecast_time
#' @param include_hosp
#' @param compute_likelihood
#' @param model_type
#' @param generation_interval vector of discretized probability mass indexed
#' starting at day 1 after infection, describing the probability of onwards
#' infection on each day
#' after infection
#' @param inf_to_hosp vector of discretized probability mass describing the
#' probability of hopsital admissions on each day of infection, indexed
#' starting at 0
#' @param infection_feedback_pmf vector of discretized probability mass
#' used to enforce the delay distribution on infection feedback
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
get_stan_data_site_level_model <- function(train_data, params, forecast_date,
                                           forecast_time,
                                           model_type,
                                           generation_interval,
                                           inf_to_hosp,
                                           infection_feedback_pmf,
                                           include_hosp = 1,
                                           compute_likelihood = 1,
                                           ...) {
  # Assign parmeter names
  par_names <- colnames(params)
  for (i in seq_along(par_names)) {
    assign(par_names[i], as.double(params[i]))
  }

  # Filter to remove outliers in WW data
  n_ww_data_points <- sum(!is.na(train_data$ww))
  train_data <- train_data %>% mutate(
    ww = ifelse(flag_as_ww_outlier != 1, ww, NA)
  )

  # Train data is only for a single targroup,
  include_ww <- unique(train_data$include_ww)
  stopifnot(
    "Multiple model types getting passed into model" =
      length(include_ww) == 1
  )

  last_obs_hosp_date <- max(train_data$date[!is.na(
    train_data$daily_hosp_admits
  )])
  last_obs_ww_date <- train_data %>%
    filter(!is.na(ww), date <= (forecast_date - days(2))) %>%
    select(date) %>%
    pull(date) %>%
    max()
  last_date <- max(last_obs_hosp_date, last_obs_ww_date)

  # Populations should be based on the site, not the lab-site combo
  # if there are multiple populations, start by just taking the avg
  # later, can refactor to pass in a vector or matrix by date
  pop_ww <- train_data %>%
    select(site_index, ww_pop) %>%
    filter(!is.na(site_index)) %>%
    group_by(site_index) %>%
    summarise(pop_avg = mean(ww_pop)) %>%
    arrange(site_index, "desc") %>%
    pull(pop_avg)

  nowcast_time <- train_data %>%
    select(date, period) %>%
    filter(period == "nowcast") %>%
    distinct() %>%
    nrow()

  # Get indices
  ot <- train_data %>%
    filter(period == "calibration") %>%
    select(t) %>%
    distinct() %>%
    nrow() # all observed time
  # number of days with which we observe WW
  owt <- train_data %>%
    filter(date <= last_obs_ww_date, !is.na(ww)) %>%
    distinct() %>%
    nrow()
  # hospital admissions times
  oht <- train_data %>%
    filter(period != "forecast", !is.na(daily_hosp_admits)) %>%
    select(t) %>%
    distinct() %>%
    nrow()

  message("Removed ", n_ww_data_points - owt, " outliers from WW data")

  n_censored <- train_data %>%
    filter(date <= last_obs_ww_date, !is.na(ww), below_LOD == 1) %>%
    distinct() %>%
    nrow()
  n_uncensored <- owt - n_censored

  # These need to be whatever indices relative to the ww_sampled_times and
  # ww_sampled sites they are so that we subset those
  ww_censored <- train_data %>%
    filter(date <= last_obs_ww_date, !is.na(ww)) %>%
    distinct() %>%
    mutate(ind_rel_to_sampled_times = row_number()) %>%
    filter(below_LOD == 1) %>%
    pull(ind_rel_to_sampled_times)

  ww_uncensored <- train_data %>%
    filter(date <= last_obs_ww_date, !is.na(ww)) %>%
    distinct() %>%
    mutate(ind_rel_to_sampled_times = row_number()) %>%
    filter(below_LOD == 0) %>%
    pull(ind_rel_to_sampled_times)

  stopifnot(
    "Length of censored vectors incorrect" =
      length(ww_censored) + length(ww_uncensored) == owt
  )
  # horizon time (forecast, though this could in reality include a nowcast)
  ht <- nowcast_time + forecast_time

  # vector of obsherved WW times (starting from
  # day 1 = first observed hospital admissions day)
  ww_sampled_times <- train_data %>%
    filter(date <= last_obs_ww_date, !is.na(ww)) %>%
    distinct() %>%
    pull(t)
  # vector of hospitalized times
  hosp_times <- train_data %>%
    filter(period != "forecast", !is.na(daily_hosp_admits)) %>%
    select(t) %>%
    distinct() %>%
    pull(t)
  ww_sampled_sites <- train_data %>%
    filter(date <= last_obs_ww_date, !is.na(ww)) %>%
    distinct() %>%
    select(site_index) %>%
    pull()
  ww_lod <- train_data %>%
    filter(date <= last_obs_ww_date, !is.na(ww)) %>%
    distinct() %>%
    select(lod_sewage) %>%
    pull()
  ww_sampled_lab_sites <- train_data %>%
    filter(date <= last_obs_ww_date, !is.na(ww)) %>%
    distinct() %>%
    select(lab_site_index) %>%
    pull()
  n_ww_sites <- train_data %>%
    select(site_index) %>%
    filter(!is.na(site_index)) %>%
    distinct() %>%
    nrow()
  n_ww_lab_sites <- train_data %>%
    select(lab_site_index) %>%
    filter(!is.na(lab_site_index)) %>%
    distinct() %>%
    nrow()
  lab_site_to_site_map <- train_data %>%
    select(lab_site_index, site_index) %>%
    arrange(lab_site_index, "desc") %>%
    filter(!is.na(lab_site_index), !is.na(site_index)) %>%
    distinct() %>%
    pull(site_index)

  t <- seq(1, ht + ot)
  dates <- seq(
    from = min(train_data$date), to =
      (min(train_data$date) + days(ht + ot - 1)), by = "days"
  )
  day_of_week <- lubridate::wday(dates, week_start = 1)
  n_weeks <- ceiling((ot + ht) / 7)
  tot_weeks <- ceiling((ot + uot + ht) / 7)

  # matrix to transform from weekly to daily
  ind_m <- get_ind_m(ot + ht, n_weeks)

  # matrix to transform p_hosp RW from weekly to daily
  p_hosp_m <- get_ind_m_cum_sum(uot + ot + ht, tot_weeks)

  # Get other variables needed from data
  pop <- train_data %>%
    select(pop) %>%
    unique() %>%
    pull(pop)
  stopifnot(
    "More than one population size in training data" =
      length(pop) == 1
  )

  # Estimate of number of initial infections
  i0 <- mean(train_data$daily_hosp_admits[1:7], na.rm = TRUE) / p_hosp_mean



  # package up parameters for stan data object
  viral_shedding_pars <- c(
    t_peak_mean, t_peak_sd, viral_peak_mean, viral_peak_sd,
    duration_shedding_mean, duration_shedding_sd
  )

  hosp_delay_max <- length(inf_to_hosp)

  hosp_train <- train_data %>%
    filter(period == "calibration", !is.na(daily_hosp_admits)) %>%
    select(date, daily_hosp_admits) %>%
    distinct()
  ww_train <- train_data %>%
    filter(date <= last_obs_ww_date, !is.na(ww)) %>%
    distinct()

  data_renewal <- list(
    gt_max = gt_max,
    hosp_delay_max = hosp_delay_max,
    inf_to_hosp = inf_to_hosp,
    dur_inf = dur_inf,
    mwpd = ml_of_ww_per_person_day,
    ot = ot,
    n_ww_sites = n_ww_sites,
    n_ww_lab_sites = n_ww_lab_sites,
    owt = owt,
    oht = oht,
    n_censored = n_censored,
    n_uncensored = n_uncensored,
    uot = uot,
    ht = ht,
    n_weeks = n_weeks,
    ind_m = ind_m,
    tot_weeks = tot_weeks,
    p_hosp_m = p_hosp_m,
    generation_interval = generation_interval,
    ts = 1:gt_max,
    n = pop,
    ww_sampled_times = ww_sampled_times,
    hosp_times = hosp_times,
    ww_sampled_lab_sites = ww_sampled_lab_sites,
    ww_log_lod = log(ww_lod),
    ww_censored = ww_censored,
    ww_uncensored = ww_uncensored,
    ww_pops = pop_ww,
    hosp = hosp_train$daily_hosp_admits,
    day_of_week = day_of_week,
    log_conc = as.numeric(log(ww_train$ww + 1e-8)),
    compute_likelihood = compute_likelihood,
    include_ww = include_ww,
    include_hosp = include_hosp,
    if_l = length(infection_feedback_pmf),
    infection_feedback_pmf = infection_feedback_pmf,
    # All the priors!
    viral_shedding_pars = viral_shedding_pars, # tpeak, viral peak,
    # duration shedding
    autoreg_rt_a = autoreg_rt_a,
    autoreg_rt_b = autoreg_rt_b,
    inv_sqrt_phi_prior_mean = inv_sqrt_phi_prior_mean,
    inv_sqrt_phi_prior_sd = inv_sqrt_phi_prior_sd,
    r_prior_mean = r_prior_mean,
    r_prior_sd = r_prior_sd,
    log10_g_prior_mean = log10_g_prior_mean,
    log10_g_prior_sd = log10_g_prior_sd,
    log_i0_prior_mean = log(i0 / pop),
    log_i0_prior_sd = 1,
    wday_effect_prior_mean = wday_effect_prior_mean,
    wday_effect_prior_sd = wday_effect_prior_sd,
    initial_growth_prior_mean = initial_growth_prior_mean,
    initial_growth_prior_sd = initial_growth_prior_sd,
    sigma_ww_site_prior_mean_mean = sigma_ww_site_prior_mean_mean,
    sigma_ww_site_prior_mean_sd = sigma_ww_site_prior_mean_sd,
    sigma_ww_site_prior_sd_mean = sigma_ww_site_prior_sd_mean,
    sigma_ww_site_prior_sd_sd = sigma_ww_site_prior_sd_sd,
    eta_sd_sd = eta_sd_sd,
    p_hosp_mean = p_hosp_mean,
    p_hosp_sd_logit = p_hosp_sd_logit,
    p_hosp_w_sd_sd = p_hosp_w_sd_sd,
    ww_site_mod_sd_sd = ww_site_mod_sd_sd,
    infection_feedback_prior_mean = infection_feedback_prior_mean,
    infection_feedback_prior_sd = infection_feedback_prior_sd
  )

  if (model_type == "site-level time-varying concentration") {
    data_renewal <- c(data_renewal,
      autoreg_conc_a = autoreg_conc_a,
      autoreg_conc_b = autoreg_conc_b
    )
  }

  if (model_type == "site-level infection dynamics") {
    data_renewal <- c(data_renewal,
      list(ww_sampled_sites = ww_sampled_sites),
      list(lab_site_to_site_map = lab_site_to_site_map),
      sigma_rt_prior = sigma_rt_prior,
      log_phi_g_prior_mean = log_phi_g_prior_mean,
      log_phi_g_prior_sd = log_phi_g_prior_sd
    )
  }



  return(data_renewal)
}



## Initialization lists to be passed to stan---------------------------------
#' @title Get initialization function for stan for aggregated model
#' @param train_data
#' @param params
#' @param stan_data
#' @return the initialization function
#' @keywords internal
state_agg_inits <- function(train_data, params, stan_data) {
  # Assign parmeter names
  par_names <- colnames(params)
  for (i in seq_along(par_names)) {
    assign(par_names[i], as.double(params[i]))
  }

  # Get other variables needed from data
  pop <- train_data %>%
    select(pop) %>%
    unique() %>%
    pull(pop)
  stopifnot(
    "More than one population size in training data" =
      length(pop) == 1
  )

  n_weeks <- as.numeric(stan_data$n_weeks)
  tot_weeks <- as.numeric(stan_data$tot_weeks)

  # Estimate of number of initial infections
  i0 <- mean(train_data$daily_hosp_admits[1:7], na.rm = TRUE) / p_hosp_mean

  init_list <- list(
    w = rnorm(n_weeks - 1, 0, 0.01),
    eta_sd = abs(rnorm(1, 0, 0.01)),
    autoreg_rt = abs(rnorm(1, autoreg_rt_a / (autoreg_rt_a + autoreg_rt_b), 0.05)),
    log_r = rnorm(1, convert_to_logmean(1, 0.1), convert_to_logsd(1, 0.1)),
    log_i0_over_n = rnorm(1, log(i0 / pop), 0.05),
    initial_growth = rnorm(1, 0, 0.001),
    inv_sqrt_phi_h = 1 / (sqrt(200)) + rnorm(1, 1 / 10000, 1 / 10000),
    sigma_ww = abs(rnorm(1, 0, 0.5)),
    p_hosp_int = rnorm(1, qlogis(p_hosp_mean), 0.01),
    p_hosp_w = rnorm(tot_weeks - 1, 0, 0.01),
    p_hosp_w_sd = abs(rnorm(1, 0.01, 0.001)),
    t_peak = rnorm(1, t_peak_mean, 0.1 * t_peak_sd),
    viral_peak = rnorm(1, viral_peak_mean, 0.1 * viral_peak_sd),
    dur_shed = rnorm(1, duration_shedding_mean, 0.1 * duration_shedding_sd),
    log10_g = rnorm(1, log10_g_prior_mean, 0.5),
    hosp_wday_effect = to_simplex(rnorm(7, 1 / 7, 0.01)),
    infection_feedback = abs(rnorm(
      1, infection_feedback_prior_mean,
      0.1 * infection_feedback_prior_sd
    ))
  )
  return(init_list)
}


#' @title Get initialization function for stan for time-varying concentration model
#' @param train_data
#' @param params
#' @param stan_data
#' @return the initialization function
#' @keywords internal
time_varying_conc_inits <- function(train_data, params, stan_data) {
  par_names <- colnames(params)
  for (i in seq_along(par_names)) {
    assign(par_names[i], as.double(params[i]))
  }


  pop <- train_data %>%
    select(pop) %>%
    unique() %>%
    pull(pop)
  stopifnot("More than one population size in training data" = length(pop) == 1)

  n_weeks <- as.numeric(stan_data$n_weeks)
  tot_weeks <- as.numeric(stan_data$tot_weeks)
  n_ww_lab_sites <- as.numeric(stan_data$n_ww_lab_sites)

  # Estimate of number of initial infections
  i0 <- mean(train_data$daily_hosp_admits[1:7], na.rm = TRUE) / p_hosp_mean

  init_list <- list(
    w = rnorm(n_weeks - 1, 0, 0.01),
    eta_sd = abs(rnorm(1, 0, 0.01)),
    sigma_log_conc = abs(rnorm(1, 2, 0.5)),
    autoreg_rt = abs(rnorm(1, autoreg_rt_a / (autoreg_rt_a + autoreg_rt_b), 0.05)),
    autoreg_conc = abs(rnorm(1, autoreg_conc_a / (autoreg_conc_a + autoreg_conc_b), 0.05)),
    log_r = rnorm(1, convert_to_logmean(1, 0.1), convert_to_logsd(1, 0.1)),
    log_i0_over_n = rnorm(1, log(i0 / pop), 0.05),
    initial_growth = rnorm(1, 0, 0.001),
    inv_sqrt_phi_h = 1 / sqrt(200) + rnorm(1, 1 / 10000, 1 / 10000),
    sigma_ww_site_mean = abs(rnorm(
      1, sigma_ww_site_prior_mean_mean,
      0.1 * sigma_ww_site_prior_mean_sd
    )),
    sigma_ww_site_sd = abs(rnorm(
      1, sigma_ww_site_prior_sd_mean,
      0.1 * sigma_ww_site_prior_sd_sd
    )),
    error_conc_site = matrix(
      rnorm(n_ww_lab_sites * (stan_data$ot + stan_data$ht),
        mean = 0, sd = 0.1
      ),
      n_ww_lab_sites, (stan_data$ot + stan_data$ht)
    ),
    sigma_ww_site_raw = as.array(abs(rnorm(n_ww_lab_sites, 0, 0.05))),
    p_hosp_int = rnorm(1, qlogis(p_hosp_mean), 0.01),
    p_hosp_w = rnorm(tot_weeks - 1, 0, 0.01),
    p_hosp_w_sd = abs(rnorm(1, 0.01, 0.001)),
    t_peak = rnorm(1, t_peak_mean, 0.1 * t_peak_sd),
    viral_peak = rnorm(1, viral_peak_mean, 0.1 * viral_peak_sd),
    dur_shed = rnorm(1, duration_shedding_mean, 0.1 * duration_shedding_sd),
    log10_g = rnorm(1, log10_g_prior_mean, 0.5),
    ww_site_mod_raw = as.array(abs(rnorm(n_ww_lab_sites, 0, 0.3))),
    ww_site_mod_sd = abs(rnorm(1, 0, 0.05)),
    hosp_wday_effect = to_simplex(rnorm(7, 1 / 7, 0.01)),
    infection_feedback = abs(rnorm(1, 750, 50))
  )
  return(init_list)
}


#' @title Get initialization function for stan for site-level observation error model
#' @param train_data
#' @param params
#' @param stan_data
#' @return the initialization function
#' @keywords internal
site_level_obs_inits <- function(train_data, params, stan_data) {
  par_names <- colnames(params)
  for (i in seq_along(par_names)) {
    assign(par_names[i], as.double(params[i]))
  }


  pop <- train_data %>%
    select(pop) %>%
    unique() %>%
    pull(pop)
  stopifnot("More than one population size in training data" = length(pop) == 1)

  n_weeks <- as.numeric(stan_data$n_weeks)
  tot_weeks <- as.numeric(stan_data$tot_weeks)
  n_ww_lab_sites <- as.numeric(stan_data$n_ww_lab_sites)

  # Estimate of number of initial infections
  i0 <- mean(train_data$daily_hosp_admits[1:7], na.rm = TRUE) / p_hosp_mean

  init_list <- list(
    w = rnorm(n_weeks - 1, 0, 0.01),
    eta_sd = abs(rnorm(1, 0, 0.01)),
    autoreg_rt = abs(rnorm(1, autoreg_rt_a / (autoreg_rt_a + autoreg_rt_b), 0.05)),
    log_r = rnorm(1, convert_to_logmean(1, 0.1), convert_to_logsd(1, 0.1)),
    log_i0_over_n = rnorm(1, log(i0 / pop), 0.05),
    initial_growth = rnorm(1, 0, 0.001),
    inv_sqrt_phi_h = 1 / sqrt(200) + rnorm(1, 1 / 10000, 1 / 10000),
    sigma_ww_site_mean = abs(rnorm(
      1, sigma_ww_site_prior_mean_mean,
      0.1 * sigma_ww_site_prior_mean_sd
    )),
    sigma_ww_site_sd = abs(rnorm(
      1, sigma_ww_site_prior_sd_mean,
      0.1 * sigma_ww_site_prior_sd_sd
    )),
    sigma_ww_site_raw = as.array(abs(rnorm(n_ww_lab_sites, 0, 0.05)),
      dim = n_ww_lab_sites
    ),
    p_hosp_int = rnorm(1, qlogis(p_hosp_mean), 0.01),
    p_hosp_w = rnorm(tot_weeks - 1, 0, 0.01),
    p_hosp_w_sd = abs(rnorm(1, 0.01, 0.001)),
    t_peak = rnorm(1, t_peak_mean, 0.1 * t_peak_sd),
    viral_peak = rnorm(1, viral_peak_mean, 0.1 * viral_peak_sd),
    dur_shed = rnorm(1, duration_shedding_mean, 0.1 * duration_shedding_sd),
    log10_g = rnorm(1, log10_g_prior_mean, 0.5),
    ww_site_mod_raw = as.array(abs(rnorm(n_ww_lab_sites, 0, 0.3))),
    ww_site_mod_sd = abs(rnorm(1, 0, 0.05)),
    hosp_wday_effect = to_simplex(rnorm(7, 1 / 7, 0.01)),
    infection_feedback = abs(rnorm(1, 750, 50))
  )
  return(init_list)
}

#' @title Get initialization function for stan for site-level infection model
#' @param train_data
#' @param params
#' @param stan_data
#' @return the initialization function
#' @keywords internal
site_level_inf_inits <- function(train_data, params, stan_data) {
  # Assign parmeter names
  par_names <- colnames(params)
  for (i in seq_along(par_names)) {
    assign(par_names[i], as.double(params[i]))
  }

  # Get other variables needed from data
  pop <- train_data %>%
    select(pop) %>%
    unique() %>%
    pull(pop)
  stopifnot("More than one population size in training data" = length(pop) == 1)

  n_weeks <- as.numeric(stan_data$n_weeks)
  tot_weeks <- as.numeric(stan_data$tot_weeks)
  n_ww_sites <- as.numeric(stan_data$n_ww_sites)
  n_ww_lab_sites <- as.numeric(stan_data$n_ww_lab_sites)
  ot <- as.numeric(stan_data$ot)
  ht <- as.numeric(stan_data$ht)

  # Estimate of number of initial infections
  i0 <- mean(train_data$daily_hosp_admits[1:7], na.rm = TRUE) / p_hosp_mean


  init_list <- list(
    w = rnorm(n_weeks - 1, 0, 0.01),
    eta_sd = abs(rnorm(1, 0, 0.01)),
    eta_i0 = abs(rnorm(n_ww_sites, 0, 0.01)),
    sigma_i0 = abs(rnorm(1, 0, 0.01)),
    eta_growth = abs(rnorm(n_ww_sites, 0, 0.01)),
    sigma_growth = abs(rnorm(1, 0, 0.01)),
    autoreg_rt = abs(rnorm(1, autoreg_rt_a / (autoreg_rt_a + autoreg_rt_b), 0.05)),
    log_r_mu_intercept = rnorm(1, convert_to_logmean(1, 0.1), convert_to_logsd(1, 0.1)),
    error_site = matrix(
      rnorm(n_ww_sites * n_weeks, mean = 0, sd = 0.1),
      n_ww_sites, n_weeks
    ),
    autoreg_rt_site = abs(rnorm(1, 0.5, 0.05)),
    sigma_rt = abs(rnorm(1, 0, 0.01)),
    log_i0_over_n = rnorm(1, log(i0 / pop), 0.05),
    initial_growth = rnorm(1, 0, 0.001),
    inv_sqrt_phi_h = 1 / sqrt(200) + rnorm(1, 1 / 10000, 1 / 10000),
    sigma_ww_site_mean = abs(rnorm(
      1, sigma_ww_site_prior_mean_mean,
      0.1 * sigma_ww_site_prior_mean_sd
    )),
    sigma_ww_site_sd = abs(rnorm(
      1, sigma_ww_site_prior_sd_mean,
      0.1 * sigma_ww_site_prior_sd_sd
    )),
    sigma_ww_site_raw = abs(rnorm(n_ww_lab_sites, 0, 0.05)),
    p_hosp_int = rnorm(1, qlogis(p_hosp_mean), 0.01),
    p_hosp_w = rnorm(tot_weeks - 1, 0, 0.01),
    p_hosp_w_sd = abs(rnorm(1, 0.01, 0.001)),
    t_peak = rnorm(1, t_peak_mean, 0.1 * t_peak_sd),
    viral_peak = rnorm(1, viral_peak_mean, 0.1 * viral_peak_sd),
    dur_shed = rnorm(1, duration_shedding_mean, 0.1 * duration_shedding_sd),
    log10_g = rnorm(1, log10_g_prior_mean, 0.5),
    ww_site_mod_raw = abs(rnorm(n_ww_lab_sites, 0, 0.05)),
    ww_site_mod_sd = abs(rnorm(1, 0, 0.05)),
    hosp_wday_effect = to_simplex(rnorm(7, 1 / 7, 0.01)),
    infection_feedback = abs(rnorm(
      1, infection_feedback_prior_mean,
      0.5 * infection_feedback_prior_sd
    ))
  )


  return(init_list)
}
#' Get shortened name for writing a file with model file name
#'
#' @param model_type
#'
#' @return shortened model type name
#' @export
#'
#' @examples
get_model_file_name <- function(model_type, include_ww) {
  model_file_name <- case_when(
    (model_type == "state-level aggregated wastewater" && include_ww == 0) ~ "hosp_only",
    (model_type == "state-level aggregated wastewater" && include_ww == 1) ~ "state_agg_WW_hosp",
    model_type == "hospital admissions only" ~ "hosp_only",
    model_type == "site-level observation error" ~ "site_level_obs_error",
    model_type == "site-level infection dynamics" ~ "site_level_inf_dyn",
    model_type == "site-level time-varying concentration" ~ "site_level_time_varying_C"
  )
  return(model_file_name)
}


#' @title Get path to stan wastewater model
#' @param model_type
#' @return the file path
#' @export
get_model_file_path <- function(model_type) {
  if (model_type %in% c(
    "state-level aggregated wastewater", "hospital admissions only"
  )) {
    model_file_path <- system.file(
      "stan",
      "renewal_ww_hosp.stan",
      package = "cfaforecastrenewalww"
    )
  } else if (model_type == "site-level observation error") {
    model_file_path <- system.file(
      "stan",
      "renewal_ww_hosp_site_level_phi.stan",
      package = "cfaforecastrenewalww"
    )
  } else if (model_type == "site-level time-varying concentration") {
    model_file_path <- system.file(
      "stan",
      "renewal_ww_hosp_site_level_phi_varying_C.stan",
      package = "cfaforecastrenewalww"
    )
  } else if (model_type == "site-level infection dynamics") {
    model_file_path <- system.file("stan",
      "renewal_ww_hosp_site_level_inf_dynamics.stan",
      package = "cfaforecastrenewalww"
    )
  } else {
    model_file_path <- "Error"
  }
  stopifnot("Model type not specified correctly" = model_file_path != "Error")


  return(model_file_path)
}

#' @title Get initialization function for stan
#' @param model_type
#' @param train_data
#' @param params
#' @param stan_data
#' @return the initialization function
#' @export
get_init_fun <- function(model_type, train_data, params, stan_data) {
  if (model_type == "site-level time-varying concentration") {
    init_fun <- function() {
      time_varying_conc_inits(train_data, params, stan_data)
    }
  } else if (model_type == "site-level observation error") {
    init_fun <- function() {
      site_level_obs_inits(train_data, params, stan_data)
    }
  } else if (model_type == "site-level infection dynamics") {
    init_fun <- function() {
      site_level_inf_inits(train_data, params, stan_data)
    }
  } else {
    message("Model type not specified properly")
  }

  return(init_fun)
}

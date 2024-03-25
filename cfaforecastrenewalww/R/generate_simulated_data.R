#' Generate simulated data from the underlying model's generative process
#' @description
#' Function that allows the user to generate hospital admissions and site-level
#' wastewater data directly from the generative model, specifying the conditions
#' and parameters to generate from.
#'
#' @param site_level_inf_dynamics if TRUE then the toy data has variation in the
#' site-level R(t), if FALSE, assumes same underlying R(t) for the state as in
#' each site
#' @param site_level_conc_dynamics if TRUE then the toy data has variation in the
#' site-level concentration each day, if FALSE, then the relationship from infection
#' to concentration in each site is the same across sites
#' @param r_in_weeks The mean weekly R(t) that drives infection dynamics at the state-
#' level. This gets jittered with random noise to add week-to-week variation.
#' @param n_sites Number of sites
#' @param ww_pop_sites Catchment area in each of those sites (order must match)
#' @param pop_size Population size in the state
#' @param n_lab_sites NUmber of unique combinations of labs and sites. Must be
#' greater than or equal to `n_sites`
#' @param map_site_to_lab Vector mapping the sites to the lab-sites in order
#' of the sites
#' @param ot observed time: length of hospital admissions calibration time in days
#' @param nt nowcast time: length of time between last hospital admissions date
#' and forecast date in days
#' @param forecast_time duration of the forecast in days e.g. 28 days
#' @param sim_start_date the start date of the simulation, used to get a weekday
#' vector
#' @param hosp_wday_effect a simplex of length 7 describing how the hospital
#' admissions are spread out over a week, starting at Monday = 1
#' @param i0_over_n the initial per capita infections in the state
#' @param initial_growth exponential growth rate during the unobserved time
#' @param sd_in_lab_level_multiplier standard deviation in the log of the site-
#' lab level multiplier determining how much variation there is systematically
#' in site-labs from the state mean
#' @param mean_obs_error_in_ww_lab_site mean day to day variation in observed
#' wastewater concentrations across all lab-sites
#' @param mean_reporting_freq mean frequency of wastewater measurements across
#'  sites in per day (e.g. 1/7 is once per week)
#' @param sd_reporting_freq standard deviation in the frequency of wastewater
#' measurements across sites
#' @param mean_reporting_latency mean time from forecast date to last
#' wastewater sample collection date, across sites
#' @param sd_reporting_latency standard deviation in the time from the forecast
#' date to the last wastewater sample collection date, across sites
#' @param mean_log_lod mean log of the LOD in each lab-site
#' @param sd_log_lod standard deviation in the log of the LOD across sites
#' @param example_params_path path to the toml file with the parameters to use to
#' generate the simulated data
#'
#' @return a list containing two dataframes. example_df is a dataframe containing
#' all the columns needed to get the stan data needed for the infection dynamics
#' model. It contains values for every site-lab-day combination, with NAs
#' when the wastewater concentrations aren't observed. Hospital admissions
#' are therefore repeated N site-lab times. param_df is a single row data
#' frame of all the static parameters used to generate the model
#' @export
#'
#' @examples
generate_simulated_data <- function(site_level_inf_dynamics = TRUE,
                                    site_level_conc_dynamics = FALSE,
                                    r_in_weeks = c(
                                      rep(1.1, 5), rep(0.9, 5),
                                      1 + 0.007 * 1:16
                                    ),
                                    n_sites = 4,
                                    ww_pop_sites = c(4e5, 2e5, 1e5, 5e4),
                                    pop_size = 1e6,
                                    n_lab_sites = 5,
                                    map_site_to_lab = c(1, 1, 2, 3, 4),
                                    ot = 90,
                                    nt = 9,
                                    forecast_time = 28,
                                    sim_start_date = ymd("2023-10-30"),
                                    hosp_wday_effect = c(
                                      0.95, 1.01, 1.02,
                                      1.02, 1.01, 1,
                                      0.99
                                    ) / 7,
                                    i0_over_n = 5e-4,
                                    initial_growth = 1e-4,
                                    sd_in_lab_level_multiplier = 0.25,
                                    mean_obs_error_in_ww_lab_site = 0.3,
                                    mean_reporting_freq = 1 / 7,
                                    sd_reporting_freq = 1 / 14,
                                    mean_reporting_latency = 7,
                                    sd_reporting_latency = 5,
                                    mean_log_lod = 3.8,
                                    sd_log_lod = 0.2,
                                    example_params_path =
                                      fs::path_package("extdata", "example_params.toml",
                                        package = "cfaforecastrenewalww"
                                      )) {
  stopifnot(
    "weekly R(t) passed in isn't long enough" =
      length(r_in_weeks) >= (ot + nt + forecast_time) / 7
  )
  stopifnot(
    "Sum of wastewater site populations is greater than state pop" =
      pop_size > sum(ww_pop_sites)
  )

  if (length(ww_pop_sites) < n_sites) {
    ww_pop_sites <- rnorm(n_sites,
      mean = (0.7 * pop_size / n_sites),
      sd = 0.1 * (0.7 * pop_size / n_sites)
    )
  }
  if (n_lab_sites < n_sites) {
    n_lab_sites <- n_sites
    map_site_to_lab <- 1:n_sites
  }

  # Get pop fractrions of each subpop
  pop_fraction <- c(ww_pop_sites / pop_size, (pop_size - sum(ww_pop_sites)) / pop_size)

  # Expose the stan functions (can use any of the models here)
  model <- cmdstan_model(
    stan_file = system.file(
      "stan", "renewal_ww_hosp.stan",
      package = "cfaforecastrenewalww"
    ),
    compile = TRUE,
    compile_standalone = TRUE,
    force_recompile = TRUE
  )

  model$expose_functions(global = TRUE)
  params <- get_params(example_params_path) # load in a data table with parameters
  par_names <- colnames(params) # pull them into memory
  for (i in seq_along(par_names)) {
    assign(par_names[i], as.double(params[i]))
  }

  site_lab_map <- data.frame(lab_site = 1:n_lab_sites, site = map_site_to_lab) %>%
    left_join(data.frame(site = 1:n_sites, ww_pop = ww_pop_sites))

  ht <- nt + forecast_time
  n_weeks <- ceiling((ot + ht) / 7)
  tot_weeks <- ceiling((uot + ot + ht) / 7)
  # We need dates to get a weekday vector
  dates <- seq(
    from = sim_start_date, to =
      (sim_start_date + days(ot + nt + ht - 1)), by = "days"
  )
  log_i0_over_n <- log(i0_over_n)
  day_of_week_vector <- lubridate::wday(dates, week_start = 1)
  date_df <- data.frame(
    t = 1:(ot + nt + ht),
    date = dates
  )
  forecast_date <- date_df %>%
    filter(t == ot + nt) %>%
    pull(date)
  # set the lab-site multiplier presumably from lab measurement processes
  log_m_lab_sites <- rnorm(n_lab_sites,
    mean = 0, sd = sd_in_lab_level_multiplier
  )
  # Assign a site level observation error to each site, but have it scale
  # inversely with the catchment area of the site (this may not be the right
  # scaling)
  sigma_ww_lab_site <- mean(site_lab_map$ww_pop) *
    mean_obs_error_in_ww_lab_site / site_lab_map$ww_pop
  # Set randomly the lab-site reporting avg frequency (per day) and the
  # reporting latency (in days). Will use this to sample times in the observed
  # data
  lab_site_reporting_freq <- abs(rnorm(
    n = n_lab_sites, mean = mean_reporting_freq,
    sd = sd_reporting_freq
  ))
  lab_site_reporting_latency <- pmax(1, ceiling(rnorm(
    n = n_lab_sites,
    mean = mean_reporting_latency, sd = sd_reporting_latency
  )))
  # Set a lab-site-specific LOD in log scale
  lod_lab_site <- rnorm(n_lab_sites, mean = mean_log_lod, sd = sd_log_lod)

  ## Delay distributions----------------------------------------------------
  generation_interval <- simulate_double_censored_pmf(
    max = gt_max, meanlog = mu_gi, sdlog = sigma_gi, fun_dist = rlnorm, n = 5e6
  ) %>% drop_first_and_renormalize()



  # Set infection feedback to generation interval
  infection_feedback_pmf <- generation_interval
  infection_feedback_rev_pmf <- rev(infection_feedback_pmf)
  infection_feedback <- 0
  if_feedback <- 1
  # Delay from infection to hospital admission: incubation period +
  # time from symptom onset to hospital admission
  inc <- make_incubation_period_pmf(
    backward_scale, backward_shape, r
  )
  sym_to_hosp <- make_hospital_onset_delay_pmf(neg_binom_mu, neg_binom_size)
  inf_to_hosp <- make_reporting_delay_pmf(inc, sym_to_hosp)
  # shedding kinetics delay distribution
  vl_trajectory <- model$functions$get_vl_trajectory(
    t_peak_mean, viral_peak_mean,
    duration_shedding_mean, gt_max
  )

  # Generate the state level weekly R(t) before infection feedback-------------
  unadj_r_weeks <- (r_in_weeks * rnorm(length(r_in_weeks), 1, 0.03))[1:n_weeks]
  # Convert to daily for input into renewal equation
  ind_m <- get_ind_m(ot + ht, n_weeks)
  unadj_r <- ind_m %*% unadj_r_weeks


  # Generate the site-level expected observed concentrations -----------------
  # first by adding
  # variation to the site-level R(t) in each site, and then by adding time varying
  # deviations in true concentration in each site, and then adding a site level
  # true variability, and then adding lab-site level multiplier and obersvation
  # error


  ### Generate the site level infection dynamics----------------------------------
  new_i_over_n_site <- matrix(nrow = n_sites + 1, ncol = (uot + ot + ht))
  r_site <- matrix(nrow = n_sites + 1, ncol = (ot + ht))
  # Generate site-level R(t)
  if (isTRUE(site_level_inf_dynamics)) {
    log_r_state_week <- log(unadj_r_weeks)
    log_r_site <- matrix(nrow = n_sites + 1, ncol = n_weeks)
    initial_growth_site <- vector(length = n_sites + 1)
    log_i0_over_n_site <- vector(length = n_sites + 1)
    for (i in 1:(n_sites + 1)) {
      if (i <= n_sites) {
        log_r_site[i, ] <- rnorm(
          n = n_weeks,
          mean = log_r_state_week,
          sd = 0.05
        ) # sigma_rt
        initial_growth_site[i] <- rnorm(
          n = 1, mean = initial_growth,
          sd = initial_growth_prior_sd
        )
        log_i0_over_n_site[i] <- rnorm(
          n = 1, mean = log_i0_over_n,
          sd = 0.5
        )
      } else {
        log_r_site[i, ] <- log_r_state_week
        initial_growth_site[i] <- initial_growth
        log_i0_over_n_site[i] <- log_i0_over_n
      }
    }

    new_i_over_n <- rep(0, (uot + ot + ht))
    for (i in 1:(n_sites + 1)) {
      unadj_r_site <- ind_m %*% exp(log_r_site[i, ]) # daily R site
      site_output <- model$functions$generate_infections(
        unadj_r_site, uot, rev(generation_interval), log_i0_over_n_site[i],
        initial_growth_site[i], ht,
        infection_feedback, infection_feedback_rev_pmf
      )
      new_i_over_n_site[i, ] <- site_output[[1]]
      new_i_over_n <- new_i_over_n + pop_fraction[i] * site_output[[1]]
      r_site[i, ] <- site_output[[2]]
    }
  } else { # site level R(t) and infections = state level R(t) and infections
    for (i in 1:n_sites) {
      new_i_over_n_site[i, ] <- new_i_over_n
      r_site[i, ] <- rt
    }
  }

  rt <- (new_i_over_n / model$functions$convolve_dot_product(
    new_i_over_n, rev(generation_interval), uot + ot + ht
  ))[(uot + 1):(uot + ot + ht)]


  # Generate expected state level hospitalizations from subpop incident infections-----
  # generate state-level incident infections using renewal equation for
  # all time points

  # Generate a time varying P(hosp|infection),
  p_hosp_int_logit <- qlogis(p_hosp_mean) # p_hosp_mean is declared in linear scale
  p_hosp_m <- get_ind_m(uot + ot + ht, tot_weeks) # matrix needed to convert
  # from weekly to daily
  p_hosp_w_logit <- p_hosp_int_logit + rnorm(
    tot_weeks - 1, 0,
    p_hosp_w_sd_sd
  )
  # random walk on p_hosp in logit scale
  p_hosp_logit_weeks <- c(
    p_hosp_int_logit,
    p_hosp_w_logit
  ) # combine with intercept
  p_hosp_logit_days <- p_hosp_m %*% c(
    p_hosp_int_logit,
    p_hosp_w_logit
  ) # convert to days
  p_hosp_days <- plogis(p_hosp_logit_days) # convert back to linear scale
  # Corresponds to a standard deviation in linear scale of 0.0003

  # Get expected trajectory of hospital admissions from incident infections
  # by convolving scaled incident infections with delay from infection to
  # hospital admission
  model_hosp_over_n <- model$functions$convolve_dot_product(
    p_hosp_days * new_i_over_n,
    rev(inf_to_hosp),
    uot + ot + ht
  )[(uot + 1):(uot + ot + ht)]
  exp_hosp <- pop_size * model$functions$day_of_week_effect(
    model_hosp_over_n,
    day_of_week_vector,
    hosp_wday_effect
  )
  # Add observation error, get hospital admissions in the forecast period
  exp_obs_hosp <- rnbinom(
    n = length(exp_hosp), mu = exp_hosp,
    size = 1 / ((inv_sqrt_phi_prior_mean)^2)
  )



  ## Generate site-level mean genomes from infections in each site-------
  log_g_over_n_site <- matrix(nrow = n_sites, ncol = (ot + ht))

  for (i in 1:n_sites) {
    model_net_i <- model$functions$convolve_dot_product(
      new_i_over_n_site[i, ],
      rev(vl_trajectory),
      (uot + ot + ht)
    )[(uot + 1):(uot + ot + ht)]
    log_g_over_n_site[i, ] <- log(10) * log10_g_prior_mean +
      log(model_net_i + 1e-8)
  }

  ## Generate site-level true genomes ------------------------------------
  # with site multiplier and time-varying deviation

  log_exp_g_over_n_site <- matrix(nrow = n_sites, ncol = (ot + ht))

  for (i in 1:n_sites) {
    if (isFALSE(site_level_conc_dynamics)) {
      log_exp_g_over_n_site[i, ] <- log_g_over_n_site[i, ]
    } else {
      log_exp_g_over_n_site[i, ] <- log_g_over_n_site[i, ] +
        rnorm(
          n = (ot + ht), mean = 0,
          sd = 0.01
        ) # sigma_log_conc prior
    }
  }

  # Add on site-lab-level observation error --------------------------------------
  log_obs_g_over_n_lab_site <- matrix(nrow = n_lab_sites, ncol = (ot + ht))
  for (i in 1:n_lab_sites) {
    log_g_w_multiplier <- log_exp_g_over_n_site[map_site_to_lab[i], ] +
      log_m_lab_sites[i]
    log_obs_g_over_n_lab_site[i, ] <- log_g_w_multiplier +
      rnorm(
        n = (ot + ht), mean = 0,
        sd = sigma_ww_lab_site[i]
      )
  }

  # Sample from some lab-sites more frequently than others and add different
  # latencies for each lab-site
  log_obs_conc_lab_site <- matrix(nrow = n_lab_sites, ncol = ot + ht)
  for (i in 1:n_lab_sites) {
    st <- sample(1:(ot + nt), round((ot + nt) * lab_site_reporting_freq[i]))
    stl <- pmin((ot + nt - lab_site_reporting_latency[i]), st)
    log_obs_conc_lab_site[i, stl] <- log_obs_g_over_n_lab_site[i, stl] -
      log(ml_of_ww_per_person_day)
  }

  # Format the data-----------------------------------------------------------

  df_long <- as.data.frame(t(log_obs_conc_lab_site)) %>%
    dplyr::mutate(t = 1:(ot + ht)) %>%
    tidyr::pivot_longer(!t,
      names_to = "lab_wwtp_unique_id",
      names_prefix = "V",
      values_to = "log_conc"
    ) %>%
    dplyr::mutate(
      lab_wwtp_unique_id = as.integer(lab_wwtp_unique_id)
    ) %>%
    dplyr::left_join(date_df, by = "t") %>%
    dplyr::left_join(
      data.frame(
        lab_site = 1:n_lab_sites,
        lod_sewage = lod_lab_site
      ),
      by = c("lab_wwtp_unique_id" = "lab_site")
    ) %>%
    dplyr::mutate(below_LOD = ifelse(log_conc >= lod_sewage, 0, 1)) %>%
    dplyr::mutate(lod_sewage = case_when(
      is.na(log_conc) ~ NA,
      !is.na(log_conc) ~ lod_sewage
    ))

  # Make a hospital admissions dataframe to bind to
  df_hosp <- data.frame(
    t = 1:(ot + ht),
    daily_hosp_admits = c(exp_obs_hosp[1:ot], rep(NA, ht)),
    daily_hosp_admits_for_eval = exp_obs_hosp
  )

  # State infections per capita
  df_inf <- data.frame(
    t = 1:(ot + ht),
    inf_per_capita = new_i_over_n[(uot + 1):(uot + ot + ht)]
  )

  example_df <- df_long %>%
    dplyr::left_join(df_hosp,
      by = "t"
    ) %>%
    dplyr::mutate(
      pop = pop_size,
      forecast_date = forecast_date,
      hosp_calibration_time = ot
    ) %>%
    dplyr::left_join(site_lab_map,
      by = c("lab_wwtp_unique_id" = "lab_site")
    ) %>%
    dplyr::left_join(df_inf,
      by = "t"
    )



  # Get the true parameter dataframe, making sure this is formatted the same as
  # as the output from get_full_param_distrib()
  p_hosp_df <- data.frame(
    name = "p_hosp", true_value = p_hosp_days,
    index_rows = NA,
    index_cols = seq_along(p_hosp_days)
  )
  r_df <- data.frame(
    name = "rt", true_value = rt,
    index_rows = NA,
    index_cols = seq_along(rt)
  )
  log10_g_df <- data.frame(
    name = "log10_g", true_value = log10_g_prior_mean,
    index_rows = NA,
    index_cols = NA
  )

  param_df <- rbind(p_hosp_df, r_df, log10_g_df)

  toy_data_and_params <- list(
    param_df = param_df,
    example_df = example_df
  )
  return(toy_data_and_params)
}

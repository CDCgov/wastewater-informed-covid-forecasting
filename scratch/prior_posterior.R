devtools::load_all('wweval')
targets::tar_load('params')

to_plot <- list(
  list(
    "name" = "log10 genomes shed per infected individual",
    "symbol" = "$\log_{10}(G)$",
    "stan_name" = "log10_g",
    "prior_pdf" = purrr::partial(
      dnorm,
      mean = params$log10_g_prior_mean,
      sd = params$log10_g_prior_sd
    )
  ),
  list(
    "name" = "infection feedback strength",
    "symbol" = "$\gamma$",
    "stan_name" = "infection_feedback",
    "prior_pdf" = purrr::partial(
      dlnorm,
      meanlog = params$infection_feedback_prior_logmean,
      sdlog = params$infection_feedback_prior_logsd
    )
  ),
  list(
    name = "Observation noise sd variability by site",
    symbol = "$\sigma_{\log \sigma_{c}}$",
    stan_name = "sd_log_sigma_ww_site",
    prior_pdf = purrr::partial(
      dnorm,
      mean = params$sd_log_sigma_ww_site_prior_mode,
      sd = params$sd_log_sigma_ww_site_prior_sd
    )
  ),
  list(
    "name" = "sd of log site-lab multipliers",
    "symbol" = "$\sigma_m$",
    "stan_name" = "ww_site_mod_sd",
    prior_pdf = purrr::partial(
      dnorm,
      mean = 0,
      sd = params$ww_site_mod_sd_sd
    )
  ),
  list(
    "name" = "s.d. of the AR(1) process on the R(t) differences",
    "symbol" = "$\sigma_r$",
    "stan_name" = "eta_sd",
    prior_pdf = purrr:partial(
      dnorm,
      mean = params$eta_sd_mean,
      sd = params$eta_sd_sd
    )
  )
)

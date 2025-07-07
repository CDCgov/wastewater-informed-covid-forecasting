#' Fit a log-linear trend in recent data

library(argparser)
options(mc.cores = 4)

fit_trend <- function(
  forecast_date,
  location,
  scenario,
  raw_output_dir,
  processed_output_dir,
  hosp_prior_params,
  ww_prior_params,
  hosp_lookback_days,
  ww_lookback_days,
  seed,
  control,
  chains,
  iter_warmup,
  iter_sampling
) {
  fits <- wweval::fit_recent_trends(
    forecast_date = forecast_date,
    location = location,
    scenario = scenario,
    raw_output_dir = raw_output_dir,
    hosp_prior_params = hosp_prior_params,
    ww_prior_params = ww_prior_params,
    hosp_lookback_days = hosp_lookback_days,
    ww_lookback_days = ww_lookback_days,
    seed = seed,
    control = control,
    iter = iter_warmup + iter_sampling,
    warmup = iter_warmup,
    chains = chains
  )

  hosp_fit <- fits$hosp
  ww_fit <- fits$ww

  wweval::process_recent_trend_fits(
    hosp_fit = hosp_fit,
    ww_fit = ww_fit,
    forecast_date = forecast_date,
    location = location,
    scenario = scenario,
    processed_output_dir = processed_output_dir,
    figure_ext = "pdf"
  )
}


parsed <- arg_parser(
  "Infer log-linear trends in admissions and wastewater data using BRMS"
) |>
  add_argument(
    "as_of_date",
    help = "As-of date, typically for a corresponding forecast."
  ) |>
  add_argument(
    "location",
    help = "Location to forecast."
  ) |>
  add_argument(
    "scenario",
    help = "Wastewater data availability scenario to analyze."
  ) |>
  add_argument(
    "hosp_lookback_days",
    help = "Number of days of hospital admissions data to fit",
    type = "integer"
  ) |>
  add_argument(
    "ww_lookback_days",
    help = "number of days of wastewater data to fit",
    type = "integer"
  ) |>
  add_argument(
    "raw_output_dir",
    help = paste0(
      "Path to a directory in which to save ",
      "raw output."
    )
  ) |>
  add_argument(
    "processed_output_dir",
    help = paste0(
      "Path to a directory in which to save ",
      "processed output."
    )
  ) |>
  add_argument(
    "seed",
    help = "Seed for Stan's pseudorandom number generator.",
    type = "integer"
  ) |>
  add_argument(
    "--iter-warmup",
    help = "Number of warmup iterations to use for each MCMC chain.",
    default = 1000L,
    type = "integer"
  ) |>
  add_argument(
    "--iter-sampling",
    help = "Number of samples to draw from each MCMC chain.",
    default = 500L,
    type = "integer"
  ) |>
  add_argument(
    "--n-chains",
    help = "Number of MCMC chains to run.",
    default = 4L,
    type = "integer"
  ) |>
  add_argument(
    "--adapt-delta",
    help = paste0(
      "Target acceptance probability for the No-U-Turn ",
      "sampler adaptation phase."
    ),
    default = 0.8
  ) |>
  add_argument(
    "--max-treedepth",
    help = "Maximum tree depth for the No-U-Turn sampler.",
    default = 10L,
    type = "integer"
  ) |>
  parse_args()

message(glue::glue(
  "Starting a trendfit task for location {parsed$location} ",
  "and as-of date {parsed$as_of_date}"
))

hosp_prior_params <- list(
  exp_rate_hosp_prior_mean = 0,
  exp_rate_hosp_prior_sd = 0.06,
  log_nb_conc_prior_mean = log(10),
  log_nb_conc_prior_sd = log(10)
)

ww_prior_params <- list(
  exp_rate_conc_pop_prior_mean = 0,
  exp_rate_conc_pop_prior_sd = 0.06,
  sd_exp_rate_conc_prior_mode = 0,
  sd_exp_rate_conc_prior_sd = 0.03,
  t0_conc_pop_prior_mean = 7,
  t0_conc_pop_prior_sd = 4,
  sd_t0_conc_prior_mode = 0,
  sd_t0_conc_prior_sd = 1,
  log_sd_obs_pop_prior_mean = log(1),
  log_sd_obs_pop_prior_sd = log(5),
  sd_log_sd_obs_prior_mode = 0,
  sd_log_sd_obs_prior_sd = log(1.5)
)

fit_trend(
  forecast_date = parsed$as_of_date,
  location = parsed$location,
  scenario = parsed$scenario,
  raw_output_dir = parsed$raw_output_dir,
  processed_output_dir = parsed$processed_output_dir,
  hosp_prior_params = hosp_prior_params,
  ww_prior_params = ww_prior_params,
  hosp_lookback_days = parsed$hosp_lookback_days,
  ww_lookback_days = parsed$ww_lookback_days,
  seed = parsed$seed,
  control = list(
    adapt_delta = parsed$adapt_delta,
    max_treedepth = parsed$max_treedepth
  ),
  chains = parsed$n_chains,
  iter_warmup = parsed$iter_warmup,
  iter_sampling = parsed$iter_sampling
)

#' Fit recent hospital admission and wastewater
#' trends using BRMS
#'
#' @param forecast_date forecast_date for which to fits recent data
#' trends. Data for an actual forecast must already have been produced
#' and exist in `raw_output_dir`.
#' @param location Location for which to fit trends.
#' @param scenario Wastewater data availability scenario to analyze.
#' the trend in admissions data.
#' @param raw_output_dir Directory containing raw output `.rds` files.
#' Used to obtain the admissions and wastewater data used in fitting
#' the forecasting model.
#' @param hosp_prior_params Named list of values for prior
#' hyperparameters needed by [fit_hosp_trend()].
#' @param ww_prior_params Named list of values for prior hyperparameters
#' needed by [fit_ww_trend()].
#' @param hosp_lookback_days Number of days to look back when fitting the
#' trend in hospital admissions.
#' @param ww_lookback_days Number of days to look back when fitting the
#' trend in wastewater concentrations.
#' @param seed Seed for Stan's pseudorandom number generator.
#' @param control list of control parameters passed to stan via
#' [brms::brm()]. Default `NULL`.
#' @param ... Additional keyword arguments passed to [brms::brm()]
#' via [fit_hosp_trend()].
#' @return The two fit objects, as a list, saving raw fits to disk
#' as a side effect.
#' @export
fit_recent_trends <- function(
  forecast_date,
  location,
  scenario,
  raw_output_dir,
  hosp_prior_params,
  ww_prior_params,
  hosp_lookback_days,
  ww_lookback_days,
  seed,
  control,
  ...
) {
  general_fit_params <- c("data", "seed", "control", "...")

  needed_hosp_prior_params <- setdiff(
    names(formals(fit_hosp_trend)),
    general_fit_params
  )
  needed_ww_prior_params <- setdiff(
    names(formals(fit_ww_trend)),
    general_fit_params
  )

  checkmate::assert_names(
    names(hosp_prior_params),
    permutation.of = needed_hosp_prior_params
  )
  checkmate::assert_names(
    names(ww_prior_params),
    permutation.of = needed_ww_prior_params
  )

  load_object <- get_object_loader(
    location,
    forecast_date,
    scenario,
    raw_output_dir
  )
  save_object <- get_object_saver(
    location,
    forecast_date,
    scenario,
    raw_output_dir
  )

  first_hosp_trend_date <-
    as.Date(forecast_date) - lubridate::days(hosp_lookback_days)
  first_ww_trend_date <-
    as.Date(forecast_date) - lubridate::days(ww_lookback_days)
  message("Loading input data...")

  input_hosp_data <- load_object("input_hosp_data") |>
    dplyr::transmute(
      time = as.numeric(
        .data$date - !!first_hosp_trend_date
      ) +
        1L,
      hosp = .data$count
    ) |>
    dplyr::filter(.data$time > 0)

  input_ww_data <- load_object("input_ww_data") |>
    dplyr::mutate(
      time = as.numeric(
        .data$date - !!first_ww_trend_date
      ) +
        1L,
      conc = ifelse(
        .data$below_lod,
        .data$log_lod,
        .data$log_genome_copies_per_ml
      ),
      cens = ifelse(.data$below_lod, -1, 0)
    ) |>
    dplyr::filter(.data$time > 0) |>
    dplyr::select(
      "time",
      "lab_site_index",
      "conc",
      "cens"
    )

  message("Fitting admissions trend...")
  hosp_args <- c(
    list(data = input_hosp_data),
    hosp_prior_params,
    list(seed = seed, control = control),
    ...
  )
  recent_hosp_trend_fit <- do.call(fit_hosp_trend, hosp_args)
  save_object(recent_hosp_trend_fit)

  message("Fitting  wastewater trend...")
  ww_args <- c(
    list(data = input_ww_data),
    ww_prior_params,
    list(seed = seed, control = control),
    ...
  )
  recent_ww_trend_fit <- do.call(fit_ww_trend, ww_args)
  save_object(recent_ww_trend_fit)

  return(list(
    hosp = recent_hosp_trend_fit,
    ww = recent_ww_trend_fit
  ))
}


#' Fit a hosp trend with BRMS
#'
#' @param data Data to fit. Must have columns `"hosp"` and `"time"`.
#' @param exp_rate_hosp_prior_mean Mean for the Normal prior on
#' the exponential growth rate of admissions.
#' @param exp_rate_hosp_prior_sd Standard deviation for the Normal prior on
#' the exponential growth rate of admissions.
#' @param log_nb_conc_prior_mean Mean for the Normal prior on
#' the log of the negative binomial concentration parameter ("phi").
#' @param log_nb_conc_prior_sd Standard deviation for the Normal prior on
#' the log of the negative binomial concentration parameter ("phi").
#' @param seed Seed for cmdnstan's pseudorandom number generator.
#' @param control list of control parameters passed to stan via
#' [brms::brm()]. Default `NULL`.
#' @param ... Additional keyword arguments passed to [brms::brm()]
#' @export
fit_hosp_trend <- function(
  data,
  exp_rate_hosp_prior_mean,
  exp_rate_hosp_prior_sd,
  log_nb_conc_prior_mean,
  log_nb_conc_prior_sd,
  seed,
  control,
  ...
) {
  checkmate::assert_names(
    names(data),
    must.include = c("hosp", "time")
  )
  fit <- brms::brm(
    formula = brms::bf(hosp ~ time),
    data = data,
    family = brms::negbinomial(
      link = "log",
      link_shape = "log"
    ),
    prior = c(
      brms::prior_string(
        glue::glue(
          "normal(",
          "{exp_rate_hosp_prior_mean}, ",
          "{exp_rate_hosp_prior_sd})"
        ),
        class = "b",
        coef = "time"
      ),
      brms::prior_string(
        glue::glue(
          "normal(",
          "{log_nb_conc_prior_mean}, ",
          "{log_nb_conc_prior_sd})"
        ),
        class = "shape"
      )
    ),
    seed = seed,
    backend = "cmdstanr",
    control = control,
    ...
  )

  return(fit)
}


#' Fit lab-site-level recent wastewater trends with BRMS.
#'
#' @param data Data to fit. Must have columns `"time"`,
#' `'lab_site_index"`, `"conc"`, and `"cens"`.
#' @param exp_rate_conc_pop_prior_mean Mean for the Normal prior on
#' the population-wide exponential growth rate of wastewater viral
#' genome concentrations.
#' @param exp_rate_conc_pop_prior_sd Standard deviation for the
#' Normal prior on the population-wide exponential growth rate of
#' wastewater viral genome concentrations.
#' @param sd_exp_rate_conc_prior_mode Mode for the positive-constrained
#' Normal prior on the standard deviation of wastewater viral
#' genome concentration exponential growth rates (which governs
#' the variability in growth rate among lab-sites)
#' @param sd_exp_rate_conc_prior_sd Standard deviation for the
#' positive-constrained Normal prior on the standard deviation
#' of wastewater viral genome concentration exponential growth
#' rates (which governs the variability in growth rate
#' among lab-sites)
#' @param t0_conc_pop_prior_mean Mean for the Normal prior on the
#' population-wide wastewater viral genome concentration at time
#' t = 0.
#' @param t0_conc_pop_prior_sd Standard deviation for the Normal
#' prior on the population-wide wastewater viral genome concentration
#' at time t = 0.
#' @param sd_t0_conc_prior_mode Mode for the positive-constrained
#' Normal prior on the standard deviation of wastewater viral genome
#' concentrations at time t = 0 (which governs the variability in
#' initial concentration among lab-sites).
#' @param sd_t0_conc_prior_sd Standard deviation for the
#' positive-constrained Normal prior on the standard deviation of
#' wastewater viral genome concentrations at time t = 0
#' (which governs the variability in initial concentration
#' among lab-sites).
#' @param log_sd_obs_pop_prior_mean Mean for the log of the
#' population-wide observation error standard deviation
#' (observation errors are Normal).
#' @param log_sd_obs_pop_prior_sd Standard deviation for the
#' log of the population-wide observation error
#' standard deviation (observation errors are Normal).
#' @param sd_log_sd_obs_prior_mode Mode for the positive-constrained
#' Normal prior on the standard deviation of log lab-site-specific
#' observation error standard deviations (which governs the variability
#' in observation error scale among lab-sites).
#' @param sd_log_sd_obs_prior_sd Standard deviation for the
#' positive-constrained Normal prior on the standard deviation
#' observation error standard deviations (which governs the variability
#' in observation error scale among lab-sites).
#' @param seed Seed for cmdnstan's pseudorandom number generator.
#' @param control list of control parameters passed to stan via
#' [brms::brm()]. Default `NULL`.
#' @param ... Additional keyword arguments passed to [brms::brm()]
#' @export
fit_ww_trend <- function(
  data,
  exp_rate_conc_pop_prior_mean,
  exp_rate_conc_pop_prior_sd,
  sd_exp_rate_conc_prior_mode,
  sd_exp_rate_conc_prior_sd,
  t0_conc_pop_prior_mean,
  t0_conc_pop_prior_sd,
  sd_t0_conc_prior_mode,
  sd_t0_conc_prior_sd,
  log_sd_obs_pop_prior_mean,
  log_sd_obs_pop_prior_sd,
  sd_log_sd_obs_prior_mode,
  sd_log_sd_obs_prior_sd,
  seed,
  control,
  ...
) {
  checkmate::assert_names(
    names(data),
    must.include = c(
      "time",
      "lab_site_index",
      "conc",
      "cens"
    )
  )

  ww_formula <- brms::bf(
    conc | cens(cens) ~ time + (time || lab_site_index),
    sigma ~ (1 || lab_site_index)
  )
  ww_priors <- c(
    brms::prior_string(
      glue::glue(
        "normal(",
        "{exp_rate_conc_pop_prior_mean}, ",
        "{exp_rate_conc_pop_prior_sd})"
      ),
      class = "b",
      coef = "time"
    ),
    brms::prior_string(
      glue::glue(
        "normal(",
        "{sd_exp_rate_conc_prior_mode}, ",
        "{sd_exp_rate_conc_prior_sd})"
      ),
      class = "sd",
      coef = "time",
      group = "lab_site_index"
    ),
    brms::prior_string(
      glue::glue(
        "normal(",
        "{t0_conc_pop_prior_mean}, ",
        "{t0_conc_pop_prior_sd})"
      ),
      class = "Intercept"
    ),
    brms::prior_string(
      glue::glue(
        "normal({sd_t0_conc_prior_mode}, ",
        "{sd_t0_conc_prior_sd})"
      ),
      class = "sd",
      coef = "Intercept",
      group = "lab_site_index"
    ),
    brms::prior_string(
      glue::glue(
        "normal(",
        "{log_sd_obs_pop_prior_mean}, ",
        "{log_sd_obs_pop_prior_sd})"
      ),
      dpar = "sigma",
      class = "Intercept"
    ),
    brms::prior_string(
      glue::glue(
        "normal(",
        "{sd_log_sd_obs_prior_mode}, ",
        "{sd_log_sd_obs_prior_sd})"
      ),
      dpar = "sigma",
      class = "sd",
      group = "lab_site_index"
    )
  )
  fit <- brms::brm(
    formula = ww_formula,
    data = data,
    family = brms::brmsfamily(
      "gaussian",
      link = "identity",
      link_sigma = "log"
    ),
    prior = ww_priors,
    seed = seed,
    backend = "cmdstanr",
    control = control,
    ...
  )

  return(fit)
}

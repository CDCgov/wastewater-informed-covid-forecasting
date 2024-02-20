############
# Make entirely fake stan input data via prior-predictive generated quantities
############

example_df <- cfaforecastrenewalww::example_df

params <- cfaforecastrenewalww::get_params()
forecast_date <- example_df |>
  dplyr::pull(forecast_date) |>
  unique()
forecast_time <- as.integer(max(example_df$date) - forecast_date)

train_data_raw <- example_df |>
  dplyr::mutate(
    ww = exp(log_conc),
    period = dplyr::case_when(
      !is.na(daily_hosp_admits) ~ "calibration",
      is.na(daily_hosp_admits) & date <= forecast_date ~
        "nowcast",
      TRUE ~ "forecast"
    ),
    include_ww = 1,
    site_index = site,
    lab_site_index = lab_wwtp_unique_id
  )

train_data <- cfaforecastrenewalww::flag_ww_outliers(train_data_raw)

generation_interval <- withr::with_seed(42, {
  cfaforecastrenewalww::simulate_double_censored_pmf(
    max = params$gt_max, meanlog = params$mu_gi,
    sdlog = params$sigma_gi, fun_dist = rlnorm, n = 5e6
  ) |> cfaforecastrenewalww::drop_first_and_renormalize()
})

inc <- cfaforecastrenewalww::make_incubation_period_pmf(
  params$backward_scale, params$backward_shape, params$r
)
sym_to_hosp <- cfaforecastrenewalww::make_hospital_onset_delay_pmf(
  params$neg_binom_mu,
  params$neg_binom_size
)
inf_to_hosp <- cfaforecastrenewalww::make_reporting_delay_pmf(inc, sym_to_hosp)

############
# Make entirely fake stan data object for site-level ID model
############
toy_stan_data_id <- cfaforecastrenewalww::get_stan_data_site_level_model(
  train_data,
  params,
  forecast_date,
  forecast_time,
  model_type = "site-level infection dynamics",
  generation_interval = generation_interval,
  inf_to_hosp = inf_to_hosp,
  infection_feedback_pmf = generation_interval
)

############
# Make entirely fake posterior::draws-class object for site-level ID model
############

model_type <- "site-level infection dynamics"
model_file_path <- cfaforecastrenewalww::get_model_file_path(model_type)

model <- cfaforecastrenewalww::ww_model(model_file_path)

fit_dynamic_rt <- model$sample(
  data = toy_stan_data_id,
  seed = 123,
  iter_sampling = 25,
  iter_warmup = 25,
  chains = 1
)

toy_stan_fit_last_draw_id <- posterior::subset_draws(fit_dynamic_rt$draws(), draw = 25)

############
# Make entirely fake stan data object for hosp-only model
############
toy_stan_data_ho <- cfaforecastrenewalww::get_stan_data(
  train_data,
  params,
  forecast_date,
  forecast_time,
  model_type = "hospital admissions only",
  generation_interval = generation_interval,
  inf_to_hosp = inf_to_hosp,
  infection_feedback_pmf = generation_interval,
  compute_likelihood = TRUE,
  include_hosp = TRUE
)

toy_stan_data_ho$include_ww <- 0

############
# Make entirely fake posterior::draws-class object for site-level ID model
############

model_type <- "hospital admissions only"
model_file_path <- cfaforecastrenewalww::get_model_file_path(model_type)

model <- cfaforecastrenewalww::ww_model(model_file_path)

fit_dynamic_rt <- model$sample(
  data = toy_stan_data_ho,
  seed = 123,
  iter_sampling = 25,
  iter_warmup = 25,
  chains = 1
)

toy_stan_fit_last_draw_ho <- posterior::subset_draws(fit_dynamic_rt$draws(), draw = 25)

usethis::use_data(
  toy_stan_data_id,
  toy_stan_fit_last_draw_id,
  toy_stan_data_ho,
  toy_stan_fit_last_draw_ho,
  internal = TRUE,
  overwrite = TRUE
)

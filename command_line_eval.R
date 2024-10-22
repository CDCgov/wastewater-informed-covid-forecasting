#!/usr/bin/env Rscript

# This script replicates the functionality of `_targets_eval.R`, but takes in
# command line arguments so that each config in eval_config.yaml can be executed
# in parallel on Azure batch

library(argparser, quietly = TRUE)
library(cfaforecastrenewalww)
library(wweval)
library(ggplot2)
# some functions from plots.R complain about aes() function not existing if we don't load ggplot2

eval_fit <- function(config_index, eval_config_path, output_dir) {
  save_object <- function(object_name, output_file_suffix) {
    saveRDS(
      object = get(object_name),
      file = file.path(output_dir, paste0(object_name, output_file_suffix))
    )
  }

  wwinference::create_dir(output_dir)

  eval_config <- yaml::read_yaml(eval_config_path)
  params <- wwinference::get_params(file.path(
    "input", "params.toml"
  )) |> as.data.frame()


  location <- eval_config$location_ww[config_index]
  forecast_date <- eval_config$forecast_date_ww[config_index]
  scenario <- eval_config$scenario[config_index]

  output_file_suffix <- paste("", location, format(as.Date(forecast_date), "%Y.%m.%d"), scenario,
    sep = "_"
  )




  # Get the evaluation data from the specified evaluation date ----------------
  eval_hosp_data <- get_input_hosp_data(
    forecast_date_i = eval_config$eval_date,
    location_i = unique(eval_config$location_ww),
    hosp_data_dir = eval_config$hosp_data_dir,
    calibration_time = 365 # Grab sufficient data for eval
  )

  save_object("eval_hosp_data", output_file_suffix)

  eval_ww_data <- get_input_ww_data(
    forecast_date_i = eval_config$eval_date,
    location_i = unique(eval_config$location_ww),
    scenario = "status_quo",
    scenario_dir = eval_config$scenario_dir,
    ww_data_dir = eval_config$ww_data_dir,
    calibration_time = 365, # Grab sufficient data for eval
    last_hosp_data_date = eval_config$eval_date,
    ww_data_mapping = eval_config$ww_data_mapping
  )

  save_object("eval_ww_data", output_file_suffix)

  stan_model_path_target <- get_model_path(
    model_type = "ww",
    stan_models_dir = eval_config$stan_models_dir
  )

  input_hosp_data <- get_input_hosp_data(
    forecast_date_i = forecast_date,
    location_i = location,
    hosp_data_dir = eval_config$hosp_data_dir,
    calibration_time = eval_config$calibration_time
  )

  save_object("input_hosp_data", output_file_suffix)

  last_hosp_data_date <- get_last_hosp_data_date(input_hosp_data)

  save_object("last_hosp_data_date", output_file_suffix)

  input_ww_data <- get_input_ww_data(forecast_date,
    location,
    scenario,
    scenario_dir = eval_config$scenario_dir,
    ww_data_dir = eval_config$ww_data_dir,
    calibration_time = eval_config$calibration_time,
    last_hosp_data_date = last_hosp_data_date,
    ww_data_mapping = eval_config$ww_data_mapping
  )

  save_object("input_ww_data", output_file_suffix)

  standata <- get_stan_data_list(
    model_type = "ww",
    forecast_date, eval_config$forecast_time,
    input_ww_data, input_hosp_data,
    generation_interval = eval_config$generation_interval,
    inf_to_hosp = eval_config$inf_to_hosp,
    infection_feedback_pmf = eval_config$infection_feedback_pmf,
    params
  )

  save_object("standata", output_file_suffix)

  init_lists <- wweval:::get_inits(
    model_type = "ww", standata, params,
    n_chains = eval_config$n_chains
  )

  save_object("init_lists", output_file_suffix)


  ww_fit_obj <- wweval::sample_model(
    standata,
    stan_model_path = stan_model_path_target,
    stan_models_dir = eval_config$stan_models_dir,
    init_lists,
    iter_warmup = eval_config$iter_warmup,
    iter_sampling = eval_config$iter_sampling,
    adapt_delta = eval_config$adapt_delta,
    n_chains = eval_config$n_chains,
    max_treedepth = eval_config$max_treedepth,
    seed = eval_config$seed
  )

  save_object("ww_fit_obj", output_file_suffix)


  ## Post-processing---------------------------------------------------------
  ww_raw_draws <- ww_fit_obj$draws

  save_object("ww_raw_draws", output_file_suffix)

  ww_diagnostics <- ww_fit_obj$diagnostics

  save_object("ww_diagnostics", output_file_suffix)

  ww_diagnostic_summary <- ww_fit_obj$summary_diagnostics

  save_object("ww_diagnostic_summary", output_file_suffix)

  errors <- ww_fit_obj$error

  save_object("errors", output_file_suffix)


  # Get evaluation data from hospital admissions and wastewater
  # Join draws with data
  hosp_draws <- if (is.null(ww_raw_draws)) {
    NULL
  } else {
    get_model_draws_w_data(
      model_output = "hosp",
      model_type = "ww",
      draws = ww_raw_draws,
      forecast_date = forecast_date,
      scenario = scenario,
      location = location,
      input_data = input_hosp_data,
      eval_data = eval_hosp_data,
      last_hosp_data_date = last_hosp_data_date,
      ot = eval_config$calibration_time,
      forecast_time = eval_config$forecast_time
    )
  }

  save_object("hosp_draws", output_file_suffix)


  ww_draws <- if (is.null(ww_raw_draws)) {
    NULL
  } else {
    get_model_draws_w_data(
      model_output = "ww",
      model_type = "ww",
      draws = ww_raw_draws,
      forecast_date = forecast_date,
      scenario = scenario,
      location = location,
      input_data = input_ww_data,
      eval_data = eval_ww_data,
      last_hosp_data_date = last_hosp_data_date,
      ot = eval_config$calibration_time,
      forecast_time = eval_config$forecast_time
    )
  }

  save_object("ww_draws", output_file_suffix)


  full_hosp_quantiles <-
    if (is.null(hosp_draws)) {
      NULL
    } else {
      get_state_level_quantiles(
        draws = hosp_draws
      )
    }


  save_object("full_hosp_quantiles", output_file_suffix)

  # @TODO Save forecasted hospital quantiles locally as well as via
  # targets caching just for backup
  plot_hosp_draws <-
    if (is.null(full_hosp_quantiles)) {
      NULL
    } else {
      full_hosp_quantiles |>
        dplyr::filter(period != "calibration")
    }

  save_object("plot_hosp_draws", output_file_suffix)

  ### Plot the draw comparison-------------------------------------
  plot_hosp_data_comparison <-
    if (is.null(hosp_draws)) {
      NULL
    } else {
      get_plot_hosp_data_comparison(
        hosp_draws,
        location,
        model_type = "ww"
      )
    }

  save_object("plot_hosp_data_comparison", output_file_suffix)


  plot_ww_draws <-
    if (is.null(ww_draws)) {
      NULL
    } else {
      get_plot_ww_data_comparison(
        ww_draws,
        location,
        model_type = "ww"
      )
    }

  save_object("plot_ww_draws", output_file_suffix)


  ## Score hospital admissions forecasts----------------------------------
  # @TODO save scores locally
  hosp_scores <- get_full_scores(hosp_draws, scenario)

  save_object("hosp_scores", output_file_suffix)


  # Get a subset of samples for plotting
  # Get a subset of quantiles for plotting
}


parsed_args <- arg_parser("Run eval pipeline for one config") |>
  add_argument("config_index", help = "index of entry in eval_config to use", type = "integer") |>
  add_argument("eval_config_path", help = "path to eval_config.yaml") |>
  add_argument("output_dir", help = "directory to store output") |>
  parse_args()


eval_fit(
  config_index = parsed_args$config_index,
  eval_config_path = parsed_args$eval_config_path,
  output_dir = parsed_args$output_dir
)

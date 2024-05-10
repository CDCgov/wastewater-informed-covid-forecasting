#' Evaluate Wastewater Fit
#'
#' @param config_index Index of eval_config to evaluate
#' @param eval_config_path Path to eval_config (created with `write_eval_config`)
#' @param params_path Path to params.toml
#' @param output_dir Path to directory for storing output
#'
#' @return NULL
#' @export
#'

eval_fit_ww <- function(config_index,
                        eval_config_path,
                        params_path,
                        output_dir) {
  save_object <- function(object_name, output_file_suffix) {
    saveRDS(
      object = get(object_name),
      file = file.path(raw_output_dir, paste0(object_name, output_file_suffix))
    )
  }
  raw_output_dir <- file.path(output_dir, "raw_output")
  cfaforecastrenewalww::create_dir(output_dir)
  cfaforecastrenewalww::create_dir(raw_output_dir)
  eval_config <- yaml::read_yaml(eval_config_path)
  params <- cfaforecastrenewalww::get_params(params_path)
  location <- eval_config$location_ww[config_index]
  forecast_date <- eval_config$forecast_date_ww[config_index]
  scenario <- eval_config$scenario[config_index]

  output_file_suffix <- paste("", location, format(as.Date(forecast_date), "%Y.%m.%d"), scenario,
    sep = "_"
  ) |> paste0(".rds")




  # Get the evaluation data from the specified evaluation date ----------------
  eval_hosp_data <- get_input_hosp_data(
    forecast_date = eval_config$eval_date,
    location = location,
    hosp_data_dir = eval_config$hosp_data_dir,
    calibration_time = 365 # Grab sufficient data for eval
  )

  save_object("eval_hosp_data", output_file_suffix)

  eval_ww_data <- get_input_ww_data(
    forecast_date = eval_config$eval_date,
    location = location,
    scenario = "status_quo",
    scenario_dir = eval_config$scenario_dir,
    ww_data_dir = eval_config$ww_data_dir,
    calibration_time = 365, # Grab sufficient data for eval
    last_hosp_data_date = eval_config$eval_date,
    ww_data_mapping = eval_config$ww_data_mapping
  )

  save_object("eval_ww_data", output_file_suffix)

  # Wastewater model fitting loop-----------------------------------------------

  stan_model_path_target <- get_model_path(
    model_type = "ww",
    stan_models_dir = eval_config$stan_models_dir
  )
  input_hosp_data <- get_input_hosp_data(forecast_date, location,
    hosp_data_dir = eval_config$hosp_data_dir,
    calibration_time = eval_config$calibration_time
  )

  save_object("input_hosp_data", output_file_suffix)


  last_hosp_data_date <- get_last_hosp_data_date(input_hosp_data)
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

  ## Get the stan data for this location, forecast_date, and scenario ----------
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
  ## Model fitting ----------------------------------------------------------
  init_lists <- get_inits(
    model_type = "ww", standata, params,
    n_chains = eval_config$n_chains
  )

  save_object("init_lists", output_file_suffix)

  ww_fit_obj <- sample_model(
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
  ww_summary <- ww_fit_obj$summary
  save_object("ww_summary", output_file_suffix)
  errors <- ww_fit_obj$error
  save_object("errors", output_file_suffix)
  flags <- ww_fit_obj$flags
  save_object("flags", output_file_suffix)
  # Save errors
  save_table(
    data_to_save = errors,
    type_of_output = "errors",
    output_dir = output_dir,
    scenario = scenario,
    forecast_date = forecast_date,
    model_type = "ww",
    location = location
  )
  # Save flags
  save_table(
    data_to_save = flags,
    type_of_output = "flags",
    output_dir = output_dir,
    scenario = scenario,
    forecast_date = forecast_date,
    model_type = "ww",
    location = location
  )
  # Get evaluation data from hospital admissions and wastewater
  # Join draws with data
  hosp_draws <- {
    if (is.null(ww_raw_draws)) {
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
  }
  save_object("hosp_draws", output_file_suffix)
  ww_draws <- {
    if (is.null(ww_raw_draws)) {
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
  }
  save_object("ww_draws", output_file_suffix)

  full_hosp_quantiles <- {
    if (is.null(hosp_draws)) {
      NULL
    } else {
      get_state_level_quantiles(
        draws = hosp_draws
      )
    }
  }
  save_object("full_hosp_quantiles", output_file_suffix)
  full_ww_quantiles <- {
    if (is.null(ww_draws)) {
      NULL
    } else {
      get_state_level_ww_quantiles(
        ww_draws = ww_draws
      )
    }
  }
  save_object("full_ww_quantiles", output_file_suffix)

  hosp_quantiles <- {
    if (is.null(full_hosp_quantiles)) {
      NULL
    } else {
      full_hosp_quantiles |>
        dplyr::filter(period != "calibration")
    }
  }
  save_object("hosp_quantiles", output_file_suffix)

  ww_quantiles <- {
    if (is.null(full_ww_quantiles)) {
      NULL
    } else {
      full_ww_quantiles |>
        dplyr::filter(period != "calibration")
    }
  }
  save_object("ww_quantiles", output_file_suffix)
  # Save forecasted quantiles locally as well as via
  # targets caching just for backup
  save_table(
    data_to_save = full_hosp_quantiles,
    type_of_output = "hosp_quantiles",
    output_dir = output_dir,
    scenario = scenario,
    forecast_date = forecast_date,
    model_type = "ww",
    location = location
  )

  save_table(
    data_to_save = full_ww_quantiles,
    type_of_output = "ww_quantiles",
    output_dir = output_dir,
    scenario = scenario,
    forecast_date = forecast_date,
    model_type = "ww",
    location = location
  )

  ### Plot the draw comparison-------------------------------------
  plot_hosp_draws <- {
    if (is.null(hosp_draws)) {
      NULL
    } else {
      get_plot_hosp_data_comparison(
        hosp_draws,
        location,
        model_type = "ww"
      )
    }
  }

  save_object("plot_hosp_draws", output_file_suffix)

  plot_ww_draws <- {
    if (is.null(ww_draws)) {
      NULL
    } else {
      get_plot_ww_data_comparison(
        ww_draws,
        location,
        model_type = "ww"
      )
    }
  }
  save_object("plot_ww_draws", output_file_suffix)
  ## Score hospital admissions forecasts----------------------------------
  hosp_scores <- get_full_scores(hosp_draws, scenario)
  save_object("hosp_scores", output_file_suffix)
  save_table(
    data_to_save = hosp_scores,
    type_of_output = "scores",
    output_dir = output_dir,
    scenario = scenario,
    forecast_date = forecast_date,
    model_type = "ww",
    location = location
  )

  hosp_scores_quantiles <- get_scores_from_quantiles(hosp_quantiles, scenario)
  save_object("hosp_scores_quantiles", output_file_suffix)
  save_table(
    data_to_save = hosp_scores_quantiles,
    type_of_output = "scores_quantiles",
    output_dir = output_dir,
    scenario = scenario,
    forecast_date = forecast_date,
    model_type = "ww",
    location = location
  )
  # Get a subset of samples for plotting
  # Get a subset of quantiles for plotting
}

#' Evaluate Hospitalizations Fit
#'
#' @param config_index Index of eval_config to evaluate
#' @param eval_config_path Path to eval_config (created with `write_eval_config`)
#' @param params_path Path to params.toml
#' @param output_dir Path to directory for storing output
#'
#' @return NULL
#' @export
#'
eval_fit_hosp <- function(config_index,
                          eval_config_path,
                          params_path,
                          output_dir) {
  save_object <- function(object_name, output_file_suffix) {
    saveRDS(
      object = get(object_name),
      file = file.path(raw_output_dir, paste0(object_name, output_file_suffix))
    )
  }
  raw_output_dir <- file.path(output_dir, "raw_output")
  cfaforecastrenewalww::create_dir(output_dir)
  cfaforecastrenewalww::create_dir(raw_output_dir)
  eval_config <- yaml::read_yaml(eval_config_path)
  params <- cfaforecastrenewalww::get_params(params_path)
  location <- eval_config$location_hosp[config_index]
  forecast_date <- eval_config$forecast_date_hosp[config_index]
  scenario <- "no_wastewater"

  output_file_suffix <- paste("", location, format(as.Date(forecast_date), "%Y.%m.%d"), scenario,
    sep = "_"
  ) |> paste0(".rds")




  # Get the evaluation data from the specified evaluation date ----------------
  eval_hosp_data <- get_input_hosp_data(
    forecast_date = eval_config$eval_date,
    location = location,
    hosp_data_dir = eval_config$hosp_data_dir,
    calibration_time = 365 # Grab sufficient data for eval
  )

  save_object("eval_hosp_data", output_file_suffix)

  # Hospital admissions model fitting loop-----------------------------------------------
  stan_model_path_target <- get_model_path(
    model_type = "hosp",
    stan_models_dir = eval_config$stan_models_dir
  )

  input_hosp_data <- get_input_hosp_data(forecast_date, location,
    hosp_data_dir = eval_config$hosp_data_dir,
    calibration_time = eval_config$calibration_time
  )
  save_object("input_hosp_data", output_file_suffix)
  last_hosp_data_date <- get_last_hosp_data_date(input_hosp_data)

  ## Get the stan data for this location, forecast_date, and scenario ----------
  standata <- get_stan_data_list(
    model_type = "hosp",
    forecast_date, eval_config$forecast_time,
    input_ww_data = NA,
    input_hosp_data = input_hosp_data,
    generation_interval = eval_config$generation_interval,
    inf_to_hosp = eval_config$inf_to_hosp,
    infection_feedback_pmf = eval_config$infection_feedback_pmf,
    params
  )
  save_object("standata", output_file_suffix)
  ## Model fitting-----------------------------------------------------------
  init_lists <- get_inits(
    model_type = "hosp",
    standata, params,
    n_chains = eval_config$n_chains
  )
  save_object("init_lists", output_file_suffix)
  hosp_fit_obj <- sample_model(
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
  save_object("hosp_fit_obj", output_file_suffix)
  ## Post-processing---------------------------------------------------------
  hosp_raw_draws <- hosp_fit_obj$draws
  save_object("hosp_raw_draws", output_file_suffix)
  hosp_diagnostics <- hosp_fit_obj$diagnostics
  save_object("hosp_diagnostics", output_file_suffix)
  hosp_diagnostic_summary <- hosp_fit_obj$summary_diagnostics
  save_object("hosp_diagnostic_summary", output_file_suffix)
  hosp_summary <- hosp_fit_obj$summary
  save_object("hosp_summary", output_file_suffix)
  errors <- hosp_fit_obj$error
  save_object("errors", output_file_suffix)
  flags <- hosp_fit_obj$flags
  save_object("flags", output_file_suffix)
  # Save errors
  save_table(
    data_to_save = errors,
    type_of_output = "errors",
    output_dir = output_dir,
    scenario = scenario,
    forecast_date = forecast_date,
    model_type = "hosp",
    location = location
  )
  # Save flags
  save_table(
    data_to_save = flags,
    type_of_output = "flags",
    output_dir = output_dir,
    scenario = scenario,
    forecast_date = forecast_date,
    model_type = "hosp",
    location = location
  )


  # Get evaluation data from hospital admissions and wastewater
  # Join draws with data
  hosp_model_hosp_draws <- get_model_draws_w_data(
    model_output = "hosp",
    model_type = "hosp",
    draws = hosp_raw_draws,
    forecast_date = forecast_date,
    scenario = "no_wastewater",
    location = location,
    input_data = input_hosp_data,
    eval_data = eval_hosp_data,
    last_hosp_data_date = last_hosp_data_date,
    ot = eval_config$calibration_time,
    forecast_time = eval_config$forecast_time
  )
  save_object("hosp_model_hosp_draws", output_file_suffix)
  full_hosp_model_quantiles <- get_state_level_quantiles(
    draws = hosp_model_hosp_draws
  )
  save_object("full_hosp_model_quantiles", output_file_suffix)

  hosp_model_quantiles <- full_hosp_model_quantiles |>
    dplyr::filter(period != "calibration")
  save_object("hosp_model_quantiles", output_file_suffix)

  # Save forecasted quantiles locally as well as via
  # targets caching just for backup
  save_table(
    data_to_save = hosp_model_quantiles,
    type_of_output = "quantiles",
    output_dir = output_dir,
    scenario = "no_wastewater",
    forecast_date = forecast_date,
    model_type = "hosp",
    location = location
  )

  ### Plot the draw comparison-------------------------------------
  plot_hosp_draws_hosp_model <- get_plot_hosp_data_comparison(
    hosp_model_hosp_draws,
    location,
    model_type = "hosp"
  )
  save_object("plot_hosp_draws_hosp_model", output_file_suffix)
  ## Score the hospital admissions only model-------------------------
  hosp_scores <- get_full_scores(hosp_model_hosp_draws,
    scenario = "no_wastewater"
  )
  save_object("hosp_scores", output_file_suffix)
  save_table(
    data_to_save = hosp_scores,
    type_of_output = "scores",
    output_dir = output_dir,
    scenario = "no_wastewater",
    forecast_date = forecast_date,
    model_type = "hosp",
    location = location
  )

  hosp_scores_quantiles <- get_scores_from_quantiles(hosp_model_quantiles,
    scenario = "no_wastewater"
  )
  save_object("hosp_scores_quantiles", output_file_suffix)
  save_table(
    data_to_save = hosp_scores_quantiles,
    type_of_output = "scores_quantiles",
    output_dir = output_dir,
    scenario = "no_wastewater",
    forecast_date = forecast_date,
    model_type = "hosp",
    location = location
  )
}

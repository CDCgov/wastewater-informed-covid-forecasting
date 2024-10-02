#' Fit Wastewater Model for Evaluation
#'
#' @param config_index Index of eval_config to evaluate
#' @param eval_config_path Path to eval_config (created with `write_eval_config`)
#' @param params_path Path to params.toml
#'
#' @return NULL
#' @export
#'

eval_fit_ww <- function(config_index,
                        eval_config_path,
                        params_path) {
  eval_config <- yaml::read_yaml(eval_config_path)
  output_dir <- eval_config$output_dir
  raw_output_dir <- eval_config$raw_output_dir

  save_object <- function(object_name, output_file_suffix) {
    saveRDS(
      object = get(object_name),
      file = file.path(raw_output_dir, paste0(object_name, output_file_suffix))
    )
  }


  wwinference::create_dir(output_dir)
  wwinference::create_dir(raw_output_dir)

  # params <- wwinference::get_params(params_path) #nolint
  # Temporarily get parameters from local package install to test, since
  # need updates from branch of wwinferece
  params <- get_params(
    system.file("extdata", "example_params.toml",
      package = "wwinference"
    )
  )
  location <- eval_config$location_ww[config_index]
  forecast_date <- eval_config$forecast_date_ww[config_index]
  scenario <- eval_config$scenario[config_index]

  output_file_suffix <- paste("", location, format(as.Date(forecast_date), "%Y.%m.%d"), scenario,
    sep = "_"
  ) |> paste0(".rds")


  table_of_exclusions <- tibble::as_tibble(eval_config$table_of_exclusions)

  # Wastewater model fitting loop-----------------------------------------------

  raw_input_hosp_data <- get_input_hosp_data(
    forecast_date_i = forecast_date,
    location_i = location,
    hosp_data_dir = eval_config$hosp_data_dir,
    calibration_time = eval_config$calibration_time
  )
  input_hosp_data <- exclude_hosp_outliers(
    raw_input_hosp_data = raw_input_hosp_data,
    forecast_date = forecast_date,
    table_of_exclusions = table_of_exclusions
  )
  save_object("input_hosp_data", output_file_suffix)


  last_hosp_data_date <- get_last_hosp_data_date(input_hosp_data)
  input_ww_data <- get_input_ww_data(
    forecast_date_i = forecast_date,
    location_i = location,
    scenario_i = scenario,
    scenario_dir = eval_config$scenario_dir,
    ww_data_dir = eval_config$ww_data_dir,
    calibration_time = eval_config$calibration_time,
    last_hosp_data_date = last_hosp_data_date,
    ww_data_mapping = eval_config$ww_data_mapping
  )

  save_object("input_ww_data", output_file_suffix)

  ## Use wwinference to fit the model------------------------------------------
  ww_fit_obj <- wwinference::wwinference(
    ww_data = input_ww_data,
    count_data = input_hosp_data,
    forecast_date = forecast_date,
    calibration_time = eval_config$calibration_time,
    forecast_horizon = eval_config$forecast_time,
    model_spec = wwinference::get_model_spec(
      generation_interval = eval_config$generation_interval,
      inf_to_count_delay = wwinference::default_covid_inf_to_hosp, # eval_config$inf_to_hosp,
      infection_feedback_pmf = eval_config$infection_feedback_pmf,
      params = params
    ),
    fit_opts = list(
      seed = eval_config$seed,
      iter_sampling = eval_config$iter_sampling,
      adapt_delta = eval_config$adapt_delta,
      chains = eval_config$n_chains,
      max_treedepth = eval_config$max_treedepth
    )
  )

  save_object("ww_fit_obj", output_file_suffix)


  # Get the evaluation data from the specified evaluation date ----------------
  eval_hosp_data <- get_input_hosp_data(
    forecast_date_i = eval_config$eval_date,
    location_i = location,
    hosp_data_dir = eval_config$hosp_data_dir,
    calibration_time = 365 # Grab sufficient data for eval
  ) |>
    dplyr::filter(date >= min(input_hosp_data$date))

  save_object("eval_hosp_data", output_file_suffix)

  eval_ww_data <- get_input_ww_data(
    forecast_date_i = eval_config$eval_date,
    location_i = location,
    scenario_i = "status_quo",
    scenario_dir = eval_config$scenario_dir,
    ww_data_dir = eval_config$ww_data_dir,
    calibration_time = 365, # Grab sufficient data for eval
    last_hosp_data_date = eval_config$eval_date,
    ww_data_mapping = eval_config$ww_data_mapping
  ) |>
    dplyr::filter(date >= min(input_ww_data$date))

  save_object("eval_ww_data", output_file_suffix)

  # Get the table of hospital admissions outliers -----------
}

#' Fit Hospitalizations Model for Evaluation
#'
#' @param config_index Index of eval_config to evaluate
#' @param eval_config_path Path to eval_config (created with `write_eval_config`)
#' @param params_path Path to params.toml
#'
#' @return NULL
#' @export
#'
eval_fit_hosp <- function(config_index,
                          eval_config_path,
                          params_path) {
  eval_config <- yaml::read_yaml(eval_config_path)
  output_dir <- eval_config$output_dir
  raw_output_dir <- eval_config$raw_output_dir

  save_object <- function(object_name, output_file_suffix) {
    saveRDS(
      object = get(object_name),
      file = file.path(raw_output_dir, paste0(object_name, output_file_suffix))
    )
  }

  wwinference::create_dir(output_dir)
  wwinference::create_dir(raw_output_dir)

  # params <- wwinference::get_params(params_path) #nolint
  # Temporarily get parameters from local package install to test, since
  # need updates from branch of wwinferece
  params <- get_params(
    system.file("extdata", "example_params.toml",
      package = "wwinference"
    )
  )
  location <- eval_config$location_hosp[config_index]
  forecast_date <- eval_config$forecast_date_hosp[config_index]
  scenario <- "no_wastewater"

  output_file_suffix <- paste("", location, format(as.Date(forecast_date), "%Y.%m.%d"), scenario,
    sep = "_"
  ) |> paste0(".rds")


  # Get the table of hospital admissions outliers ----------------------------
  table_of_exclusions <- tibble::as_tibble(eval_config$table_of_exclusions)

  # Hospital admissions model fitting loop-----------------------------------------------
  raw_input_hosp_data <- get_input_hosp_data(
    forecast_date_i = forecast_date,
    location_i = location,
    hosp_data_dir = eval_config$hosp_data_dir,
    calibration_time = eval_config$calibration_time
  )
  input_hosp_data <- exclude_hosp_outliers(
    raw_input_hosp_data = raw_input_hosp_data,
    forecast_date = forecast_date,
    table_of_exclusions = table_of_exclusions
  )
  save_object("input_hosp_data", output_file_suffix)


  last_hosp_data_date <- get_last_hosp_data_date(input_hosp_data)

  ## Use wwinference to fit the model------------------------------------------
  hosp_fit_obj <- wwinference::wwinference(
    ww_data = NULL,
    count_data = input_hosp_data,
    forecast_date = forecast_date,
    calibration_time = eval_config$calibration_time,
    forecast_horizon = eval_config$forecast_time,
    model_spec = wwinference::get_model_spec(
      generation_interval = eval_config$generation_interval,
      inf_to_count_delay = wwinference::default_covid_inf_to_hosp, # eval_config$inf_to_hosp,
      infection_feedback_pmf = eval_config$infection_feedback_pmf,
      params = params,
      include_ww = FALSE
    ),
    fit_opts = list(
      seed = eval_config$seed,
      iter_sampling = eval_config$iter_sampling,
      adapt_delta = eval_config$adapt_delta,
      chains = eval_config$n_chains,
      max_treedepth = eval_config$max_treedepth
    )
  )

  save_object("hosp_fit_obj", output_file_suffix)


  # Get the evaluation data from the specified evaluation date ----------------
  eval_hosp_data <- get_input_hosp_data(
    forecast_date_i = eval_config$eval_date,
    location_i = location,
    hosp_data_dir = eval_config$hosp_data_dir,
    calibration_time = 365 # Grab sufficient data for eval
  ) |>
    dplyr::filter(date >= min(input_hosp_data$date))

  save_object("eval_hosp_data", output_file_suffix)
}

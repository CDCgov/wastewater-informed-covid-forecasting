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
  params <- wwinference::get_params(params_path)
  location <- eval_config$location_ww[config_index]
  forecast_date <- eval_config$forecast_date_ww[config_index]
  scenario <- eval_config$scenario[config_index]
  raw_output_suffix <- get_raw_output_suffix(
    location,
    forecast_date,
    scenario
  )

  save_object <- purrr::partial(to_rds_with_suffix,
    output_dir = raw_output_dir,
    save_suffix = raw_output_suffix
  )

  wwinference::create_dir(output_dir)
  wwinference::create_dir(raw_output_dir)

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
  save_object(input_hosp_data)


  last_hosp_data_date <- get_last_hosp_data_date(input_hosp_data)
  input_ww_data <- tryCatch(
    {
      # Try to do peprocessing
      get_input_ww_data(
        forecast_date_i = forecast_date,
        location_i = location,
        scenario_i = scenario,
        scenario_dir = eval_config$scenario_dir,
        ww_data_dir = eval_config$ww_data_dir,
        calibration_time = eval_config$calibration_time,
        last_hosp_data_date = last_hosp_data_date,
        ww_data_mapping = eval_config$ww_data_mapping
      )
    },
    error = function(e) {
      # Handle the error
      message("Caught an error: ", e$message)
    }
  )
  save_object(input_ww_data)

  ## Use wwinference to fit the model------------------------------------------
  if (!is.null(input_ww_data)) {
    ww_fit_obj <- tryCatch(
      {
        wwinference::wwinference(
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
      },
      error = function(e) {
        # Handle the error
        message("Caught an error in wwinference: ", e$message)
      }
    )
  } else {
    # Format as expected from cmdstan object
    ww_fit_obj <- list(
      fit =
        list(result = list(error = "missing ww data"))
    )
  }

  # If wwinference job fails its due to data not overlapping, replace
  # with missing data error for postprocessing to proceed without failure
  if (is.null(ww_fit_obj)) {
    ww_fit_obj <- list(
      fit =
        list(result = list(error = "missing ww data"))
    )
  }

  save_object(ww_fit_obj)
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
  params <- wwinference::get_params(params_path)
  location <- eval_config$location_hosp[config_index]
  forecast_date <- eval_config$forecast_date_hosp[config_index]
  scenario <- "no_wastewater"

  raw_output_suffix <- get_raw_output_suffix(
    location,
    forecast_date,
    scenario
  )

  save_object <- purrr::partial(to_rds_with_suffix,
    output_dir = raw_output_dir,
    save_suffix = raw_output_suffix
  )

  wwinference::create_dir(output_dir)
  wwinference::create_dir(raw_output_dir)

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

  save_object(input_hosp_data)


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
  save_object(hosp_fit_obj)
}

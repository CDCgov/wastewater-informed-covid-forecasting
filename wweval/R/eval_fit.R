#' Fit model for evaluation
#'
#' @param config_index Index of eval_config to evaluate
#' @param eval_config_path Path to eval_config (created with `write_eval_config`)
#' @param params_path Path to params.toml
#' @param model Model to fit. One of `"ww"` and `"hosp"`
#' @return NULL
#' @export
#'
eval_fit <- function(config_index,
                     eval_config_path,
                     params_path,
                     model) {
  checkmate::assert_names(model, subset.of = c("ww", "hosp"))
  ww_model <- model == "ww"

  eval_config <- yaml::read_yaml(eval_config_path)
  output_dir <- eval_config$output_dir
  raw_output_dir <- eval_config$raw_output_dir
  params <- wwinference::get_params(params_path)
  location <- eval_config$location_ww[config_index]
  forecast_date <- eval_config$forecast_date_ww[config_index]
  scenario <- ifelse(ww_model,
    eval_config$scenario[config_index],
    "no_wastewater"
  )
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

  if (ww_model) {
    ww_data_pull <- purrr::safely(get_input_ww_data)(forecast_date_i = forecast_date,
      location_i = location,
      scenario_i = scenario,
      scenario_dir = eval_config$scenario_dir,
      ww_data_dir = eval_config$ww_data_dir,
      calibration_time = eval_config$calibration_time,
      last_hosp_data_date = last_hosp_data_date,
      ww_data_mapping = eval_config$ww_data_mapping
    )

    input_ww_data <- ww_data_pull$result
    save_object(input_ww_data)
    include_ww <- TRUE
  } else {
    input_ww_data <- NULL
    include_ww <- FALSE
  }

  model_spec <- wwinference::get_model_spec(
    generation_interval = eval_config$generation_interval,
    inf_to_count_delay = wwinference::default_covid_inf_to_hosp,
    infection_feedback_pmf = eval_config$infection_feedback_pmf,
    params = params,
    include_ww = include_ww
  )

  fit_opts <- list(
    seed = eval_config$seed,
    iter_sampling = eval_config$iter_sampling,
    adapt_delta = eval_config$adapt_delta,
    chains = eval_config$n_chains,
    max_treedepth = eval_config$max_treedepth
  )

  do_fit <- (
    !is.null(input_hosp_data) &&
      (!include_ww || !is.null(input_ww_data))
  )

  fit_obj <- NULL

  if (do_fit) {
    fit_fn <- purrr::safely(wwinference::wwinference)
    fit_obj <- fit_fn(
      ww_data = input_ww_data,
      count_data = input_hosp_data,
      forecast_date = forecast_date,
      calibration_time = eval_config$calibration_time,
      forecast_horizon = eval_config$forecast_time,
      model_spec = model_spec,
      fit_opts = fit_opts
    )$result
  }

  err_msg <- ifelse(include_ww,
    "missing ww data",
    "fitting error or non-wastewater data error"
  )

  ## If wwinference job fails, replace
  ## with missing data error for postprocessing to proceed
  ## without failure
  if (is.null(fit_obj)) {
    fit_obj <- list(fit = list(result = list(error = err_msg)))
  }

  save_object(fit_obj, save_basename = glue::glue("{model}_fit_obj"))
}

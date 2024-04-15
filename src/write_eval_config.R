#' Write evaluation config file
#'
#' @param locations locations to iterate through, for a full run this should
#' be all 50 states + PR
#' @param forecast_dates the forecast dates we want to run the model on
#' @param scenatios the scenarios (which will pertain to site ids) to
#' run the model on
#' @param config_dir the directory where we want to save the config file
#'
#' @return
#' @export
#'
#' @examples
write_eval_config <- function(locations, forecast_dates,
                              scenarios,
                              config_dir,
                              eval_date) {
  # get a dataframe of all the iteration combinations for the wastewater mapping
  df_ww <- expand.grid(
    location = locations,
    forecast_date = forecast_dates,
    scenario = scenarios
  )
  # No scenarios for the hosp admissions only, so we just need all combos
  # of locations and forecast dates
  df_hosp <- expand.grid(
    location = locations,
    forecast_date = forecast_dates
  )

  # Specify other variables
  ww_data_dir <- file.path("input", "ww_data", "monday_datasets")
  scenario_dir <- file.path("input", "config", "eval", "scenarios")
  hosp_data_dir <- file.path("input", "hosp_data", "vintage_datasets")
  # stan_models_dir <- system.file("stan", package = "cfaforecastrenewalww") #nolint
  stan_models_dir <- file.path("cfaforecastrenewalww", "inst", "stan")
  init_dir <- file.path("input", "init_lists")

  ww_data_mapping <- "Monday: Monday, Wednesday: Monday"
  calibration_time <- 90
  forecast_time <- 28

  iter_warmup <- 750
  iter_sampling <- 500
  n_chains <- 4
  n_parallel_chains <- 4
  adapt_delta <- 0.95
  max_treedepth <- 12
  seed <- 123

  init_fps <- c()
  for (i in 1:n_chains) {
    init_fps <- c(init_fps, file.path(init_dir, glue::glue("init_{i}.json")))
  }

  # Pre-specified delay distributions
  generation_interval <- read.csv(here::here(
    "input", "saved_pmfs",
    "generation_interval.csv"
  )) |>
    dplyr::pull(probability_mass)
  inf_to_hosp <- read.csv(here::here(
    "input", "saved_pmfs",
    "inf_to_hosp.csv"
  )) |>
    dplyr::pull(probability_mass)

  config <- list(
    location_ww = df_ww |> dplyr::pull(location) |> as.vector(),
    forecast_date_ww = df_ww |> dplyr::pull(forecast_date) |> as.vector(),
    scenario = df_ww |> dplyr::pull(scenario) |> as.vector(),
    location_hosp = df_hosp |> dplyr::pull(location) |> as.vector(),
    forecast_date_hosp = df_hosp |> dplyr::pull(forecast_date) |> as.vector(),
    eval_date = eval_date,
    ww_data_dir = ww_data_dir,
    scenario_dir = scenario_dir,
    hosp_data_dir = hosp_data_dir,
    stan_models_dir = stan_models_dir,
    init_dir = init_dir,
    init_fps = init_fps,
    calibration_time = calibration_time,
    forecast_time = forecast_time,
    ww_data_mapping = ww_data_mapping,
    # MCMC settings
    iter_warmup = iter_warmup,
    iter_sampling = iter_sampling,
    n_chains = n_chains,
    n_parallel_chains = n_parallel_chains,
    adapt_delta = adapt_delta,
    max_treedepth = max_treedepth,
    seed = seed,
    # Input delay distributions
    generation_interval = generation_interval,
    infection_feedback_pmf = generation_interval,
    inf_to_hosp = inf_to_hosp
  )

  cfaforecastrenewalww::create_dir(config_dir)
  yaml::write_yaml(config, file = file.path(
    config_dir,
    glue::glue("eval_config.yaml")
  ))

  return(config)
}

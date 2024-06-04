#' Write evaluation config file
#'
#' @param locations locations to iterate through, for a full run this should
#' be all 50 states + PR
#' @param forecast_dates the forecast dates we want to run the model on
#' @param scenatios the scenarios (which will pertain to site ids) to
#' run the model on
#' @param config_dir the directory where we want to save the config file
#' @param scenario_dir the directory where the files defining scenarios
#' (default `.tsv` format) are located
#' @param eval_date the data of the evaluation dataset, in ISO YYYY-MM-DD format
#'
#' @return
#' @export
#'
#' @examples
write_eval_config <- function(locations, forecast_dates,
                              scenarios,
                              config_dir,
                              scenario_dir,
                              eval_date,
                              overwrite_summary_table) {
  # Will need to load in the files corresponding to the input scenarios, so we
  # get the list of locations that are relevant for each scenario. We will bind
  # these all together to create the full eval config.
  df_ww <- data.frame(row.names = c("location", "forecast_date", "scenario"))

  # This is a "manual" way of generating the dataframe we need to pass to targets
  # It does not handle the case of missing wastewater data.
  for (i in seq_along(scenarios)) {
    if (scenarios[i] == "status_quo") {
      locs <- locations
    } else {
      scenario_df <- read.table(file.path(
        scenario_dir,
        glue::glue("{scenarios[i]}.tsv")
      ), header = TRUE)
      locs <- scenario_df |>
        dplyr::filter(wwtp_jurisdiction %in% !!locations) |>
        dplyr::pull(wwtp_jurisdiction) |>
        unique()
    }

    df_i <- expand.grid(
      location = locs,
      forecast_date = forecast_dates,
      scenario = scenarios[i]
    )

    df_ww <- rbind(df_ww, df_i)
  }

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
  population_data_path <- file.path("input", "locations.csv")
  baseline_score_table_dir <- file.path("output", "baseline_score")
  # stan_models_dir <- system.file("stan", package = "cfaforecastrenewalww") #nolint
  stan_models_dir <- file.path("cfaforecastrenewalww", "inst", "stan")
  init_dir <- file.path("input", "init_lists")
  output_dir <- file.path("output", "eval")
  figure_dir <- file.path("output", "eval", "plots")
  hub_subdir <- file.path("output", "eval", "hub")
  score_subdir <- file.path("output", "eval", "hub")
  hub_model_names <- c(
    "COVIDhub-4_week_ensemble", "UMass-trends_ensemble",
    "UT-Osiris", "COVIDhub-baseline"
  )
  raw_output_dir <- file.path(output_dir, "raw_output")
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

  # Table of hospital admissions outliers by location-forecast-date-admissions-date:
  # This is currently fake/a test. We will replace with a load in to a path
  # to a saved csv eventually.
  table_of_exclusions <- data.frame(
    location = c("TX", "TX", "TX", "TX"),
    forecast_date = "2024-02-26",
    dates_to_exclude = c("2024-01-30", "2024-01-31", "2024-02-01", "2024-02-02")
  )

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
    baseline_score_table_dir = baseline_score_table_dir,
    output_dir = output_dir,
    hub_subdir = hub_subdir,
    score_subdir = score_subdir,
    raw_output_dir = raw_output_dir,
    figure_dir = figure_dir,
    hub_model_names = hub_model_names,
    population_data_path = population_data_path,
    init_dir = init_dir,
    init_fps = init_fps,
    overwrite_summary_table = overwrite_summary_table,
    calibration_time = calibration_time,
    forecast_time = forecast_time,
    ww_data_mapping = ww_data_mapping,
    table_of_exclusions = table_of_exclusions,
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

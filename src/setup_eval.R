library(argparser)

default_locations <- forecasttools::us_location_table |>
  dplyr::filter_out(.data$abbr %in% c("GU", "MP", "UM", "VI", "AS", "US")) |>
  dplyr::pull("abbr") # 50 states, DC, PR


default_forecast_dates <- as.character(
  seq(
    from = lubridate::ymd("2023-10-16"),
    to = lubridate::ymd("2024-03-25"),
    by = "week"
  )
)

#' Write evaluation config file
#'
#' @param locations locations to iterate through, for a full run this should
#' be all 50 states + PR
#' @param forecast_dates the forecast dates we want to run the model on
#' @param scenatios the scenarios (which will pertain to site ids) to
#' run the model on
#' @param scenario_dir the directory where the files defining scenarios
#' (default `.tsv` format) are located
#' @param eval_date the data of the evaluation dataset, in ISO YYYY-MM-DD format
#' @param output_dir name of the directory in which to save output
#' @param param_file Path to the from which to read priors and other
#' configuration not in this config.
#' @param wwinference_version Character string indicating the version
#' of the wwinference model being run. Default's to the version on disk.
#'
#' @return
#' @export
#'
#' @examples
write_eval_config <- function(
  config_path,
  locations,
  forecast_dates,
  scenarios,
  config_dir,
  scenario_dir,
  eval_date,
  output_dir,
  param_file,
  wwinference_version = sessioninfo::package_info(
    "wwinference",
    dependencies = FALSE
  )$source # nolint
) {
  forecast_dates <- as.Date(forecast_dates)
  first_real_time_forecast_date <- max(
    min(forecast_dates),
    lubridate::ymd("2024-02-05")
  )
  last_real_time_forecast_date <- min(
    max(forecast_dates),
    lubridate::ymd("2025-04-29")
  )

  df_ww <- data.frame(
    row.names = c("location", "forecast_date", "scenario")
  )

  # This is a "manual" way of generating the dataframe we need to pass to targets
  # It does not handle the case of missing wastewater data.
  for (i in seq_along(scenarios)) {
    if (scenarios[i] == "status_quo") {
      locs <- locations
    } else {
      scenario_df <- read.table(
        file.path(
          scenario_dir,
          glue::glue("{scenarios[i]}.tsv")
        ),
        header = TRUE
      )
      locs <- scenario_df |>
        dplyr::filter(
          wwtp_jurisdiction %in% !!locations
        ) |>
        dplyr::pull(wwtp_jurisdiction) |>
        unique()
    }

    df_i <- expand.grid(
      location = locs,
      forecast_date = as.character(forecast_dates),
      scenario = scenarios[i]
    )

    df_ww <- rbind(df_ww, df_i)
  }

  # No scenarios for the hosp admissions only, so we just need all combos
  # of locations and forecast dates
  df_hosp <- expand.grid(
    location = locations,
    forecast_date = as.character(forecast_dates)
  )

  # Specify other variables
  ww_data_dir <- file.path("input", "ww_data", "monday_datasets")
  scenario_dir <- file.path("input", "config", "eval", "scenarios")
  hosp_data_dir <- file.path("input", "hosp_data", "vintage_datasets")
  population_data_path <- file.path("input", "locations.csv")
  real_time_metadata_dir <- file.path("output", "forecasts")
  figure_dir <- file.path(output_dir, "plots")
  hub_subdir <- file.path(output_dir, "hub")
  retro_rt_path <- file.path("input", "retro_Rt", "Rt_draws.parquet")
  score_subdir <- file.path(output_dir, "scores")
  min_submissions_hub <- 20
  min_locs_per_submission_hub <- 40
  min_paired_forecasts_per_jurisdiction <- 4 #nolint
  min_paired_forecasts_per_date <- 20
  raw_output_dir <- file.path(output_dir, "raw_output")
  ww_data_mapping <- "Monday: Monday, Wednesday: Monday"
  calibration_time <- 90
  forecast_time <- 28
  trend_lookback_days <- 14 + 9
  # two weeks from the last admission date

  scoring_offset <- 1
  forecast_log_diff_offset <- scoring_offset

  iter_warmup <- 750
  iter_sampling <- 500
  n_chains <- 4
  n_parallel_chains <- 4
  adapt_delta <- 0.95
  max_treedepth <- 12
  seed <- 123

  # Pre-specified delay distributions
  generation_interval <- wwinference::default_covid_gi

  inf_to_hosp <- wwinference::default_covid_inf_to_hosp

  ## no retro data exclusions
  table_of_exclusions <- tibble::tibble(
    location = c(),
    forecast_date = c(),
    dates_to_exclude = c()
  )

  real_time_output_dir <- file.path("output", "real_time_outputs")
  table_of_run_ids <- tibble::tibble(
    ids = c(
      "b84a4",
      "5ebc5",
      "bb0b4",
      "a6e67",
      "f86b2",
      "8150f",
      "235d1",
      "6aa44"
    ),
    forecast_date = seq(
      from = first_real_time_forecast_date,
      to = last_real_time_forecast_date,
      by = "week"
    ) |>
      as.character(),
    dates_run = c(
      "2024-02-05",
      "2024-02-12",
      "2024-02-18",
      "2024-02-25",
      "2024-03-02",
      "2024-03-09",
      "2024-03-16",
      "2024-03-23"
    )
  )

  # These come from the yaml files we saved in the forecast folders,
  date_locs_exclude_ww_retro <- tibble::tibble(
    location = c("MN", "MN", "MN"),
    forecast_date = c(
      "2024-01-15",
      "2024-01-22",
      "2024-01-29"
    )
  )

  config <- list(
    scored_forecast_dates = as.character(forecast_dates),
    first_real_time_forecast_date = as.character(
      first_real_time_forecast_date
    ),
    last_real_time_forecast_date = as.character(
      last_real_time_forecast_date
    ),
    location_ww = df_ww |> dplyr::pull(location) |> as.vector(),
    forecast_date_ww = df_ww |>
      dplyr::pull(forecast_date) |>
      as.vector(),
    scenario = df_ww |> dplyr::pull(scenario) |> as.vector(),
    location_hosp = df_hosp |> dplyr::pull(location) |> as.vector(),
    forecast_date_hosp = df_hosp |>
      dplyr::pull(forecast_date) |>
      as.vector(),
    eval_date = eval_date,
    ww_data_dir = ww_data_dir,
    scenario_dir = scenario_dir,
    hosp_data_dir = hosp_data_dir,
    output_dir = output_dir,
    hub_subdir = hub_subdir,
    param_file = param_file,
    wwinference_version = wwinference_version,
    min_submissions_hub = min_submissions_hub,
    min_locs_per_submission_hub = min_locs_per_submission_hub,
    min_paired_forecasts_per_jurisdiction = min_paired_forecasts_per_jurisdiction,
    min_paired_forecasts_per_date = min_paired_forecasts_per_date,
    retro_rt_path = retro_rt_path,
    score_subdir = score_subdir,
    raw_output_dir = raw_output_dir,
    figure_dir = figure_dir,
    real_time_metadata_dir = real_time_metadata_dir,
    population_data_path = population_data_path,
    calibration_time = calibration_time,
    forecast_time = forecast_time,
    ww_data_mapping = ww_data_mapping,
    table_of_exclusions = table_of_exclusions,
    table_of_run_ids = table_of_run_ids,
    real_time_output_dir = real_time_output_dir,
    date_locs_exclude_ww_retro = date_locs_exclude_ww_retro,
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
    inf_to_hosp = inf_to_hosp,
    trend_hosp_lookback_days = trend_lookback_days,
    trend_ww_lookback_days = trend_lookback_days,
    scoring_offset = scoring_offset,
    forecast_log_diff_offset = forecast_log_diff_offset
  )

  config_path |>
    fs::path_dir() |>
    fs::dir_create()

  yaml::write_yaml(config, file = config_path)

  return(config)
}


parsed <- arg_parser(
  "Generate a YAML configuration file for an evaluation run."
) |>
  add_argument(
    "output_dir",
    help = "Output directory for the evaluation run."
  ) |>
  add_argument(
    "config_path",
    help = "Path to save the config."
  ) |>
  add_argument(
    "--priors",
    help = "Path to a TOML file containing prior parameters for the evaluation run.",
    default = "input/priors/params_default.toml"
  ) |>
  parse_args()


write_eval_config(
  config_path = parsed$config_path,
  locations = default_locations,
  forecast_dates = default_forecast_dates,
  scenarios = "status_quo",
  scenario_dir = file.path("input", "config", "eval", "scenarios"),
  eval_date = "2025-03-10",
  output_dir = parsed$output_dir,
  param_file = parsed$priors
)

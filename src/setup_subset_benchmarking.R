# This is a script used to generate the global inputs needed to run the
# _targets_subset_benchmarking.R function. Editing the locations and
# forecast dates and model types tells the evaluation pipeline which
# combinations to run and post process.
# We will select a subset of locations and forecast dates to benchmark on.
source(file.path("src", "write_eval_config.R"))
write_eval_config(
  locations =
    c(
      "AK", "MA", "NJ", "NH", "WA"
    ),
  forecast_dates =
    as.character(
      seq(
        from = lubridate::ymd("2023-10-16"),
        to = lubridate::ymd("2024-03-11"),
        by = "4 weeks"
      )
    ),
  scenarios = c(
    "status_quo"
  ),
  config_dir = file.path("input", "config", "eval"),
  benchmark_dir = file.path("output", "benchmarking"),
  scenario_dir = file.path("input", "config", "eval", "scenarios"),
  ms_fig_dir = file.path("output", "eval", "plots", "manuscript"),
  eval_date = "2024-04-29",
  overwrite_summary_table = FALSE, # Set as TRUE if trying to get a baseline
  # score for all locations one forecast date
  overwrite_benchmark = TRUE, # Set as TRUE if want to save outputs of
  # benchmarking in directory,
  name_of_config = "benchmark_config",
  wwinference_version = "v0.1.0"
)

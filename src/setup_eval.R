# This is a script used to generate the global inputs needed to run the
# _targets_eval.R function. Editing the locations and forecast dates
# and model types tells the evaluation pipeline which combinations to
# pass to tar map to iterate over.
source(file.path("src", "write_eval_config.R"))
write_eval_config(
  locations =
    c(
      "AK", "AL", "AR", "AZ", "CA",
      "CO", "CT", "DC", "DE", "FL", "GA", "ND",
      "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA",
      "MD", "ME", "MI", "MN", "MO", "MS", "MT", "NC",
      "NE", "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR",
      "PA", "PR", "RI", "SC", "SD", "TN", "TX", "UT", "VA",
      "VT", "WA", "WI", "WV", "WY"
    ),
  forecast_dates =
    as.character(
      seq(
        from = lubridate::ymd("2023-10-16"),
        to = lubridate::ymd("2024-03-11"),
        by = "week"
      )
    ),
  scenarios = c(
    "status_quo"
  ),
  config_dir = file.path("input", "config", "eval"),
  benchmark_dir = file.path("output", "benchmarking"),
  scenario_dir = file.path("input", "config", "eval", "scenarios"),
  eval_date = "2024-04-29",
  overwrite_summary_table = FALSE, # Set as TRUE if trying to get a baseline
  # score for all locations one forecast date
  overwrite_benchmark = TRUE # Set as TRUE if want to save outputs of
)

# This is a script used to generate the global inputs needed to run the
# _targets_eval.R function. Editing the locations and forecast dates
# and model types tells the evaluation pipeline which combinations to
# pass to tar map to iterate over.
source(file.path("src", "write_eval_config.R"))
write_eval_config(
  locations = c("MA", "WA"),
  forecast_dates = c(
    "2023-10-16", "2023-10-23", "2023-10-30",
    "2023-11-06", "2023-11-13", "2023-11-20",
    "2023-11-27", "2023-12-04", "2023-12-11",
    "2023-12-18", "2023-12-25", "2024-01-01",
    "2024-01-08", "2024-01-15", "2024-01-22",
    "2024-01-29", "2024-02-05", "2024-02-12",
    "2024-02-19"
  ),
  scenarios = c("Status quo", "One site per jurisdiction"),
  config_dir = file.path("input", "config", "eval"),
  eval_date = "2024-03-25"
)

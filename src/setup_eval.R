# This is a script used to generate the global inputs needed to run the
# _targets_eval.R function. Editing the locations and forecast dates
# and model types tells the evaluation pipeline which combinations to
# pass to tar map to iterate over.
source(file.path("src", "write_eval_config.R"))
write_eval_config(
  locations = c(
    "AK", "AL", "AR", "AZ", "CA",
    "CO", "CT", "DC", "DE", "FL", "GA", "ND",
    "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA",
    "MD", "ME", "MI", "MN", "MO", "MS", "MT", "NC",
    "NE", "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR",
    "PA", "PR", "RI", "SC", "SD", "TN", "TX", "UT", "VA",
    "VT", "WA", "WI", "WV", "WY"
  ),
  forecast_dates =
    c(
      "2023-10-23", "2023-11-20", # nolint
      "2023-12-18", "2024-01-15", "2024-02-12", # nolint
      "2024-03-11"
    ), # nolint
  scenarios = c("status_quo", "coes", "hhs_regions", "msas", "national"), # nolint
  config_dir = file.path("input", "config", "eval"),
  scenario_dir = file.path("input", "config", "eval", "scenarios"),
  eval_date = "2024-04-15"
)

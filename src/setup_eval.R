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
  forecast_dates = c(
    "2023-10-16", "2023-10-30", "2023-11-06", "2023-11-13",
    "2023-11-27", "2023-12-04", "2023-12-11", "2023-12-25",
    "2024-01-01", "2024-01-08", "2024-01-22", "2024-01-29",
    "2024-02-05", "2024-02-19", "2024-02-26", "2024-03-04"
  ),
  scenarios = c("coe_proxy", "coe_regional", "max_40", "fed_funded"), # nolint
  config_dir = file.path("input", "config", "eval"),
  scenario_dir = file.path("input", "config", "eval", "scenarios"),
  eval_date = "2024-04-29"
)

# Scratch file to run postprocessing locally

# Get missing files
table_missing <- readr::read_csv(file.path(
  "output", "eval",
  "files_missing", "ww",
  "hosp_quantiles.csv"
))

loc_vec <- table_missing$location
forecast_date_vec <- as.character(lubridate::ymd(table_missing$forecast_date))
scenario_vec <- table_missing$scenario

# Make a new config

# Load in the eval_config
eval_config <- yaml::read_yaml(file.path(
  "input", "config",
  "eval", "eval_config_rerun.yaml"
))

config_dir <- file.path("input", "config", "eval")

new_eval_config <- eval_config
new_eval_config$location_ww <- loc_vec
new_eval_config$forecast_date_ww <- forecast_date_vec
new_eval_config$scenario <- scenario_vec
new_eval_config$location_hosp <- "MA"
new_eval_config$forecast_date_hosp <- "2023-10-16"


yaml::write_yaml(new_eval_config, file = file.path(
  config_dir,
  glue::glue("eval_config_rerun.yaml")
))

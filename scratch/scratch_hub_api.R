# Scratch file to prototype using zoltr API instead of pulling into memory from github

library(zoltr)
library(cfaforecastrenewalww)

truth_data_path <- "https://media.githubusercontent.com/media/reichlab/covid19-forecast-hub/master/data-truth/truth-Incident%20Hospitalizations.csv" # nolint
truth_data <- truth_data <- readr::read_csv(truth_data_path)
cfaforecastrenewalww::setup_secrets("secrets.yaml")


zoltar_connection <- new_connection()
zoltar_authenticate(zoltar_connection, get_secret("Z_USERNAME"), get_secret("Z_PASSWORD"))
zoltar_connection

# list of project on zoltar
the_projects <- projects(zoltar_connection)
str(the_projects)

# Grabbing a specific project
project_url <- the_projects[the_projects$name == "COVID-19 Forecasts", "url"]
the_project_info <- project_info(zoltar_connection, project_url)
names(the_project_info)

# get the models
the_models <- models(zoltar_connection, project_url)
str(the_models)

# Submit query, poll job, get job data

forecast_data <- do_zoltar_query(
  zoltar_connection = zoltar_connection,
  project_url = project_url,
  query_type = "forecasts",
  models = NULL, # "cfa-wwrenewal",
  units = c("06", "08041"), # would need state codes
  targets = NULL,
  types = "quantile",
  timezeros = seq(from = ymd("2023-10-16"), to = ymd("2024-03-11"), by = "week")
)

n_unique_forecasts <- forecast_data |>
  dplyr::distinct(timezero) |>
  dplyr::pull() |>
  length()

n_forecasts_per_model <- forecast_data |>
  dplyr::distinct(timezero, model) |>
  dplyr::group_by(model) |>
  dplyr::summarize(
    n_forecast_dates = dplyr::n(),
    prop_present = n_forecast_dates / n_unique_forecasts
  )

models <- n_forecasts_per_model |>
  dplyr::filter(prop_present > 0.85) |>
  dplyr::pull(model)

see_missing_dates <- forecast_data |>
  dplyr::filter(model %in% c("MUNI-ARIMA", "PSI-DICE")) |>
  dplyr::distinct(timezero, model)





# Their example
forecast_data <- do_zoltar_query(
  zoltar_connection,
  "https://www.zoltardata.com/api/project/44/",
  "forecasts",
  models = c("CMU-TimeSeries", "UMass-MechBayes"),
  units = c("US"),
  targets = c("1 wk ahead inc death"),
  types = c("quantile")
  # as_of = "2020-07-10" #nolint
)

## Try with Getting Started
project_url <- the_projects[the_projects$name == "Docs Example Project", "url"]
the_project_info <- project_info(zoltar_connection, project_url)
names(the_project_info)

the_models <- models(zoltar_connection, project_url)
str(the_models)

query <- list("targets" = list("pct next week", "cases next week"), "types" = list("point"))
job_url <- submit_query(zoltar_connection, project_url, "forecasts", query)
busy_poll_job(zoltar_connection, job_url)
the_job_data <- job_data(zoltar_connection, job_url)
the_job_data

forecast_data <- do_zoltar_query(zoltar_connection, project_url, "forecasts", "docs_mod",
  c("loc1", "loc2"), c("pct next week", "cases next week"),
  c("2011-10-02", "2011-10-09", "2011-10-16"),
  types = c("point", "quantile")
)

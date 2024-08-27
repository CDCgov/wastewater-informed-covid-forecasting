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

forecast_data <- do_zoltar_query(zoltar_connection,
  project_url,
  "forecasts",
  models = NULL,
  units = NULL,
  targets = NULL,
  types = "quantile",
  as_of = "2024-02-05"
)

forecast_data <- do_zoltar_query(
  zoltar_connection,
  "https://www.zoltardata.com/api/project/44/",
  "forecasts",
  models = c("CMU-TimeSeries", "UMass-MechBayes"),
  units = c("01003", "US"),
  targets = c("1 wk ahead inc death"),
  types = c("quantile"),
  as_of = "2020-07-10"
)

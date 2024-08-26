# Scratch file to prototype using zoltr API instead of pulling into memory from github

library(zoltr)

truth_data_path <- "https://media.githubusercontent.com/media/reichlab/covid19-forecast-hub/master/data-truth/truth-Incident%20Hospitalizations.csv" # nolint
truth_data <- truth_data <- readr::read_csv(truth_data_path)
cfaforecastrenewalww::setup_secrets("secrets.yaml")

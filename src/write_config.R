#' @title Get configuration values for a single model run
#' @description This function creates the list that determines the parameters of
#' the model run(s). This will be used to create the datasets and groupings to
#' be passed to the targets, as well as the other settings for the model runs.
#' @param location list of locations to fit to
#' @param geo_type geographic type of the location
#' @param forecast_date list of forecast dates to run model on
#' @param include_ww list of whether or not to include WW (1 = yes, 0 = no)
#' @param hosp_reporting_delay list of time difference between forecast date
#' and last observed hospital admission
#' @return list of objects of various lengths corresponding to the metadata
#' @export
#'
#' @examples
write_config <- function(run_id,
                         date_run,
                         forecast_date,
                         location,
                         save_config = TRUE,
                         config_path = file.path("input", "config"),
                         geo_type = "state",
                         include_ww = 1,
                         hosp_reporting_delay = 4,
                         ww_data_source = "NWSS",
                         hosp_data_source = "HHS_protect_vintages",
                         hosp_data_dir = file.path(
                           "input", "hosp_data",
                           "vintage_datasets"
                         ),
                         train_data_dir = file.path(
                           "input", "train_data"
                         ),
                         population_data_path = file.path(
                           "input",
                           "locations.csv"
                         ),
                         param_file_path = file.path("input", "params.toml"),
                         prod_run = FALSE, # TRUE if running for submission
                         ww_data_path = file.path(
                           "input", "ww_data", "nwss_data",
                           "2024-01-11.csv"
                         ),
                         ww_geo_type = "site",
                         ww_target_type = "pcr_target_avg_conc",
                         ww_data_type = "pop_weighted_conc_w_thres",
                         pull_from_local = FALSE,
                         model_type = "state-level aggregated wastewater",
                         submitting_model_name = "cfa-wwrenewal") {
  if (is.null(location)) {
    if (model_type == "state-level aggregated wastewater") {
      location <- c(
        "AK", "AL", "AR", "AZ", "CA",
        "CO", "CT", "DC", "DE", "FL", "GA", "GU", "ND",
        "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA",
        "MD", "ME", "MI", "MN", "MO", "MS", "MT", "NC",
        "NE", "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR",
        "PA", "PR", "RI", "SC", "SD", "TN", "TX", "UT", "VA",
        "VT", "WA", "WI", "WV", "WY", "US"
      )
    } else {
      # put NY back in
      location <- c(
        "AK", "AL", "AR", "AZ", "CA",
        "CO", "CT", "DC", "DE", "FL", "GA", "ND",
        "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA",
        "MD", "ME", "MI", "MN", "MO", "MS", "MT", "NC",
        "NE", "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR",
        "PA", "PR", "RI", "SC", "SD", "TN", "TX", "UT", "VA",
        "VT", "WA", "WI", "WV", "WY"
      )
    }
  }

  # More transient vals
  forecast_date <- lubridate::ymd(forecast_date)
  date_run <- as.character(lubridate::ymd(date_run))
  calibration_time <- 90
  forecast_time <- 28
  n_draws <- 100 # draws from the posterior to save


  output_dir <-
    file.path(
      "output", forecast_date,
      stringr::str_glue("run-on-{date_run}-{run_id}")
    ) # nolint
  path_to_save_metadata <-
    file.path(output_dir, "pipeline_run_metadata")

  # Pre-specified delay distributions
  generation_interval <- read.csv(here::here(
    "input", "saved_pmfs",
    "generation_interval.csv"
  )) |>
    dplyr::pull(probability_mass)
  inf_to_hosp <- read.csv(here::here(
    "input", "saved_pmfs",
    "inf_to_hosp.csv"
  )) |>
    dplyr::pull(probability_mass)

  # Assign the dates and locations for hospital admissions to be manually
  # removed
  dates_for_hosp_removal <- c(seq(
    from = lubridate::ymd("2024-01-30"),
    to = lubridate::ymd("2024-02-02"),
    by = "days"
  ))

  states_for_hosp_removal <- c(rep(
    "TX",
    4
  ))

  # Assign the dates and locations for wastewater data to be removed manually
  first_date <- forecast_date - lubridate::days(calibration_time) - hosp_reporting_delay
  dates_for_ww_removal <- c(seq(
    from = first_date,
    to = lubridate::ymd("2023-12-31"),
    by = "days"
  ))

  states_for_ww_removal <- c(rep(
    "MN",
    length(dates_for_ww_removal)
  ))


  # How to fit the model
  compute_likelihood <- 1 # if want to use data, else just priors
  include_hosp <- 1 # if want to use hosp
  iter_warmup <- 1000
  iter_sampling <- 750
  n_chains <- 4
  n_parallel_chains <- 4
  adapt_delta <- 0.99
  max_treedepth <- 12
  seed <- 123
  model_file_name <- get_model_file_name(model_type, include_ww)

  if (prod_run == TRUE) {
    full_file_path <- file.path(
      config_path, "prod",
      glue::glue("{forecast_date}-run-on-{date_run}")
    )
  } else {
    full_file_path <- file.path(
      config_path, "test",
      glue::glue("{forecast_date}-run-on-{date_run}")
    )
  }

  fp <- file.path(
    full_file_path,
    glue::glue("config-{model_file_name}-{run_id}.yaml")
  )

  config <- list(
    forecast_date = as.character(lubridate::ymd(forecast_date)),
    location = location,
    run_id = run_id,
    date_run = date_run,
    calibration_time = calibration_time,
    forecast_time = forecast_time,
    hosp_reporting_delay = hosp_reporting_delay,
    geo_type = geo_type,
    hosp_data_source = hosp_data_source,
    train_data_dir = train_data_dir,
    param_file_path = param_file_path,
    ww_data_source = ww_data_source,
    compute_likelihood = compute_likelihood,
    include_ww = include_ww,
    include_hosp = include_hosp,
    iter_warmup = iter_warmup,
    iter_sampling = iter_sampling,
    n_chains = n_chains,
    n_parallel_chains = n_parallel_chains,
    adapt_delta = adapt_delta,
    max_treedepth = max_treedepth,
    seed = seed,
    n_draws = n_draws,
    output_dir = output_dir,
    ww_data_path = ww_data_path,
    hosp_data_dir = hosp_data_dir,
    population_data_path = population_data_path,
    ww_data_type = ww_data_type,
    ww_target_type = ww_target_type,
    ww_geo_type = ww_geo_type,
    pull_from_local = pull_from_local,
    model_type = model_type,
    dates_for_hosp_removal = as.character(lubridate::ymd(dates_for_hosp_removal)),
    states_for_hosp_removal = states_for_hosp_removal,
    dates_for_ww_removal = as.character(lubridate::ymd(dates_for_ww_removal)),
    states_for_ww_removal = states_for_ww_removal,
    generation_interval = generation_interval,
    infection_feedback_pmf = generation_interval,
    inf_to_hosp = inf_to_hosp,
    submitting_model_name = submitting_model_name,
    prod_run = prod_run,
    path_to_save_metadata = path_to_save_metadata,
    config_file_path = fp
  )


  if (save_config == TRUE) {
    create_dir(full_file_path)
    yaml::write_yaml(config, file = file.path(
      full_file_path,
      glue::glue("config-{model_file_name}-{run_id}.yaml")
    ))
  }


  return(fp)
}

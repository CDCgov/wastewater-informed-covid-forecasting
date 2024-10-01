#' Get input wastewater data
#'
#' @param forecast_date_i The forecast date for this iteration,
#' formatted as a character string in IS08601 format (YYYY-MM-DD).
#' @param location_i The location (state or other jurisdiction)
#' for this iteration, formatted as a string (uppercase USPS two-letter
#' abbreviation, e.g. AK for Alaska, DC for the District of Columbia,
#' PR for Puerto Rico).
#' @param scenario_i The scenario for this iteration, formatted as a
#' string
#' @param scenario_dir A string indicating the path to the directory
#' containing the csvs with the wwtp ids needed for each scenario
#' @param ww_data_dir  A string indicating the path to the directory
#' containing time stamped wastewater datasets
#' @param calibration_time The duration of the model calibration period
#' (relative to the last hospital admissions data point) in units of
#' model timesteps (typically days).
#' @param last_hosp_data_date A date indicating the date of last reported
#' hospital admission as of the forecast date
#' @param ww_data_mapping A string indicating how to map the
#' forecast date to the wastewater dates (see [date_of_ww_data()]
#' for more details)
#'
#' @return a tibble containing the pre-processed wastewater data ready to
#' be passed into the wwinference function
#' @export
get_input_ww_data <- function(forecast_date_i,
                              location_i,
                              scenario_i,
                              scenario_dir,
                              ww_data_dir,
                              calibration_time,
                              last_hosp_data_date,
                              ww_data_mapping) {
  # Load in the appropriate time-stamped NWSS dataset. This depends on
  # the date `ww_data_mapping` which is a string that we will specify
  # in the config
  date_to_pull <- date_of_ww_data(
    forecast_date_i, ww_data_mapping,
    ww_data_dir
  )

  ww_data_path <- file.path(ww_data_dir, paste0(date_to_pull, ".csv"))
  raw_nwss_data <- readr::read_csv(ww_data_path, show_col_types = FALSE)

  # Use wweval functions to subset NWSS data and format for wwinference package
  all_ww_data <- raw_nwss_data |>
    clean_and_filter_nwss_data()
  # Get the data corresponding to the scenario
  list_of_site_ids <- get_scenario_site_ids(
    all_ww_data,
    scenario_i,
    scenario_dir
  )
  subsetted_ww_data <- all_ww_data |>
    dplyr::filter(wwtp_name %in% !!list_of_site_ids)

  ww_data_pkg <- subsetted_ww_data |>
    clean_ww_data() |>
    filter(
      location %in% c(!!location_i),
      date >= lubridate::ymd(!!last_hosp_data_date) -
        lubridate::days(!!calibration_time) + lubridate::days(1)
    )

  deduplicated_data <- ww_data_pkg |>
    dplyr::group_by(lab, site, date) |>
    dplyr::summarize(
      log_genome_copies_per_ml = mean(log_genome_copies_per_ml),
      log_lod = mean(log_lod),
      site_pop = mean(site_pop)
    ) |>
    dplyr::ungroup()

  ww_data_preprocessed <- wwinference::preprocess_ww_data(
    deduplicated_data,
    conc_col_name = "log_genome_copies_per_ml",
    lod_col_name = "log_lod"
  )
  ww_data_to_fit <- wwinference::indicate_ww_exclusions(
    ww_data_preprocessed,
    outlier_col_name = "flag_as_ww_outlier",
    remove_outliers = TRUE
  )


  return(ww_data_to_fit)
}

#' Get scenario site ids
#'
#' @param init_subset_nwss_data a dataframe of the raw NWSS data
#' filtered to exclude solids and upstream sites
#' @param scenario a string indicating what scenario we are running.
#' Default is "status_quo" which uses all the data we have available
#' @param scenario_dir a string indicating the file path where the
#' scenario csvs will live. Default is NA because we don't need this
#' for the status_quo scenario
#'
#' @return a dataframe that only contains the ww data from
#' the sites in the list pertaining to the scenario
#' @export
get_scenario_site_ids <- function(init_subset_nwss_data,
                                  scenario = "status_quo",
                                  scenario_dir = NA) {
  list_of_wwtp_ids <- unique(init_subset_nwss_data$wwtp_name)
  if (scenario != "status_quo") {
    list_of_wwtp_ids <- read.table(
      file.path(
        scenario_dir,
        glue::glue("{scenario}.tsv")
      ),
      header = TRUE
    ) |>
      pull(wwtp_name) |>
      unique() # There could be duplicates here if multiple site ids
    # had the same WWTP name (e.g. labs switched but represents same pop)
  }

  return(list_of_wwtp_ids)
}


#' Get input hospital admissions data
#'
#' @param forecast_date_i The forecast date for this iteration
#' @param location_i The location (state) for this iteration
#' @param hosp_data_dir A string indicating the path to the directory containing
#'  time stamped hospital admissions datasets
#' @param calibration_time  A numeric indicating the duration of model
#' calibration (based on the last hospital admissions data point)
#' @param load_from_epidatr boolean indicating whether or not the hospital
#' admissions datasets should be loaded directly from epidatr.
#' `default = FALSE` because we are assuming that we have already created a
#' folder with time stamped datasets
#' @param population_data_path path to a table of state populations, default is
#' `NULL`, only needed if pulling from epidatr
#'
#' @return a tibble containing the preprocessed hospital admissions data ready
#' to be passed into the wwinference function
#' @export
get_input_hosp_data <- function(forecast_date_i, location_i,
                                hosp_data_dir, calibration_time,
                                load_from_epidatr = FALSE,
                                population_data_path = NA) {
  fp <- file.path(hosp_data_dir, paste0(forecast_date_i, ".csv"))

  # Load in the appropriate time-stamped hospital admissions dataset
  if (isTRUE(load_from_epidatr)) {
    # These codechunk depends on the epidatr package
    check_package_is_installed("epidatr")
    options(covidcast.auth = get_secret("covidcast_api_key"))

    hosp_raw <- quiet(epidatr::pub_covidcast(
      source = "hhs",
      signals = "confirmed_admissions_covid_1d",
      geo_type = "state",
      time_type = "day",
      geo_values = "*",
      time_values = "*",
      as_of = forecast_date_i
    ))

    state_population_table <- readr::read_csv(population_data_path) |>
      dplyr::mutate(population = as.numeric(population))

    hosp <- hosp_raw |>
      as_tibble() |>
      mutate(abbreviation = toupper(geo_value)) |>
      left_join(state_population_table, by = "abbreviation") |>
      rename(
        date = time_value,
        daily_hosp_admits = value,
        pop = population
      ) |>
      select(date, ABBR = abbreviation, daily_hosp_admits, pop)
    message("Writing full time stamped dataset to local storage")

    readr::write_csv(hosp, fp)
  } else {
    hosp <- readr::read_csv(fp)
  }
  last_hosp_data_date <- max(hosp$date, na.rm = TRUE)
  input_hosp <- hosp |>
    rename(location = ABBR) |>
    filter(
      location %in% c(!!location_i),
      date >= (
        ymd(!!last_hosp_data_date) -
          lubridate::days(!!calibration_time) +
          lubridate::days(1)
      )
    )

  hosp_data_preprocessed <- wwinference::preprocess_count_data(
    input_hosp,
    count_col_name = "daily_hosp_admits",
    pop_size_col_name = "pop"
  )
  return(hosp_data_preprocessed)
}

#' Date of wastewater data
#'
#' @param forecast_date the forecast date for this iteration
#' @param ww_data_mapping a string that tells this function how to pick
#' data pull dates from forecast dates. This function needs to be configured
#' for each new string.
#' @param ww_data_dir A string indicating the path to the directory containing
#'  time stamped wastewater datasets
#'
#' @return the date to get the ww data from
#' @export
date_of_ww_data <- function(forecast_date, ww_data_mapping,
                            ww_data_dir) {
  if (is.null(ww_data_mapping)) {
    dates <- gsub(".{4}$", "", list.files(ww_data_dir))
    # Get the nearest date less than the forecast date
    date_to_pull <- as.character(max(dates[dates < ymd(forecast_date)], na.rm = TRUE))
  } else if (ww_data_mapping == "Monday: Monday, Wednesday: Monday") {
    # Error if mapping is Monday to Wednesday and forecast date is neither
    stopifnot(
      "Forecast date is not a Monday or Wednesday" =
        lubridate::wday(forecast_date) == 2 || lubridate::wday(forecast_date) == 4
    )

    if (lubridate::wday(forecast_date) == 2) {
      date_to_pull <- as.character(ymd(forecast_date))
    } else if (lubridate::wday(forecast_date) == 4) {
      date_to_pull <- as.character(
        ymd(forecast_date) - lubridate::days(2)
      )
    }
  } else { # Anything else right now we don't have algorithm written for,
    # so prompt
    date_to_pull <- NA
  }

  stopifnot(
    "Need to write case to specify which wastewater data to pull" =
      !is.na(date_to_pull)
  )

  return(date_to_pull)
}

#' Get last hospital admissions data point date
#'
#' @param input_hosp the hospital admissions dataset for that location and
#' forecast date
#'
#' @return a date indicating the last day of observed data
#' @export
get_last_hosp_data_date <- function(input_hosp) {
  last_hosp_data_date <- max(input_hosp$date, na.rm = TRUE)
  return(last_hosp_data_date)
}


#' Clean wastewater data
#'
#' @param nwss_subset the raw nwss data filtered down to only the columns we use
#'
#' @return A site-lab level dataset with names and variables that can be used
#' for model fitting
#' @export
clean_ww_data <- function(nwss_subset) {
  ww_data <- nwss_subset |>
    ungroup() |>
    rename(
      date = sample_collect_date,
      site_pop = population_served
    ) |>
    mutate(
      location = toupper(wwtp_jurisdiction),
      site = wwtp_name,
      lab = lab_id,
      log_genome_copies_per_ml = log(pcr_target_avg_conc + 1e-8),
      log_lod = log(lod_sewage)
    ) |>
    select(
      date, site, lab, log_genome_copies_per_ml,
      log_lod, site_pop, location
    )

  return(ww_data)
}

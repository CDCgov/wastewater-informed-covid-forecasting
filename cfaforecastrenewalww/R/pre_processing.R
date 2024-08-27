#' Local functions that are used to pre-process our specific data for use
#' in the stan model
#' These will be used to get the data up to the point it needs to be in the
#' get_stan_data functions, which format the data into the stan lists that the
#' stan model needs.
#' None of these will be exported

## Hospitalization data pre-processing -------------------------------------
#' Get forecast date from today's date
#' @export
get_nearest_forecast_date <- function() {
  today_weekday <- lubridate::wday(lubridate::today(tzone = "EST"))
  today_date <- lubridate::today(tzone = "EST")

  forecast_date <- case_when(
    today_weekday == 6 ~ as.character(today_date + days(3)), # if Fri the Mon
    today_weekday == 7 ~ as.character(today_date + days(2)), # if Sat then Mon
    today_weekday == 1 ~ as.character(today_date + days(1)), # if Sun then Mon
    today_weekday == 2 ~ as.character(today_date), # if Mon then Mon
    today_weekday == 3 ~ as.character(today_date - days(1)), # if Tues then Mond
    today_weekday == 4 ~ as.character(today_date), # if Wed the Wed
    today_weekday == 5 ~ as.character(today_date - days(1)) # if Thurs then Wed
  )

  return(forecast_date)
}

#' Get the hospital reporting delay
#' @description
#' This is purely based on our knowledge of the NHSN data reporting, as we
#' pre-specify before loading the most recent data..
#'
#' @param forecast_date The date the forecast is made
#'
#' @return hosp_reporting delay The delay from forecast date to last hospital
#' admission date
#' @export
#'
#' @examples
get_hosp_reporting_delay <- function(forecast_date) {
  hosp_reporting_delay <-
    case_when(
      lubridate::wday(forecast_date) %in% c(1, 2, 3, 7) ~ 9, # Sun Mon Tues Wed
      lubridate::wday(forecast_date) %in% c(4, 5, 6) ~ 4
    ) # Wed Thurs Fri

  return(hosp_reporting_delay)
}

#' @title Get raw hospitalization data
#' @description
#' This function takes in a hospitalization data source and a forecast date
#' and generates a dataframe with state-level hospitalizations from all time
#' points as of that forecast date.
#' If the vintaged data available as of a forecast date already exists, it pulls
#' from that, otherwise it pulls from hosp data source and saves the file.
#'
#' @param hosp_data_source covidcast or another source (e.g. if pulling directly
#' from DCIPHER or the S drive, will need to write relevant paths for this)
#' @param forecast_date date we want the data as of (as if we were making a
#' forecast on that day)
#' @param hosp_data_dir if pulling from local, which directory
#' to search for datestamped hospitalization data
#' @param population_data_path path to a table of state populations
#' @param pull_from_local  if TRUE, look for local files first,
#' default value is FALSE
#'
#' @return dataframe containing number of hospital admissions by state
#'
#' @export
#'
#' @examples
get_state_level_hosp_data <- function(hosp_data_source,
                                      forecast_date,
                                      hosp_data_dir,
                                      population_data_path,
                                      pull_from_local = FALSE) {
  state_population_table <- readr::read_csv(population_data_path) %>%
    dplyr::mutate(population = as.numeric(population))
  hosp_data_local_path <- file.path(
    hosp_data_dir,
    paste0(as.character(forecast_date), ".csv")
  )

  if (isTRUE(pull_from_local) && file.exists(hosp_data_local_path)) {
    cli::cli_inform(
      c(
        "Pulling file from local for",
        "complete hospitalization data"
      )
    )

    hosp <- readr::read_csv(hosp_data_local_path)
  } else {
    # Raw state level hospital data
    if (hosp_data_source == "covidcast") {
      # These codechunk depends on the epidatr package
      check_package_is_installed("epidatr")
      options(covidcast.auth = get_secret("covidcast_api_key"))

      # Get hospital admissions data by state
      hosp_raw <- epidatr::pub_covidcast(
        source = "hhs",
        signals = "confirmed_admissions_covid_1d",
        geo_type = "state",
        time_type = "day",
        geo_values = "*",
        time_values = "*"
      )
      stopifnot(
        "Hospitalization data more than 13 days delayed" =
          max(hosp_raw$time_value) > lubridate::today() - 13
      )
    }

    if (hosp_data_source == "NHSN") {
      # Get hospital admissions data by state from forecastools
      api_key_id <- get_secret("NHSN_API_KEY_ID")
      api_key_secret <- get_secret("NHSN_API_KEY_SECRET")

      hosp_raw <- pull_nhsn(
        columns = c(
          "state", "date",
          "previous_day_admission_adult_covid_confirmed",
          "previous_day_admission_pediatric_covid_confirmed"
        ),
        start_date = "2023-01-01",
        api_key_id = api_key_id,
        api_key_secret = api_key_secret
      ) %>%
        dplyr::rename(
          geo_value = state
        ) %>%
        dplyr::mutate(
          value = as.numeric(previous_day_admission_adult_covid_confirmed) +
            as.numeric(previous_day_admission_pediatric_covid_confirmed),
          time_value = lubridate::as_date(date) - lubridate::days(1) # Assign admissions to the
          # previous day
        ) %>%
        dplyr::select(-date)
    }

    if (hosp_data_source == "HHS_protect_vintages") {
      hosp_raw <- quiet(epidatr::pub_covidcast(
        source = "hhs",
        signals = "confirmed_admissions_covid_1d",
        geo_type = "state",
        time_type = "day",
        geo_values = "*",
        time_values = "*",
        as_of = forecast_date
      ))
    }

    hosp <- hosp_raw %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(abbreviation = toupper(geo_value)) %>%
      dplyr::left_join(state_population_table, by = "abbreviation") %>%
      dplyr::rename(
        date = time_value,
        daily_hosp_admits = value,
        pop = population
      ) %>%
      dplyr::select(date, ABBR = abbreviation, daily_hosp_admits, pop)

    if (hosp_data_source == "HHS_protect_vintages") {
      # Then we want to write this to a file
      create_dir(hosp_data_dir)

      readr::write_csv(
        hosp,
        file = file.path(
          hosp_data_dir, paste0(as.character(lubridate::ymd(forecast_date)), ".csv")
        )
      )
    } else if (hosp_data_source == "NHSN") {
      # Then we want to write this to a file with the date run time stamped to it
      create_dir(hosp_data_dir)

      readr::write_csv(
        hosp,
        file = file.path(
          hosp_data_dir, paste0(as.character(lubridate::today()), ".csv")
        )
      )
    }
  } # End if pull_from_local = TRUE

  return(hosp)
}


#' Gets hospitalization data that model will be trained on
#'
#' @param hosp_data_source Source of hospitalization data
#' from which to pull. Options include "NHSN" and "covidcast".
#' @param geo_type type of geographic region for which to pull data
#' @param forecast_date date for which we are producing a forecast
#' @param hosp_data_dir if pulling from local, where to get the data from
#' @param population_data_path path to a table of state populations
#' @param pull_from_local  if TRUE, look for local files first,
#' default value is FALSE
#'
#'
#' @return a data frame at the geo_type level of number of daily hospital admits
#'
#' @export
#'
#' @examples
get_hosp_data <- function(hosp_data_source,
                          geo_type,
                          forecast_date,
                          hosp_data_dir,
                          population_data_path,
                          pull_from_local = FALSE) {
  state_hosp_data <- get_state_level_hosp_data(
    hosp_data_source,
    forecast_date,
    hosp_data_dir,
    population_data_path,
    pull_from_local = pull_from_local
  )

  if (geo_type == "region") {
    regions <- get_regions_for_mapping()

    hosp <- state_hosp_data %>%
      dplyr::inner_join(regions, by = "ABBR") %>%
      # summarize populations and admissions over the week
      dplyr::filter(!is.na(daily_hosp_admits)) %>%
      group_by(date, region) %>% # value = daily new admissionsin region
      summarize(across(c(daily_hosp_admits, pop), sum), .groups = "drop") %>%
      # add in week, defined by wednesdays like in Biobot data
      mutate(week = round_date(date, "week", week_start = 3)) %>%
      rename(location = region)
  }

  if (geo_type == "state") {
    hosp <- state_hosp_data %>%
      dplyr::filter(!is.na(daily_hosp_admits)) %>%
      mutate(week = round_date(date, "week", week_start = 3)) %>%
      rename(location = ABBR)
  }

  # National hosp
  state_population_table <- readr::read_csv(population_data_path) %>%
    dplyr::mutate(population = as.numeric(population))

  usa_hosp <- state_hosp_data %>%
    dplyr::filter(!is.na(daily_hosp_admits)) %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(daily_hosp_admits = sum(daily_hosp_admits)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      week = round_date(date, "week", week_start = 3),
      abbreviation = "US"
    ) %>%
    dplyr::inner_join(state_population_table, by = "abbreviation") %>%
    dplyr::select(
      date,
      location = abbreviation,
      daily_hosp_admits,
      pop = population,
      week
    )

  # Bind US hospitalizations to the end
  hosp <- rbind(hosp, usa_hosp)

  return(hosp)
}

#' Get dataframe with wastewater and hospitalizaiton data for training the model
#'
#' @param ww_data_raw
#' @param hosp_data_raw
#' @param forecast_date
#' @param calibration_time
#' @param location
#' @param hosp_reporting_delay
#' @param forecast_time
#'
#' @return a dataframe of daily hospital admits, observed wastewater
#' concentrations, time starting at the start of the calibration period and
#' going til the end of the forecast period, the day of the week, period, date,
#' and location
#'
#' @export
#'
#' @examples
get_training_data <- function(ww_data_raw,
                              hosp_data_raw,
                              forecast_date,
                              calibration_time,
                              location,
                              hosp_reporting_delay,
                              forecast_time) {
  # Pull the hospitalization data
  # This will either grab the data from the latest covidcast data
  # or will use the forecast date in config vars if vitnages is specified

  if (forecast_date %in% unique(hosp_data_raw$date)) {
    closest_date <- forecast_date
  } else {
    closest_date <- max(hosp_data_raw$date)
  }


  full_dates <- data.frame(date = seq(
    from = min(hosp_data_raw$date),
    to = max(forecast_date, max(hosp_data_raw$date)), by = "days"
  ))

  df_intermediate <- tidyr::expand_grid(
    date = seq(
      from = min(hosp_data_raw$date),
      to = max(forecast_date, max(hosp_data_raw$date)), by = "days"
    ),
    location = unique(hosp_data_raw$location)
  ) %>%
    dplyr::left_join(
      hosp_data_raw %>%
        group_by(location) %>%
        dplyr::filter(date == closest_date) %>%
        dplyr::ungroup() %>%
        dplyr::select(pop, location),
      by = "location"
    )

  hosp_data_expanded <- df_intermediate %>%
    left_join(hosp_data_raw %>% select(-pop),
      by = c("date", "location")
    )

  comb <- hosp_data_expanded %>%
    left_join(ww_data_raw, by = c("date", "location"))

  selected_location <- location
  last_hosp_data_date <- min(
    forecast_date - days(hosp_reporting_delay),
    max(hosp_data_raw$date)
  )

  comb <- comb %>% dplyr::filter(
    date > lubridate::ymd(last_hosp_data_date - days(calibration_time)),
    date <= lubridate::ymd(forecast_date + days(forecast_time)),
    location %in% selected_location
  )

  # make sure you have values for all dates
  date_df <- data.frame(
    date = seq(
      from = min(comb$date),
      to = max(comb$date), by = "days"
    ),
    t = seq(
      from = 1,
      to = max(comb$date) - min(comb$date) + 1
    )
  )

  train_data <- date_df %>%
    left_join(comb, by = c("date")) %>%
    mutate(
      daily_hosp_admits = ifelse(is.na(daily_hosp_admits),
        0, daily_hosp_admits
      ),
      day_of_week = lubridate::wday(date, week_start = 1),
      forecast_date = forecast_date
    ) %>%
    dplyr::filter(
      date <= (forecast_date + days(forecast_time)),
    ) %>%
    # Fill data with NAs if during nowcast/calibration time!
    dplyr::mutate(daily_hosp_admits = ifelse(
      date <= last_hosp_data_date,
      daily_hosp_admits,
      NA
    )) %>%
    dplyr::ungroup()

  return(train_data)
}

#' Get testing dataset
#'
#' @param ww_data_raw The WW data that we need alongside
#' the testing hospitalization data (WW data doesn't change)
#' @param config_vars The config vars that tell us the
#' forecast date, locations, data sources, hosp reporting
#' delays, etc
#' @param date_as_of The date that we want to evaluate the
#' projections as of. This could be today, or could be some
#' weeks in the future
#' @return A dataset that goes up until the forecast_date +
#' forecast_time and starts when the calibration started
#' for each location. Labels whether the data is calibration,
#' nowcast, or forecasted. This will be for multiple locations,
#' but only for a single model type, forecast date, and
#' hospitalizaiton reporting delay
#'
#' @export
#'
#' @examples
get_testing_data <- function(data_as_of,
                             forecast_date,
                             location,
                             calibration_time,
                             forecast_time,
                             hosp_reporting_delay,
                             geo_type,
                             hosp_data_source,
                             pull_from_local,
                             hosp_data_dir,
                             population_data_path) {
  ## Pull the latest hospitalization data
  ## If source is HHS_protect_vintages, will
  ## pull from data as of otherwise pulls
  ## the latest data. Use forecast date input
  ## but set to more recent date
  hosp_data_raw <- get_hosp_data(
    hosp_data_source,
    geo_type,
    data_as_of,
    hosp_data_dir,
    population_data_path,
    pull_from_local
  )

  if (forecast_date %in% unique(hosp_data_raw$date)) {
    closest_date <- forecast_date
  } else {
    closest_date <- max(hosp_data_raw$date)
  }

  full_dates <- data.frame(date = seq(
    from = min(hosp_data_raw$date),
    to = max(forecast_date, max(hosp_data_raw$date)), by = "days"
  ))
  df_intermediate <- tidyr::expand_grid(
    date = seq(
      from = min(hosp_data_raw$date),
      to = max(forecast_date + days(forecast_time), max(hosp_data_raw$date)), by = "days"
    ),
    location = unique(hosp_data_raw$location)
  ) %>%
    dplyr::left_join(
      hosp_data_raw %>%
        group_by(location) %>%
        dplyr::filter(date == closest_date) %>%
        dplyr::ungroup() %>%
        dplyr::select(pop, location),
      by = "location"
    ) # grabs one population per location

  hosp_data_expanded <- df_intermediate %>%
    dplyr::left_join(
      hosp_data_raw %>% select(-pop),
      by = c("date", "location")
    )

  comb <- hosp_data_expanded
  calibration_time <- as.numeric(calibration_time)
  selected_location <- location
  last_hosp_data_date <- forecast_date - days(hosp_reporting_delay)

  comb <- comb %>%
    dplyr::filter(
      date > lubridate::ymd(!!last_hosp_data_date - days(!!calibration_time)),
      location %in% !!selected_location
    )

  # make sure you have values for all dates
  date_df <- data.frame(
    date = seq(
      from = min(comb$date),
      to = max(comb$date), by = "days"
    ),
    t = seq(
      from = 1,
      to = max(comb$date) - min(comb$date) + 1
    )
  )

  test_data <- date_df %>%
    dplyr::left_join(comb, by = c("date")) %>%
    dplyr::select(-t) %>%
    dplyr::mutate(
      day_of_week = lubridate::wday(date, week_start = 1)
    ) %>%
    dplyr::filter(
      date <= (forecast_date + days(forecast_time)),
    ) %>%
    dplyr::rename(daily_hosp_admits_for_eval = daily_hosp_admits)

  return(test_data)
}



#' Get long tibble of data needed for model fitting
#'
#' @param ww_data_raw
#' @param forecast_date
#' @param location
#' @param calibration_time
#' @param geo_type
#' @param pull_from_local
#' @param hosp_data_dir
#' @param population_data_path
#' @param forecast_time
#' @param hosp_reporting_delay
#' @param ww_geo_type
#' @param hosp_data_source
#' @param include_ww
#'
#' @return a long stacked dataframe with columns corresponding to model specs
#' and data specs
#'
#' @export
#'
#' @examples
get_all_training_data <- function(ww_data_raw,
                                  forecast_date,
                                  location,
                                  calibration_time,
                                  geo_type,
                                  pull_from_local,
                                  hosp_data_dir,
                                  population_data_path,
                                  forecast_time,
                                  hosp_reporting_delay,
                                  ww_geo_type,
                                  hosp_data_source,
                                  include_ww,
                                  train_data_dir,
                                  model_type,
                                  write_files = TRUE,
                                  ...) {
  # These config vars contain multiple elements for forecast_date,
  # hosp data delay, and whether or not to include ww

  # Load in a single set of hospitalization and ww data that is up to date
  # This way we only call the covidcast API twice.
  test_data <- get_testing_data(
    data_as_of = lubridate::today(),
    forecast_date = lubridate::today(),
    location,
    calibration_time = as.numeric(lubridate::today() - lubridate::ymd("2020-01-01")),
    forecast_time,
    hosp_reporting_delay = 0,
    geo_type,
    hosp_data_source,
    pull_from_local,
    hosp_data_dir,
    population_data_path
  )

  # First get just the training data, which should be the length of the
  # calibration time.
  for (i in seq_along(forecast_date)) {
    single_forecast_date <- forecast_date[i]
    # Grab the hospitalization data as of the forecast date
    hosp_data <- get_hosp_data(
      hosp_data_source,
      geo_type,
      single_forecast_date,
      hosp_data_dir,
      population_data_path,
      pull_from_local
    )

    for (j in seq_along(hosp_reporting_delay)) {
      for (k in seq_along(include_ww)) {
        single_hosp_reporting_delay <- hosp_reporting_delay[j]
        single_include_ww <- include_ww[k]


        # If the WW data is at the state level do aggregation here:
        if (ww_geo_type == "state") {
          ww_data <- aggregate_ww(
            ww_data_raw, forecast_date, calibration_time,
            hosp_reporting_delay
          )
        } else {
          ww_data <- ww_data_raw # no further aggregation needed
        }

        single_group_train_data <- get_training_data(
          ww_data,
          hosp_data,
          single_forecast_date,
          calibration_time,
          location,
          single_hosp_reporting_delay,
          forecast_time
        )

        last_hosp_data_date <- single_group_train_data %>%
          dplyr::filter(!is.na(daily_hosp_admits)) %>%
          dplyr::pull(date) %>%
          max()

        # get the test data in the exact format we need for joining
        test_data_single_group <- test_data %>%
          mutate(forecast_date = single_forecast_date) %>%
          dplyr::filter(
            date > lubridate::ymd(lubridate::ymd(last_hosp_data_date) - days(calibration_time)),
            date <= lubridate::ymd(lubridate::ymd(single_forecast_date) + days(forecast_time))
          )

        single_group_train_data <- test_data_single_group %>%
          left_join(
            single_group_train_data %>% select(
              -pop, -day_of_week, -week,
              -forecast_date, -t
            ) %>%
              distinct(),
            by = c("date", "location")
          ) %>%
          mutate(
            forecast_date = single_forecast_date,
            hosp_reporting_delay = single_hosp_reporting_delay,
            include_ww = single_include_ww,
            period = case_when(
              date <= last_hosp_data_date ~ "calibration",
              (date > last_hosp_data_date & date <= single_forecast_date) ~ "nowcast",
              date > forecast_date ~ "forecast"
            )
          )

        # make sure you have values for all dates
        date_df <- data.frame(
          date = seq(
            from = min(single_group_train_data$date),
            to = max(single_group_train_data$date), by = "days"
          ),
          t = seq(
            from = 1,
            to = max(single_group_train_data$date) - min(single_group_train_data$date) + 1
          )
        )

        single_group_train_data <- single_group_train_data %>%
          left_join(date_df, by = "date")

        if (i == 1 && j == 1 && k == 1) {
          train_data <- single_group_train_data
        } else {
          train_data <- rbind(train_data, single_group_train_data)
        }
      }
    }
  }


  if (ww_geo_type == "site") {
    site_map <- train_data %>%
      group_by(location, forecast_date) %>%
      select(site, location, forecast_date) %>%
      dplyr::filter(!is.na(site)) %>%
      distinct() %>%
      mutate(site_index = row_number())
    train_data <- train_data %>%
      left_join(site_map, by = c("site", "location", "forecast_date"))

    site_lab_map <- train_data %>%
      group_by(location, forecast_date) %>%
      select(lab_wwtp_unique_id, location, forecast_date) %>%
      dplyr::filter(!is.na(lab_wwtp_unique_id)) %>%
      distinct() %>%
      mutate(lab_site_index = row_number())
    train_data <- train_data %>%
      left_join(site_lab_map, by = c("lab_wwtp_unique_id", "location", "forecast_date"))
  }

  if (isTRUE(write_files)) {
    model_file_name <- get_model_file_name(model_type, include_ww)
    fp <- file.path(train_data_dir, forecast_date, model_file_name)
    create_dir(fp)
    readr::write_csv(train_data, file.path(fp, "train_data.csv"))
  }
  return(train_data)
}





## WW data pre-processing, used for site level data------------------------

#' Save time stampped NWSS data
#'
#' @param ww_path_to_save filepath to save the time stamped wastewater datasets
#' default is data/ww_data
#' keys
#'
#' @return ww_data_path
#' @export
#'
#' @examples
save_timestamped_nwss_data <- function(ww_path_to_save) {
  hour <- lubridate::hour(Sys.time())
  minute <- lubridate::minute(Sys.time())

  time <- hour + minute / 60
  if (time <= (21 + 5 / 60)) { # Hacky way of saying we want the data to be labeled
    # as data from the previous day if NWSS data not updated (before 9:05)
    time_stamp <- as.character(lubridate::today() - lubridate::days(1))
  } else {
    time_stamp <- as.character(lubridate::today())
  }
  ww_data_path <- file.path(
    ww_path_to_save,
    glue::glue("{time_stamp}.csv")
  )

  if (file.exists(ww_data_path)) {
    message("Loading ww data from local storage")
  } else {
    check_package_is_installed("httr")

    # Load in the nwss data token and RID
    token <- get_secret("nwss_data_token")
    rid <- get_secret("data_rid")

    url <- paste0(
      "https://dcipher.cdc.gov/api/v1/datasets/", rid,
      "/readTable?preview=True&format=ARROW"
    )


    raw_result <- httr::GET(
      url,
      httr::add_headers(Authorization = token)
    )
    if (raw_result$status_code == 401) {
      warning(
        "Attempt to read data from API has resulted in a 401 Unauthorized status code. ",
        "The \"nwss_data_token\" may be expired."
      )
    }

    arrow_stream <- httr::content(raw_result, "raw")
    temp_file <- tempfile(fileext = ".arrow")
    writeBin(arrow_stream, temp_file)
    message("Loading in NWSS data from API, time stamping to ", time_stamp)
    arrow_table <- arrow::read_ipc_stream(temp_file)

    nwss_data <- arrow_table
    create_dir(ww_path_to_save)

    write_csv(nwss_data, file.path(
      ww_path_to_save,
      paste0(time_stamp, ".csv")
    ))
  }

  return(ww_data_path)
}

#' Subsample wastewater sites
#'
#' @param ww_data Site-level wastewater data, extracted and trnsformed from
#' the raw NWSS data to contain only the relevant columns. This will be for all
#' time points
#' @param prop_sites Proportion of all sites that we want to keep in the input
#' data. default is 0.2
#' @param sampled_sites The list of sites that you want to keep, so that we
#' can compare the exact same subsample if desired. Default is NULL, then
#' random sampling is done according to the proportion
#'
#' @return a data frame structured the same way as the ww_data but with only
#' prop_sites included. These sites are chosen randomly
#' @export
#'
#' @examples
subsample_sites <- function(ww_data, prop_sites = 0.2,
                            sampled_sites = NULL) {
  if (is.null(sampled_sites)) {
    site_list <- ww_data %>%
      select(site) %>%
      unique() %>%
      filter(!is.na(site)) %>%
      pull()
    n_sites <- length(site_list)

    sampled_sites <- sample(site_list, max(1, round(prop_sites * n_sites)))
  }

  ww_data_subsampled <- ww_data %>% filter(site %in% c(sampled_sites))

  return(ww_data_subsampled)
}


#' Initial subsetting of NWSS data
#' @description
#' Grab the columns we want and subset to raw wastewater and to only
#' Wastewater treatment plants (rather than downstream sites, for now).
#' Transform concentration to copies per mL
#'
#'
#' @param raw_nwss_data nwss data from nwss
#'
#' @return nwss_subset_raw which just dplyr::filters based on sample types and wwtp and
#' returns a subset of the columns
#'
#' @export
#'
#' @examples
init_subset_nwss_data <- function(raw_nwss_data) {
  nwss_subset_raw <- raw_nwss_data %>%
    dplyr::filter(
      sample_location == "wwtp",
      sample_matrix != "primary sludge",
      pcr_target_units != "copies/g dry sludge",
      pcr_target == "sars-cov-2"
    ) %>%
    #* Note, we need to figure out how to convert copies/g dry sludge to a WW concentration,
    #* but now now we're just going to exclude
    select(
      lab_id, sample_collect_date, wwtp_name, pcr_target_avg_conc,
      wwtp_jurisdiction, county_names, population_served, pcr_target_units,
      pcr_target_below_lod, lod_sewage, quality_flag, county_names
    ) %>%
    mutate(
      pcr_target_avg_conc = case_when(
        pcr_target_units == "copies/l wastewater" ~ pcr_target_avg_conc / 1000,
        pcr_target_units == "log10 copies/l wastewater" ~ (10^(pcr_target_avg_conc)) / 1000
      ),
      lod_sewage = case_when(
        pcr_target_units == "copies/l wastewater" ~ lod_sewage / 1000,
        pcr_target_units == "log10 copies/l wastewater" ~ (10^(lod_sewage)) / 1000
      ),
    ) %>%
    dplyr::filter(!quality_flag %in% c(
      "yes", "y", "result is not quantifiable",
      "temperature not assessed upon arrival at the laboratory",
      "> max temp and/or hold time"
    ))

  # will treat data without LOD as uninformative
  conservative_lod <- as.numeric(
    quantile(nwss_subset_raw$lod_sewage, 0.95, na.rm = TRUE)
  )
  nwss_subset_raw <- nwss_subset_raw %>%
    mutate(lod_sewage = ifelse(is.na(lod_sewage),
      conservative_lod,
      lod_sewage
    )) %>%
    mutate(below_LOD = case_when(
      pcr_target_below_lod == "Yes" ~ 1,
      pcr_target_below_lod == "No" ~ 0,
      pcr_target_avg_conc < lod_sewage ~ 1,
      pcr_target_avg_conc >= lod_sewage ~ 0,
      round(pcr_target_avg_conc) == 0 ~ 1
    )) %>%
    mutate( # if value below LOD, set at LOD
      pcr_target_avg_conc = ifelse(below_LOD == 1, lod_sewage, pcr_target_avg_conc)
    )

  unique_combos_map <- nwss_subset_raw %>%
    select(wwtp_name, lab_id) %>%
    unique() %>%
    mutate(lab_wwtp_unique_id = row_number())

  nwss_subset <- nwss_subset_raw %>%
    left_join(unique_combos_map,
      by = c("wwtp_name", "lab_id")
    ) %>%
    mutate(sample_collect_date = lubridate::ymd(sample_collect_date))

  # Find the number of datapoints in each lab site (exclude rows
  # where lab_wwtp_unique_id is NA bc those are days with no WW obs)
  n_dps <- nwss_subset %>%
    group_by(lab_wwtp_unique_id) %>%
    summarise(n_data_points = n())
  # Remove labs that have only 1 data point
  nwss_subset <- nwss_subset %>%
    left_join(n_dps, by = "lab_wwtp_unique_id") %>%
    dplyr::filter(n_data_points > 1)

  # If there are multiple values per lab-site-day, replace with the mean
  nwss_subset_no_repeats <- nwss_subset %>%
    group_by(lab_wwtp_unique_id, sample_collect_date) %>%
    summarise(
      pcr_target_avg_conc = mean(pcr_target_avg_conc, na.rm = TRUE),
      population_served = mean(population_served, na.rm = TRUE),
      county_names = county_names[which.max(nchar(county_names))] # mult entries
      # for a site-lab-day will list different countys, pick the most inclusive one
    )

  # Get only one value per lab-site-day combo
  nwss_subset_clean <- nwss_subset %>%
    dplyr::select(
      -pcr_target_avg_conc,
      -population_served,
      -county_names
    ) %>%
    dplyr::distinct() %>%
    dplyr::left_join(nwss_subset_no_repeats,
      by = c(
        "lab_wwtp_unique_id",
        "sample_collect_date"
      )
    ) %>%
    dplyr::select(colnames(nwss_subset))

  return(nwss_subset_clean)
}


#' Summarize data into weekly by site
#' @description
#' Get one sample per site per week by averaging over the week if a site submits
#' more than once per week
#'
#' @param nwss_subset subsetted nwss data
#'
#' @return weekly summary of nwss data by site
#'
#' @export
#'
#' @examples
get_weekly_summary <- function(nwss_subset, ww_target_type = "pcr_target_avg_conc") {
  nwss_subset <- nwss_subset %>% mutate(
    week = epiweek(lubridate::ymd(sample_collect_date)),
    year = lubridate::year(lubridate::ymd(sample_collect_date)),
    sample_collect_date = lubridate::ymd(sample_collect_date),
    midweek_date = round_date(lubridate::ymd(sample_collect_date), "week", week_start = 3)
  )

  if (ww_target_type == "pcr_target_avg_conc") {
    nwss_by_week <- nwss_subset %>%
      dplyr::group_by(wwtp_name, midweek_date) %>%
      dplyr::summarise(
        site_weekly_avg_conc = mean(pcr_target_avg_conc,
          na.rm = TRUE
        )
      ) %>%
      dplyr::left_join(
        nwss_subset %>%
          dplyr::select(
            county_names,
            population_served,
            wwtp_jurisdiction,
            wwtp_name,
            midweek_date
          ) %>%
          dplyr::distinct(),
        by = c("midweek_date", "wwtp_name")
      )
  }
  if (ww_target_type == "pcr_target_flowpop_lin") {
    nwss_by_week <- nwss_subset %>%
      dplyr::group_by(wwtp_name, midweek_date) %>%
      dplyr::summarise(
        site_weekly_avg_conc = mean(pcr_target_flowpop_lin,
          na.rm = TRUE
        )
      ) %>%
      dplyr::left_join(
        nwss_subset %>% dplyr::select(
          county_names,
          population_served,
          wwtp_jurisdiction,
          wwtp_name,
          midweek_date
        ) %>%
          dplyr::distinct(),
        by = c("midweek_date", "wwtp_name")
      )
  }

  return(nwss_by_week)
}

#' Get summary of WW viral concentrations by state
#' @description
#' This uses the same processing steps that biobot uses (described in:
#' https://github.com/biobotanalytics/covid19-wastewater-data#wastewater-data)
#' We make columns for the unweighted avg concentraiton across the state,
#' the population weighted average, the population weighted average with a
#' 300,000 person threshold for site-specific population served, and a 3 week
#' rolling average of the weighted concentration with the threshold, for both
#' the state and national level. If there are gaps in weeks for a state,
#' the 3 week average is across a longer time frame. We purposely do not use
#' expand grid to inflate this, as we want those states with missing/ incomplete
#' data to be missing data when the model is fit (rather than have NA values).
#'
#'
#' @param nwss_by_week
#'
#' @return
#'
#' @export
#'
#' @examples
get_state_level_summary <- function(nwss_by_week) {
  nwss_by_state <- nwss_by_week %>%
    dplyr::group_by(
      wwtp_jurisdiction,
      midweek_date
    ) %>%
    dplyr::summarise(
      pop_weighted_conc = sum(site_weekly_avg_conc * population_served) /
        sum(population_served),
      unweighted_avg_conc = mean(site_weekly_avg_conc),
      pop_weighted_conc_w_thres = sum(site_weekly_avg_conc * pmin(
        3e5,
        population_served
      )) /
        sum(pmin(3e5, population_served))
    ) %>%
    dplyr::mutate(
      rlng_avg_pop_weighted_conc_w_thres = rollmean(
        pop_weighted_conc_w_thres,
        3,
        fill = NA,
        align = "center"
      )
    ) %>%
    dplyr::left_join(
      nwss_by_week %>%
        dplyr::group_by(midweek_date) %>%
        dplyr::summarise(
          ntl_pop_weighted_conc = sum(site_weekly_avg_conc * population_served,
            na.rm = TRUE
          ) / sum(population_served),
          ntl_unweighted_avg_conc = mean(site_weekly_avg_conc),
          ntl_pop_weighted_conc_w_thres = sum(site_weekly_avg_conc * pmin(
            3e5,
            population_served
          ), na.rm = TRUE) / sum(pmin(3e5, population_served),
            na.rm = TRUE
          )
        ) %>%
        dplyr::mutate(
          rlng_avg_ntl_pop_weighted_conc_w_thres = rollmean(
            ntl_pop_weighted_conc_w_thres,
            3,
            fill = NA,
            align = "center"
          )
        ),
      by = "midweek_date"
    )

  nwss_usa <- nwss_by_week %>%
    dplyr::group_by(midweek_date) %>%
    dplyr::summarise(
      pop_weighted_conc = sum(site_weekly_avg_conc * population_served) /
        sum(population_served),
      unweighted_avg_conc = mean(site_weekly_avg_conc),
      pop_weighted_conc_w_thres = sum(site_weekly_avg_conc *
        pmin(
          3e5,
          population_served
        )) / sum(pmin(
        3e5,
        population_served
      ))
    ) %>%
    dplyr::mutate(
      wwtp_jurisdiction = "us",
      rlng_avg_pop_weighted_conc_w_thres = rollmean(
        pop_weighted_conc_w_thres,
        3,
        fill = NA,
        align = "center"
      )
    ) %>%
    dplyr::left_join(
      nwss_by_week %>%
        dplyr::group_by(midweek_date) %>%
        dplyr::summarise(
          ntl_pop_weighted_conc = sum(
            site_weekly_avg_conc *
              population_served,
            na.rm = TRUE
          ) / sum(population_served),
          ntl_unweighted_avg_conc = mean(
            site_weekly_avg_conc
          ),
          ntl_pop_weighted_conc_w_thres = sum(
            site_weekly_avg_conc *
              pmin(
                3e5,
                population_served
              ),
            na.rm = TRUE
          ) /
            sum(pmin(3e5, population_served),
              na.rm = TRUE
            )
        ) %>%
        dplyr::mutate(
          rlng_avg_ntl_pop_weighted_conc_w_thres = rollmean(
            ntl_pop_weighted_conc_w_thres,
            3,
            fill = NA,
            align = "center"
          )
        ),
      by = "midweek_date"
    ) %>%
    dplyr::select(colnames(nwss_by_state))

  nwss_by_state <- rbind(nwss_by_state, nwss_usa)

  return(nwss_by_state)
}

#' Get the watstewater data for a particular source
#' and geographic granularity
#' @description
#' Wrapper function around the worker functions.
#' Outputs a different dataframe depending on if
#' the pipeline is site level (ww_geo_type != geo_type)
#' or state level
#'
#' @param ww_data_source
#' @param geo_type
#' @param ww_data_type
#' @param ww_target_type
#' @param ww_geo_type
#' @param ww_data_path
#'
#' @return dataframe with observed viral concentrations at the geographic
#' level specified and weekly temporal granularity. Dates correspond to the
#' middle of the week. Will be used to map to hospitalization data.
#'
#' @export
#'
#' @examples
get_ww_data <- function(ww_data_source, geo_type, ww_data_type,
                        ww_target_type, ww_geo_type, ww_data_path,
                        dates_for_ww_removal = c(),
                        states_for_ww_removal = c(),
                        ...) {
  if (
    ww_data_source == "biobot" &&
      geo_type == "region" &&
      ww_geo_type == "region"
  ) {
    ww_region <- quiet(readr::read_csv(ww_data_path))

    ww_data <- ww_region %>%
      dplyr::rename(
        ww = effective_concentration_rolling_average
      ) %>%
      dplyr::filter(region != "Nationwide") %>%
      dplyr::rename(location = region) %>%
      dplyr::group_by(location, sampling_week) %>%
      dplyr::summarise(ww = mean(ww, na.rm = TRUE))
  }

  if (
    ww_data_source == "NWSS" &&
      geo_type == "state"
  ) {
    if (!file.exists(ww_data_path)) {
      print("NWSS data not in specified location")
    }
    nwss <- readr::read_csv(ww_data_path)
    nwss_subset <- init_subset_nwss_data(nwss)


    ww_data <- nwss_subset %>%
      ungroup() %>%
      rename(
        date = sample_collect_date,
        ww = {{ ww_target_type }},
        ww_pop = population_served
      ) %>%
      mutate(
        location = toupper(wwtp_jurisdiction),
        site = wwtp_name,
        lab = lab_id
        # since we might expect to see
      ) %>%
      select(
        date, location, ww, site, lab, lab_wwtp_unique_id, ww_pop,
        below_LOD, lod_sewage
      )


    for (i in seq_along(dates_for_ww_removal)) {
      to_remove <- (
        ww_data$location == states_for_ww_removal[i] &
          ww_data$date == lubridate::ymd(dates_for_ww_removal[i])
      )
      ww_data$ww[to_remove] <- NA
    }
    # Remove rows where ww data is NA
    ww_data <- ww_data %>% filter(!is.na(ww))



    # Duplicate all ww data with the location labeled US so it gets aggregated
    ww_data_usa <- ww_data %>%
      mutate(location = "US")

    ww_data <- rbind(ww_data, ww_data_usa)
  }


  return(ww_data)
}



#' @title: get states mapped to their regions
#'
#' @return state abbreviations and region that corr to the WW data source for
#' the biobot data
#'
#' @export
#'
#' @examples
get_regions_for_mapping <- function() {
  # Need regions if we're mapping to biobot data only
  # Biobot uses US Census regions, but puts Maryland in the Northeast
  regions <- tibble(
    ABBR = us_states_abb,
    region = case_when(
      ABBR %in% c("MD") ~ "Southwest",
      TRUE ~ as.character(recode(state.region, "North Central" = "Midwest"))
    )
  )
  return(regions)
}


#' Flag WW outliers
#'
#' @param ww_data data at the lab-site level of WW concentrations
#' @param rho_threshold z-score threshold for "jump"
#' @param log_conc_threshold z-score threshold for log concentration
#' @param threshold_n_dps min number of data points above the LOD per lab-site
#'
#' @return ww_data + columns for outlier flagging
#' @export
#'
#' @examples
flag_ww_outliers <- function(ww_data, rho_threshold = 2,
                             log_conc_threshold = 3,
                             threshold_n_dps = 1) {
  n_dps <- ww_data %>%
    dplyr::filter(below_LOD == 0) %>%
    group_by(lab_wwtp_unique_id) %>%
    summarise(n_data_points = n())

  # Get the ww statistics we need for outlier detection
  ww_stats <- ww_data %>%
    left_join(n_dps, by = "lab_wwtp_unique_id") %>%
    # exclude below LOD from z scoring and remove lab-sites with too few data points
    dplyr::filter(below_LOD == 0, n_data_points > threshold_n_dps) %>%
    group_by(lab_wwtp_unique_id) %>%
    arrange(date, "desc") %>%
    mutate(
      log_conc = log(ww),
      prev_log_conc = lag(log_conc, 1),
      prev_date = lag(date, 1),
      diff_log_conc = log_conc - prev_log_conc,
      diff_time = as.numeric(difftime(date, prev_date)),
      rho = diff_log_conc / diff_time
    ) %>%
    select(date, lab_wwtp_unique_id, rho) %>%
    distinct()

  # Combine stats with ww data
  ww_rho <- ww_data %>%
    left_join(ww_stats, by = c("lab_wwtp_unique_id", "date"))

  # compute z scores and flag
  ww_z_scored <- ww_rho %>%
    dplyr::left_join(
      ww_rho %>%
        dplyr::group_by(lab_wwtp_unique_id) %>%
        dplyr::summarise(
          mean_rho = mean(rho, na.rm = TRUE),
          std_rho = sd(rho, na.rm = TRUE),
          mean_conc = mean(ww, na.rm = TRUE),
          std_conc = sd(ww, na.rm = TRUE)
        ),
      by = "lab_wwtp_unique_id"
    ) %>%
    dplyr::group_by(lab_wwtp_unique_id) %>%
    mutate(
      z_score_conc = (ww - mean_conc) / std_conc,
      z_score_rho = (rho - mean_rho) / std_rho
    ) %>%
    dplyr::mutate(
      z_score_rho_t_plus_1 = lead(z_score_rho, 1),
      flagged_for_removal_conc = dplyr::case_when(
        abs(z_score_conc) >= log_conc_threshold ~ 1,
        is.na(z_score_conc) ~ 0,
        TRUE ~ 0
      ),
      flagged_for_removal_rho = dplyr::case_when(
        (
          abs(z_score_rho) >= rho_threshold &
            (abs(z_score_rho_t_plus_1) >= rho_threshold) &
            sign(z_score_rho != sign(z_score_rho_t_plus_1))
        ) ~ 1,
        is.na(z_score_rho) ~ NA,
        TRUE ~ 0
      )
    ) %>%
    dplyr::mutate(flag_as_ww_outlier = dplyr::case_when(
      flagged_for_removal_rho == 1 ~ 1,
      flagged_for_removal_conc == 1 ~ 1,
      TRUE ~ 0
    )) %>%
    dplyr::ungroup()

  return(ww_z_scored)
}

#' Manual removal of hopsital admissions
#' data points in particular states
#'
#' @param train_data original training dataset
#' with hopsitalizations in tact
#' @param states_for_hosp_removal states in the order
#' of the corresponding dates we want to remove
#' @param dates_for_hosp_removal dates lining up with the
#' states we want to remove from
#'
#' @return revised train data with hospital admissions removed
#' @export
#'
#' @examples
manual_removal_of_hosp_data <- function(train_data,
                                        states_for_hosp_removal,
                                        dates_for_hosp_removal,
                                        ...) {
  ## Iterate through each location and each date
  ## and remove if both date and state present
  for (i in seq_along(dates_for_hosp_removal)) {
    to_remove <- (
      train_data$location == states_for_hosp_removal[i] &
        train_data$date == dates_for_hosp_removal[i]
    )
    train_data$daily_hosp_admits[to_remove] <- NA
  }

  return(train_data)
}

#' Aggregate the WW data to the state-level
#'
#' @param ww_data_raw The site leve ww data after being cleaned
#' @param forecast_date date forecast is being made
#' @param calibration_time duration of calibration period for hospitalizations
#' @param hosp_reporting_delay the hospital reporting delay (how much is
#' being nowcasted)
#'
#' @return a dataframe with wastewater data for all locations,
#' the weekly avg pcr concentration, and the date
#' @export
#'
#' @examples
aggregate_ww <- function(ww_data_raw, forecast_date, calibration_time,
                         hosp_reporting_delay) {
  ww_data <- ww_data_raw %>%
    dplyr::filter(
      date <= lubridate::ymd(forecast_date),
      date >= forecast_date - days(hosp_reporting_delay) - days(calibration_time)
    )

  ww_data_outliers_flagged <- flag_ww_outliers(ww_data)

  ww_data_outliers_removed <- ww_data_outliers_flagged %>% dplyr::filter(flag_as_ww_outlier == 0)

  # Get a single weekly value per site
  ww_full_data <- ww_data_outliers_removed %>% mutate(
    week = epiweek(lubridate::ymd(date)),
    year = lubridate::year(lubridate::ymd(date)),
    midweek_date = round_date(lubridate::ymd(date), "week", week_start = 3)
  )

  # Summarize over the week
  ww_weekly <- ww_full_data %>%
    dplyr::group_by(lab_wwtp_unique_id, midweek_date) %>%
    dplyr::summarise(ww = mean(ww, na.rm = TRUE)) %>%
    dplyr::left_join(
      ww_full_data %>% select(
        lab_wwtp_unique_id,
        ww_pop,
        location, site, lab,
        midweek_date
      ) %>%
        dplyr::distinct(),
      by = c("midweek_date", "lab_wwtp_unique_id")
    ) %>%
    dplyr::ungroup()

  # Summarize over the sites within each location
  ww_agg <- ww_weekly %>%
    dplyr::group_by(location, midweek_date) %>%
    dplyr::summarise(ww = mean(ww, na.rm = TRUE)) %>%
    dplyr::left_join(
      ww_weekly %>%
        dplyr::select(
          location,
          midweek_date
        ) %>%
        dplyr::distinct(),
      by = c("midweek_date", "location")
    ) %>%
    dplyr::rename(date = midweek_date) %>%
    dplyr::ungroup()

  return(ww_agg)
}


#' Get site county mapp
#'
#' @param nwss wastewater data
#' @param county_site_map_path path to a county site map CSV file;
#' if it exists, it will be used, otherwise the file will be created and placed here
#'
#' @return a mapping from the unique county fips in the ww data to the major county
#' @export
#'
#' @examples
get_site_county_map <- function(nwss,
                                county_site_map_path) {
  if (fs::file_exists(county_site_map_path)) {
    site_county_map <- readr::read_csv(county_site_map_path)
    cli::cli_inform("Reading in existing site county map")
  } else {
    dir_path <- fs::path_dir(county_site_map_path)
    create_dir(file.path("data", "ww_data"))

    county_state_fips_path <- paste0(
      "https://raw.githubusercontent.com/",
      "kjhealy/fips-codes/master/",
      "state_and_county_fips_master.csv"
    )
    county_state_fips_map <- readr::read_csv(county_state_fips_path)
    site_county_map <- nwss %>%
      select(wwtp_name, county_names) %>%
      unique() %>%
      mutate(major_county_name_numeric = as.numeric(substr(county_names, 1, 5))) %>%
      left_join(county_state_fips_map, by = c("major_county_name_numeric" = "fips")) %>%
      mutate(full_county_name = paste0(name, ", ", state)) %>%
      select(wwtp_name, county_names, full_county_name) %>%
      rename(
        site = wwtp_name,
        county_codes = county_names
      )

    readr::write_csv(
      site_county_map, county_site_map_path
    )

    message("Writing site-county map")
  }

  return(site_county_map)
}

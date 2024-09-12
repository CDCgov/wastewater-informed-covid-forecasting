#' Combine and summarize wastewater data across all forecast dates and
#'  locations
#'
#' @description
#' This function iterates through the vector of forecast dates and locations,
#' checks for the presence of wastewater data, and if present loads it in.
#' It then computes a number of summary metrics on the wastewater data for that
#' particular location and forecast date to produce a single row of output
#' metadata for each combination. Locations without wastewater data are
#' indicated as such and have NAs for the wastewater specific entries. This
#' will be used to get an overall summary across locations and forecast dates
#' of wastewater characteristics
#'
#' @param forecast_dates The vector of character strings of all the forecast
#' dates
#' @param locations The vector of character strings of all the locations
#' @param eval_output_subdir The outer subdirectory of the nested file structure
#'
#' @return ww_metadata: a tibble with a row for each forecast date location and
#' columns that provide summaries of the wastewater data
#' @export
combine_and_summarize_ww_data <- function(forecast_dates,
                                          locations,
                                          eval_output_subdir) {
  if (length(forecast_dates) != length(locations)) {
    cli::cli_abort(
      message = c(
        "Vector of forecast dates and vector of locations must be equal in length",
        "Got {length(forecast_dates)} forecast dates and {length(locations)} locations"
      )
    )
  }

  ww_metadata <- tibble()
  flag_failed_output <- tibble()

  for (i in seq_along(forecast_dates)) {
    this_forecast_date <- forecast_dates[i]
    this_location <- locations[i]

    fp_ww <- get_filepath(
      eval_output_subdir,
      scenario = "status_quo",
      forecast_date = this_forecast_date,
      model_type = "ww",
      location = this_location,
      output_type = "input_ww_data",
      file_extension = "tsv"
    )
    fp_hosp <- get_filepath(
      eval_output_subdir,
      scenario = "status_quo",
      forecast_date = this_forecast_date,
      model_type = "ww",
      location = this_location,
      output_type = "input_hosp_data",
      file_extension = "tsv"
    )

    if (file.exists(fp_ww)) {
      this_ww_metadata <- load_data_and_summarize(
        fp_hosp, fp_ww,
        this_forecast_date,
        this_location
      )
      ww_metadata <- rbind(ww_metadata, this_ww_metadata)
    } else {
      warning(glue::glue(
        "File missing for {this_scenario}",
        "in {this_location} on {this_forecast_date}"
      ))
      # Create a tibble of the combos that are missing, to save
      this_failed_output <- tibble(
        scenario = this_scenario,
        location = this_location,
        forecast_date = this_forecast_date
      )
      flag_failed_output <- rbind(flag_failed_output, this_failed_output)
    } # end if else if filepath doesn't exist
  } # end for loop

  if (nrow(flag_failed_output) != 0) {
    # Save the missing files in a new subfolder in the eval_output_subdir
    cfaforecastrenewalww::create_dir(file.path(
      eval_output_subdir,
      "files_missing", model_type
    ))

    read::write_csv(
      flag_failed_output,
      file.path(
        eval_output_subdir, "files_missing", model_type,
        "ww_data_metadata.csv"
      )
    )
  }

  return(ww_metadata)
}

#' Load data and summarize metadata
#'
#' @description
#' This function pulls in an individual hospital admissions and wastewater
#' dataset for a particular forecast date and location. It computes summary
#' statistics on the wastewater characteristics for that state on that
#' forecast date. It will return a single row dataframe with the relevant
#' metadata.
#'
#'
#' @param fp_hosp Character string indicating the filepath for the hospital
#' admissions dataset, expected as a tsv
#' @param fp_ww Character string indicating the filepath for the hospital
#' admisisons dataset, expected as a tsv
#' @param this_forecast_date Character string indicating the forecast date in
#'  ISO8601 format (YYYY-MM-DD)
#' @param this_location Character string indicating location, formatted as
#' state abbreviation
#'
#' @return this_ww_metadata: a 1 row dataframe with metadata on the wastewater
#' summary statistics for that forecast date and location
#' @export
load_data_and_summarize <- function(fp_hosp, fp_ww,
                                    this_forecast_date,
                                    this_location) {
  # Use hospital data to get state pop
  this_hosp_data <- readr::read_tsv(fp_hosp)
  state_pop <- this_hosp_data |>
    dplyr::distinct(pop) |>
    dplyr::pull()
  if (length(state_pop) != 1) {
    cli::cli_abort(message = "multiple state pops reported")
  }

  this_ww_data <- readr::read_tsv(fp_ww)
  if (!nrow(this_ww_data) == 0) {
    n_sites <- this_ww_data |>
      dplyr::distinct(site) |>
      nrow()
    n_labs <- this_ww_data |>
      dplyr::distinct(lab) |>
      nrow()
    sum_site_pops <- this_ww_data |>
      dplyr::group_by(site) |>
      dplyr::summarise(
        mean_site_pop = mean(ww_pop, na.rm = TRUE)
      ) |>
      dplyr::ungroup() |>
      dplyr::summarise(ww_total_pop = sum(mean_site_pop,
        na.rm = TRUE
      )) |>
      dplyr::pull(ww_total_pop)
    pop_coverage <- sum_site_pops / state_pop

    avg_latency <- this_ww_data |>
      dplyr::group_by(lab_wwtp_unique_id) |>
      dplyr::summarize(max_date = max(date)) |>
      dplyr::mutate(
        latency = as.numeric(lubridate::ymd(
          !!this_forecast_date
        ) - lubridate::ymd(max_date))
      ) |>
      dplyr::summarize(
        mean_latency = mean(latency, na.rm = TRUE)
      ) |>
      dplyr::pull(mean_latency)

    avg_sampling_freq <- this_ww_data |>
      dplyr::group_by(lab_wwtp_unique_id) |>
      dplyr::arrange(date, desc = TRUE) |>
      # There are some duplicate dates within a site and lab
      dplyr::distinct(date) |>
      dplyr::mutate(
        prev_date = dplyr::lag(date, 1),
        diff_time = as.numeric(lubridate::days(
          difftime(date, prev_date)
        ), "days")
      ) |>
      dplyr::ungroup() |>
      dplyr::summarize(
        mean_collection_freq = mean(diff_time, na.rm = TRUE)
      ) |>
      dplyr::pull(mean_collection_freq)

    n_duplicate_obs <- this_ww_data |>
      dplyr::group_by(lab_wwtp_unique_id, date) |>
      dplyr::summarize(n_obs = dplyr::n()) |>
      dplyr::ungroup() |>
      dplyr::summarize(
        n_duplicates = sum(n_obs > 1)
      ) |>
      dplyr::pull(n_duplicates)

    this_ww_metadata <- tibble::tibble(
      forecast_date = this_forecast_date,
      location = this_location,
      ww_data_present = 1,
      n_sites,
      n_labs,
      pop_coverage,
      state_pop,
      avg_latency,
      avg_sampling_freq,
      n_duplicate_obs
    )
  } else { # Wastewater data has no rows -- fill in rows with NAs for ww metrics
    this_ww_metadata <- tibble::tibble(
      forecast_date = this_forecast_date,
      location = this_location,
      ww_data_present = 0,
      n_sites = NA,
      n_labs = NA,
      pop_coverage = NA,
      state_pop = state_pop,
      avg_latency = NA,
      avg_sampling_freq = NA,
      n_duplicate_obs = NA
    )
  }


  return(this_ww_metadata)
}

#' Get additional wastewater metadata
#'
#' @description
#' This function takes the summary statistics from just the input wastewater
#' data (granular_ww_metadata) and joins the downstream metadata about
#' convergenece and manual exclusions and combines them all into one table
#'
#' @param granular_ww_metadata a tibble with a row for each forecast date
#' location and columns that provide summaries of the wastewater data. This
#' is focused on input data. It is the output of
#' "combine_and_summarize_ww_data".
#' @param ww_forecast_date_locs_to_excl table of forecast date-locations
#' to manually exclude
#' @param convergence_df table containing convergence flags for every forecast
#' date location combination
#' @param table_of_loc_dates_w_ww table containing wastewater metadata
#' for every location-forecast date with wastewater
#'
#' @return a tibble with a number of additional columns indicating whether
#' or not there were any flags for manual exclusions, convergence issues,
#' or wastewater quality issues
#' @export
get_add_ww_metadata <- function(granular_ww_metadata,
                                ww_forecast_date_locs_to_excl,
                                convergence_df,
                                table_of_loc_dates_w_ww) {
  granular_ww_metadata_used <- granular_ww_metadata |>
    dplyr::mutate(forecast_date = lubridate::ymd(forecast_date)) |>
    dplyr::left_join(
      ww_forecast_date_locs_to_excl |>
        mutate(
          ww_exclude_manual = TRUE,
          forecast_date = lubridate::ymd(.data$orecast_date)
        ),
      by = c("location", "forecast_date")
    ) |>
    dplyr::mutate(
      ww_exclude_manual = dplyr::replace_na(.data$ww_exclude_manual, FALSE)
    ) |>
    dplyr::left_join(convergence_df,
      by = c("location", "forecast_date")
    ) |>
    dplyr::left_join(table_of_loc_dates_w_ww,
      by = c("location", "forecast_date")
    )

  return(granular_ww_metadata_used)
}

#' Get wastewater data summary tables
#'
#' @description
#' This function reports out summary statistics on the presence of and
#' characteristics of wastewater data across all forecast
#' dates and locations.
#'
#'
#' @param ww_metadata a tibble containing one row for every forecast date
#' location containing metrics averaged across that forecast date location
#' about the wastewater data. This is the output of "get_add_ww_metadata".
#' @param hosp_quantiles_filtered a large tibble containing all of the
#' quantiled forecasts for the wastewater and hospital admissions only models
#' after it has been filtered for convergence, wastewater data quality,
#' manual exclusions, and dates that don't have any wastewater present
#'
#' @return a list containing a summary overall table, a summary by forecast
#' date and a summary by state
#' @export
get_summary_ww_table <- function(ww_metadata, hosp_quantiles_filtered) {
  # First, get the true number of forecast-date locations with wastewater
  # in the current analysis
  n_w_ww_actual <- hosp_quantiles_filtered |>
    dplyr::filter(
      .data$model_type == "ww"
    ) |>
    dplyr::distinct(.data$forecast_date, .data$location) |>
    nrow()

  n_no_ww_actual <- nrow(ww_metadata) - n_w_ww_actual

  # Then get what would be expected

  n_combinations <- nrow(ww_metadata)

  n_combos_w_ww_data <- ww_metadata |>
    dplyr::summarise(
      n_ww_present = sum(ww_data_present, na.rm = TRUE)
    )

  n_states_w_complete_ww_data <- ww_metadata |>
    dplyr::group_by(location) |>
    dplyr::summarize(
      complete_ww = all(.data$ww_data_present == 1)
    ) |>
    ungroup() |>
    dplyr::summarize(n_complete_ww = sum(.data$complete_ww)) |>
    dplyr::pull(.data$n_complete_ww)

  n_states_w_no_ww_data <- ww_metadata |>
    dplyr::group_by(location) |>
    dplyr::summarize(
      no_ww = all(ww_data_present == 0)
    ) |>
    ungroup() |>
    dplyr::summarize(n_zero_ww = sum(no_ww)) |>
    dplyr::pull(n_zero_ww)

  n_combos_w_hosp_conv_flags <- ww_metadata |>
    dplyr::summarise(n_hosp_flags = sum(any_flags_hosp, na.rm = TRUE)) |>
    dplyr::pull(n_hosp_flags)
  n_combos_w_ww_conv_flags <- ww_metadata |>
    dplyr::summarise(n_ww_flags = sum(any_flags_ww, na.rm = TRUE)) |>
    dplyr::pull(n_ww_flags)

  n_insuff_ww <- ww_metadata |>
    dplyr::summarise(
      n_ww_insuff = sum(ww_sufficient == FALSE, na.rm = TRUE)
    ) |>
    dplyr::pull(n_ww_insuff)

  n_ww_excluded <- ww_metadata |>
    dplyr::summarise(
      n_ww_excl = sum(ww_exclude_manual, na.rm = TRUE)
    ) |>
    dplyr::pull(n_ww_excl)

  n_w_ww_expected <- ww_metadata |>
    dplyr::mutate(
      ww_expected = dplyr::case_when(
        ww_data_present == 1 & is.na(ww_exclude_manual) &
          !isTRUE(any_flags_hosp) & !isTRUE(any_flags_ww) &
          ww_sufficient == TRUE ~ TRUE,
        TRUE ~ FALSE
      )
    ) |>
    dplyr::summarise(
      n_expected_exclude = sum(ww_expected, na.rm = TRUE)
    ) |>
    dplyr::pull(n_expected_exclude)

  n_no_ww_expected <- nrow(ww_metadata) - n_w_ww_expected

  summary_table <- tibble::tibble(
    n_forecast_date_states = n_combinations,
    n_forecast_date_states_w_ww = n_combos_w_ww_data,
    prop_combos_w_ww = n_combos_w_ww_data / n_combinations,
    n_states_w_complete_ww_data,
    n_states_w_no_ww_data,
    n_combos_w_hosp_conv_flags,
    n_combos_w_ww_conv_flags,
    n_insuff_ww,
    n_ww_excluded,
    n_no_ww_expected,
    n_no_ww_actual,
    n_w_ww_expected,
    n_w_ww_actual
  )


  # Summarize across states by forecast date
  forecast_date_summary_table <-
    ww_metadata |>
    dplyr::group_by(forecast_date) |>
    dplyr::summarize(
      prop_states_w_ww = sum(ww_data_present) / dplyr::n(),
      pop_coverage_by_date = sum(pop_coverage * state_pop, na.rm = TRUE) / sum(state_pop),
      avg_avg_latency = mean(avg_latency, na.rm = TRUE),
      avg_avg_sampling_freq = mean(avg_sampling_freq, na.rm = TRUE),
      n_states_w_duplicate_obs = sum(n_duplicate_obs > 0, na.rm = TRUE)
    )

  # Summarize across forecast dates by state
  state_summary_table <-
    ww_metadata |>
    dplyr::group_by(location) |>
    dplyr::summarize(
      prop_forecast_dates_w_ww = sum(ww_data_present) / dplyr::n(),
      avg_pop_coverage_by_state = mean(pop_coverage, na.rm = TRUE),
      avg_avg_latency = mean(avg_latency, na.rm = TRUE),
      avg_avg_sampling_frequency = mean(avg_sampling_freq, na.rm = TRUE),
      n_forecast_dates_w_duplicate_obs = sum(n_duplicate_obs > 0, na.rm = TRUE)
    )


  ww_metadata_list <- list(
    summary_table = summary_table,
    state_summary_table = state_summary_table,
    forecast_date_summary_table = forecast_date_summary_table
  )


  return(ww_metadata_list)
}

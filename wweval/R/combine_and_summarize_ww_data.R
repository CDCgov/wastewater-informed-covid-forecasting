#' Combine and summarize wastewater data
#'
#' This function iterates through the vector of forecast dates and locations,
#' checks for the presence of wastewater data, and if present loads it in.
#' It then computes a number of summary metrics on the wastewater data for that
#' particular location and forecast date to produce a single row of output
#' metadata. Locations without wastewater data are indicated as such and have
#' NAs for the wastewater specific entries
#'
#' @param forecast_dates The vector of character strings of all the forecast dates
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
      message = "Vector of forecast dates and locations must be equal in length"
    )
  }

  ww_metadata <- tibble()

  for (i in seq_len(forecast_dates)) {
    this_forecast_date <- forecast_dates[i]
    this_location <- locations[i]

    fp <- get_filepath(
      eval_output_subdir,
      scenario = "status_quo",
      forecast_date = this_forecast_date,
      model_type = "ww",
      location = this_location,
      output_type = glue::glue("input_ww_data"),
      file_extension = "tsv"
    )
    fp_hosp <- get_filepath(
      eval_output_subdir,
      scenario = "status_quo",
      forecast_date = this_forecast_date,
      model_type = "ww",
      location = this_location,
      output_type = glue::glue("input_hosp_data"),
      file_extension = "tsv"
    )

    if (file.exists(fp)) {
      tryCatch(
        {
          # Use hospital data to get state pop
          this_hosp_data <- readr::read_tsv(fp_hosp)
          state_pop <- this_hosp_data |>
            dplyr::distinct(pop) |>
            dplyr::pull()

          this_ww_data <- readr::read_tsv(fp)
          if (!nrow(this_ww_data) == 0) {
            n_sites <- this_ww_data |>
              dplyr::distinct(site) |>
              nrow()
            n_labs <- this_ww_data |>
              dplyr::distinct(lab) |>
              nrow()
            if (length(state_pop) != 1) {
              cli::cli_abort(message = "multiple state pops reported")
            }
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
              dplyr::mutate(
                prev_date = dplyr::lag(date, 1),
                diff_time = as.numeric(difftime(date, prev_date))
              ) |>
              dplyr::ungroup() |>
              dplyr::summarize(
                mean_collection_freq = mean(diff_time, na.rm = TRUE)
              ) |>
              dplyr::pull(mean_collection_freq)

            this_ww_metadata <- tibble::tibble(
              forecast_date = this_forecast_date,
              location = this_location,
              ww_data_present = 1,
              n_sites,
              n_labs,
              pop_coverage,
              state_pop,
              avg_latency,
              avg_sampling_freq
            )
          } else {
            this_ww_metadata <- tibble::tibble(
              forecast_date = this_forecast_date,
              location = this_location,
              ww_data_present = 0,
              n_sites = NA,
              n_labs = NA,
              pop_coverage = NA,
              state_pop = state_pop,
              avg_latency = NA,
              avg_sampling_freq = NA
            )
          }


          ww_metadata <- rbind(ww_metadata, this_ww_metadata)
        },
        error = function(e) {}
      )
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
    }
  }

  if (nrow(flag_failed_output) != 0) {
    # Save the missing files in a new subfolder in the eval_output_subdir
    cfaforecastrenewalww::create_dir(file.path(
      eval_output_subdir,
      "files_missing", model_type
    ))

    write.csv(
      flag_failed_output,
      file.path(
        eval_output_subdir, "files_missing", model_type,
        glue::glue("ww_data_metadata.csv")
      )
    )
  }

  return(ww_metadata)
}

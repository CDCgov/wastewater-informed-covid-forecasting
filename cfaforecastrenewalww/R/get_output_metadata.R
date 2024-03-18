#' Get a table that records notes on data characteristics
#' and model choices for individual locations,
#' e.g. the absence of recent wastewater observations or
#' the decision to publish the hospital admissions only
#' model
#'
#' @param full_diagnostics_df Data frame of data diagnostics
#' by locations.
#' @param hosp_only_states States for which we chose
#' to use the hospitalization only model, as a vector
#' with states coded identically to how they are in
#' the `location` column of `full_diagnostics_df`
#' @param output_dir Path to a directory in which to save
#' the metadata table. Only used if `prod_run = TRUE`.
#' Default `NULL`.
#' @param prod_run Is this a run for publication /
#' production? If so, the output will be saved in an
#' appropriate location as a .tsv. Boolean, default `FALSE`
#'
#' @return The table of information as a [tibble::tibble()].
#' @export
get_location_notes_table <- function(full_diagnostics_df,
                                     hosp_only_states,
                                     output_dir = NULL,
                                     prod_run = FALSE) {
  metadata_df <- full_diagnostics_df %>%
    dplyr::select(location, diagnostic, value) %>%
    dplyr::filter(diagnostic %in% c(
      "no_wastewater_data_flag",
      "delayed_wastewater_data_flag",
      "insufficient_ww_data_flag"
    )) %>%
    tidyr::pivot_wider(
      id_cols = location,
      names_from = diagnostic,
      values_from = value
    ) %>%
    dplyr::mutate(additional_notes = dplyr::case_when(
      no_wastewater_data_flag == 1 ~ "No wastewater data available",
      insufficient_ww_data_flag == 1 ~ paste0(
        "Wastewater data is too sparse, ",
        "forecast not likely to be ",
        "informed by wastewater"
      ),
      delayed_wastewater_data_flag == 1 ~ paste0(
        "Wastewater data is ",
        "unavailable for recent ",
        "time-points, ",
        "forecast not likely to be ",
        "informed by wastewater"
      ),
      location %in% c(hosp_only_states) ~ paste0(
        "Hospital admissions only model was run ",
        "due to either issues with input data or model"
      ),
      TRUE ~ "None"
    )) %>%
    dplyr::select(location, additional_notes)

  if (isTRUE(prod_run)) {
    create_dir(output_dir)
    save_path <- fs::path(
      output_dir,
      glue::glue("wastewater_metadata_table.tsv")
    )
    cli::cli_inform(
      "Writing forecast additional notes to {save_path}"
    )
    readr::write_tsv(
      metadata_df,
      file = save_path
    )
  }

  (metadata_df)
}

#' Create YAML formatted metadata file
#' listing states without wastewater data,
#' states with insufficient wastewater data,
#' and states for which we used the hospital
#' admissions only model.
#'
#' @param data_diagnostics_df Data frame of data
#' diagnostics by location
#' @param hosp_only_states States for which we chose
#' to use the hospitalization only model, as a vector
#' with states coded identically to how they are in
#' the `location` column of `full_diagnostics_df`
#' @param output_dir Path to a directory in which to save
#' the metadata table. Only used if `prod_run = TRUE`.
#' Default `NULL`.
#' @param prod_run Is this a run for publication /
#' production? If so, the output will be saved in an
#' appropriate location as a .tsv. Boolean, default `FALSE`
#'
#' @return The metadata to be saved, as a `list`.
#' @export
get_metadata_yaml <- function(data_diagnostics_df,
                              hosp_only_states,
                              output_dir = NULL,
                              prod_run = FALSE) {
  summary_list <- get_summary_stats(data_diagnostics_df)

  metadata <- list(
    "States without wastewaster data" =
      ifelse(rlang::is_empty(summary_list$states_w_no_ww_data), "None",
        list(summary_list$states_w_no_ww_data)
      ),
    "States with insufficient wastewater data" =
      ifelse(rlang::is_empty(summary_list$states_to_flag_for_hub),
        "None",
        list(summary_list$states_to_flag_for_hub)
      ),
    "States we chose to use hospital admissions only model on" =
      ifelse(rlang::is_empty(hosp_only_states),
        "None",
        list(hosp_only_states)
      )
  )
  if (isTRUE(prod_run)) {
    create_dir(output_dir)
    write_yaml(metadata, file = file.path(
      output_dir,
      glue::glue("metadata.yaml")
    ))
  }

  return(metadata)
}



#' @title Assemble pipeline metadata for a given
#' run, returning it as a list and
#' writing it to disk as a YAML file.
#'
#' @param us_run Was the model run for the US as a
#' whole in addition to states and territories?
#' If so, this will be recorded. Boolean.
#' @param dir_to_save_metadata Subdirectory in which to
#' save the resultant metadata file.
#' @param run_id Unique ID of the pipeline run (as a
#' hash generated at runtime).
#' @param date_run Date the pipeline was run.
#' @param ww_data_dir Subdirectory in which wastewater
#' data used for the run is stored.
#' @param hosp_data_dir Subdirectory in which hospital
#' admissions data used for the run is stored.
#' @param forecast_date Forecast date targeted by the
#' run.
#' @param prod_run Is this a production run? Boolean.
#' @param model_type What was the model type for this run?
#' @param location What location(s) besides the US as a whole
#' were run?
#' @param output_dir What output directory was used to
#' store run output.
#' @param root_dir root directory where pipeline is being run, used to make absolute filepaths
#' @param ... Other keyword arguments, for compatibility
#' with do.call on a large parameter list.
#' @return the metadata, as a list.
#' @export
get_pipeline_metadata <- function(us_run,
                                  dir_to_save_metadata,
                                  run_id,
                                  date_run,
                                  ww_data_path, hosp_data_dir,
                                  forecast_date,
                                  prod_run,
                                  model_type,
                                  location,
                                  output_dir,
                                  root_dir,
                                  ...) {
  git_hash <- system("git log --pretty=format:'%h' -n 1", intern = TRUE)


  prod <- ifelse(isTRUE(prod_run), "prod", "test")
  if (isTRUE(us_run)) {
    locations_run <- c(location, "US")
  } else {
    locations_run <- location
  }

  relative_hosp_path <- file.path(
    hosp_data_dir,
    glue::glue(
      as.character(forecast_date),
      ".csv"
    )
  )

  if (prod_run == TRUE) {
    full_file_path <- file.path(
      dir_to_save_metadata, "prod",
      glue::glue("{forecast_date}-run-on-{date_run}")
    )
  } else {
    full_file_path <- file.path(
      dir_to_save_metadata, "test",
      glue::glue("{forecast_date}-run-on-{date_run}")
    )
  }
  create_dir(full_file_path)

  pipeline_metadata <- list(
    git_hash = git_hash,
    relative_output_path = output_dir,
    relative_hosp_path = relative_hosp_path,
    relative_ww_path = ww_data_path,
    full_output_path = fs::path(root_dir, output_dir),
    full_ww_path = fs::path(root_dir, ww_data_path),
    full_hosp_path = fs::path(root_dir, relative_hosp_path),
    date_run = date_run,
    prod_run = prod_run,
    run_id = run_id,
    model_type = model_type,
    locations_run = locations_run,
    all_locs_run = length(locations_run) == 53
  )

  write_yaml(pipeline_metadata, file = file.path(
    full_file_path,
    glue::glue("{run_id}.yaml")
  ))
  return(pipeline_metadata)
}

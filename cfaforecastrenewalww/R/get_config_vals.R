#' Read in the config yaml
#'
#' @param path_to_config path the config yaml
#'
#' @return
#' @export
#'
#' @examples
get_config_vals <- function(path_to_config) {
  config <- read_yaml(path_to_config)
  config$forecast_date <- lubridate::ymd(config$forecast_date)
  config$date_run <- lubridate::ymd(config$forecast_date)
  model_file_path <- get_model_file_path(config$model_type)
  full_config <- c(config,
    model_file_path = model_file_path
  )

  return(full_config)
}


#' Get the figure file path
#'
#' @param output_dir
#'
#' @return
#' @export
#'
#' @examples
get_figure_file_path <- function(output_dir) {
  fp <- file.path(
    output_dir, "figures"
  )
  return(fp)
}

#' get the cleaned pdf filepath
#'
#' @param output_dir
#'
#' @return
#' @export
#'
#' @examples
get_pdf_file_path <- function(output_dir) {
  fp <- file.path(
    output_dir,
    "cleaned"
  )
  return(fp)
}

#' Get the submission file path fior hub csv
#'
#' @param output_dir
#' @param forecast_date
#' @param date_run
#'
#' @return
#' @export
#'
#' @examples
get_submission_file_path <- function(output_dir,
                                     forecast_date,
                                     date_run) {
  fp <- file.path(
    output_dir,
    "cleaned",
    glue::glue("{forecast_date}-run-on-{date_run}"),
    "external"
  )
  return(fp)
}

#' Get summarized table with each location and
#'
#' @param hub_submission_df
#' @param full_diagnostics_df
#' @param hosp_only_states
#' @param repo_file_path
#'
#' @return
#' @export
#'
#' @examples
get_summarized_table <- function(hub_submission_df,
                                 full_diagnostics_df,
                                 hosp_only_states,
                                 repo_file_path) {
  diag_df_wide <- full_diagnostics_df %>%
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
    dplyr::mutate(
      additional_notes =
        ifelse(no_wastewater_data_flag == 1, "No wastewater data available",
          dplyr::case_when(
            insufficient_ww_data_flag == 1 ~ paste0(
              "Wastewater data is too sparse, ",
              "forecast not likely to be informed ",
              "by wastewater"
            ),
            (delayed_wastewater_data_flag == 1) ~ paste0(
              "Wastewater data is unavailable for recent ",
              "time-points, forecast not likely to be ",
              "informed by wastewater"
            ),
            location %in% c(hosp_only_states) ~ paste0(
              "Hospital admissions only model was run ",
              "due to either issues with input data or model"
            ),
            TRUE ~ "None"
          )
        )
    ) %>%
    dplyr::select(location, additional_notes)

  create_dir(repo_file_path)

  readr::write_tsv(
    diag_df_wide,
    file = fs::path(
      repo_file_path,
      glue::glue("wastewater_metadata_table.tsv")
    )
  )
  cli::cli_inform("Writing forecast additional notes to repo ")


  (diag_df_wide)
}

#' Get metadata to list all states with no wastewater or insufficient wastewater
#' data
#'
#' @param full_diagnostics_df
#' @param repo_file_path
#'
#' @return
#' @export
#'
#' @examples
get_metadata_yaml <- function(full_diagnostics_df,
                              repo_file_path,
                              hosp_only_states) {
  summary_list <- get_summary_stats(full_diagnostics_df)

  metadata <- list(
    "States without wastewaster data" =
      ifelse(rlang::is_empty(summary_list$states_w_no_ww_data), "None",
        list(summary_list$states_w_no_ww_data)
      ),
    "States with insufficient wastewater data" =
      ifelse(rlang::is_empty(summary_list$states_to_flag_for_hub), "None",
        list(summary_list$states_to_flag_for_hub)
      ),
    "States we chose to use hospital admissions only model on" =
      ifelse(rlang::is_empty(hosp_only_states), "None", list(hosp_only_states))
  )
  create_dir(repo_file_path)
  write_yaml(metadata, file = file.path(
    repo_file_path,
    glue::glue("metadata.yaml")
  ))
  return(metadata)
}



#' @title Write pipeline metadata to a YAML
#' @param us_run US run?
#' @param path_to_save_metadata
#' @param run_id
#' @param date_run
#' @param ww_data_dir
#' @param hosp_data_dir
#' @param forecast_date
#' @param prod_run
#' @param model_type
#' @param location
#' @param output_dir

#' @param root_dir root directory where pipeline is being run, used to make absolute filepaths
#' @param ...
#' @return the metadata
#' @export
get_pipeline_metadata <- function(us_run,
                                  path_to_save_metadata,
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
      path_to_save_metadata, "prod",
      glue::glue("{forecast_date}-run-on-{date_run}")
    )
  } else {
    full_file_path <- file.path(
      path_to_save_metadata, "test",
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

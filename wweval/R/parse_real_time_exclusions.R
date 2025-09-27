exclusion_hierarchy <- c(
  "manual_exclude_both",
  "manual_exclude_ww",
  "non_convergence_ww",
  "non_convergence_hosp",
  "absent_ww",
  "insufficient_ww"
)

#' Parse forecast exclusions from real-time run archived metadata
#'
#' @param dir Directory to parse, with subdirectories representing
#' forecast dates, each of which contains a `metadata.yaml` file.
#' @return Table of the parsed exclusions, as a
#' tidy [`tibble`][tibble::tibble()].
#' @export
parse_real_time_exclusions <- function(dir) {
  dirs <- fs::dir_ls(dir, type = "directory")
  .parse_meta_yaml <- function(dir_path) {
    forecast_date <- as.Date(fs::path_file(dir_path))
    meta_path <- fs::path(dir_path, "metadata", ext = "yaml")
    metadata <- yaml::read_yaml(meta_path)

    fields <- c(
      "absent_ww" = "States without wastewaster data",
      "insufficient_ww" = "States with insufficient wastewater data",
      "manual_exclude_ww" = "States we chose to use hospital admissions only model on",
      "manual_exclude_both" = "States we chose to not submit a forecast for"
    )
    result <- purrr::imap(fields, \(field, fieldname) {
      tibble::tibble(
        forecast_date = forecast_date,
        location = as.character(unlist(metadata[[field]])),
        exclusion = fieldname
      )
    }) |>
      dplyr::bind_rows() |>
      dplyr::filter(!.data$location == "None") |>
      order_col("exclusion", levels = exclusion_hierarchy) |>
      dplyr::arrange(exclusion) |>
      dplyr::distinct(.data$forecast_date, .data$location, .keep_all = TRUE)
    ## max one exclusion reason per location-date pair, with the reported
    ## exclusion based on the exclusion_hierarchy when there are multiple
    ## potential reasons to exclude.

    return(result)
  }

  return(purrr::map_df(dirs, .parse_meta_yaml))
}

#' Compute reasons for exclusion of forecast date / location pairs
#' from the paired retrospective analysis.
#'
#' @param real_time_exclusion_table Table of real-time exclusions, with
#' reasoning, as the output of [parse_real_time_exclusions()].
#' @param ww_data_quality_table Table of wasteater data quality status,
#' as the output of [summarize_ww_data_quality()].
#' @param convergence_table Table of convergence statuses for each model
#' by date and location.
#' @return Table of the computed exclusions, as a
#' tidy [`tibble`][tibble::tibble()].
#' @export
compute_retro_exclusions <- function(
  real_time_exclusion_table,
  ww_data_quality_table,
  convergence_table
) {
  manual_exclusions <- real_time_exclusion_table |>
    dplyr::filter(
      .data$exclusion %in% c("manual_exclude_ww", "manual_exclude_both")
    )
  data_quality_exclusions <- ww_data_quality_table |>
    dplyr::filter(!.data$ww_sufficient) |>
    dplyr::select("forecast_date", "location", exclusion = "status")

  convergence_exclusions <- convergence_table |>
    dplyr::filter(.data$any_flags_ww | .data$any_flags_hosp) |>
    dplyr::mutate(
      exclusion = dplyr::case_when(
        .data$any_flags_ww ~ "non_convergence_ww",
        .data$any_flags_hosp ~ "non_convergence_hosp",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::select("forecast_date", "location", "exclusion")

  result <- dplyr::bind_rows(
    manual_exclusions,
    data_quality_exclusions,
    convergence_exclusions
  ) |>
    order_col("exclusion", levels = exclusion_hierarchy) |>
    dplyr::arrange(exclusion) |>
    dplyr::distinct(.data$forecast_date, .data$location, .keep_all = TRUE)

  return(result)
}

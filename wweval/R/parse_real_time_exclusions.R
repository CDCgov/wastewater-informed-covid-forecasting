#' Parse forecast exclusions fromreal-time run archived metadata
#'
#' @param dir Directory to parse, with subdirectories representing forecast dates,
#' each of which contains a `metadata.yaml` file.
#' @return The parsed exclusions, as a tidy [`tibble`][tibble::tibble()].
#' @export
parse_real_time_exclusions <- function(dir) {
    exclusion_hierarchy <- c(
        "manual_exclude_both",
        "manual_exclude_ww",
        "absent_ww",
        "insufficient_ww")

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
        dplyr::arrange(dplyr::desc(exclusion_hierarchy)) |>
        dplyr::distinct(.data$forecast_date,
                        .data$location,
                        .keep_all = TRUE)
    ## max one exclusion reason per location-date pair, with the reported
    ## exclusion based on the exclusion_hierarchy when there are multiple
    ## potential reasons to exclude.

    return(result)
  }

  return(purrr::map_df(dirs, .parse_meta_yaml))
}

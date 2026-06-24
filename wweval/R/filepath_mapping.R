#' Get filepath
#' @description
#' A function to return a character string for each item to either be saved or
#' read in, based on the nesting that is specified in this function. To change
#' the nesting, we will change this function
#'
#'
#' @param output_subdir Character string to upper level directory where outputs
#' live
#' @param scenario string indicating the evaluation scenario
#' @param forecast_date string indicating the forecast date in "YYYY-MM-DD"
#' format
#' @param model_type string indicating the type of model, either "ww" or "hosp"
#'  for now but can be used for model comparison
#' @param location string indicating the location
#' @param output_type string indicating the type of output
#' @param file_extension string indicating the file extension e.g. ".tsv"
#'
#' @return string of the full filepath
#' @export
#'
get_filepath <- function(
  output_subdir,
  scenario,
  forecast_date,
  model_type,
  location,
  output_type,
  file_extension
) {
  fp <- file.path(
    output_subdir,
    scenario,
    forecast_date,
    model_type,
    location,
    glue::glue("{output_type}.{file_extension}")
  )

  return(fp)
}

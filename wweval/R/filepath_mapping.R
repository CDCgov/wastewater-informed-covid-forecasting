#' Get model path
#'
#' @param model_type string specifying the model to be run, options are either
#' 'hosp' for the hospital admissions only model or
#' 'ww' for the site-level infection dyanmics model using wastewater
#' @param stan_models_dir directory where stan files are located
#'
#' @return string indicating path to correct stan file
#' @export
#'
#' @examples model_path <- get_model_path("hosp", system.file("stan",
#'   package = "cfaforecastrenewalww"
#' ))
get_model_path <- function(model_type, stan_models_dir) {
  stopifnot("Model type is empty" = !is.null(model_type))
  model_file_name <- if (model_type == "ww") {
    "renewal_ww_hosp_site_level_inf_dynamics"
  } else if (model_type == "hosp") {
    "renewal_ww_hosp"
  } else {
    NULL
  }

  stopifnot("Model type is not specified properly" = !is.null(model_file_name))
  fp <- file.path(stan_models_dir, paste0(model_file_name, ".stan"))
  return(fp)
}

#' Get filepath
#' @description
#' A function to return a character string for each item to either be saved or
#' read in, based on the nesting that is specified in this function. To change
#' the nesting, we will change this funciton
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
get_filepath <- function(output_subdir,
                         scenario,
                         forecast_date,
                         model_type,
                         location,
                         output_type,
                         file_extension) {
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


get_fake_model_path <- function(model_type, path_to_stan_models) {
  model_file_name <- if (model_type == "modx") {
    "x"
  } else {
    "y"
  }
  fp <- file.path(path_to_stan_models, paste0(model_file_name, ".stan"))
  return(fp)
}

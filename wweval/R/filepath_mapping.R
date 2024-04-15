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

get_fake_model_path <- function(model_type, path_to_stan_models) {
  model_file_name <- if (model_type == "modx") {
    "x"
  } else {
    "y"
  }
  fp <- file.path(path_to_stan_models, paste0(model_file_name, ".stan"))
  return(fp)
}

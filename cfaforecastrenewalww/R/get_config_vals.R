#' Read in the config yaml and add run-specific
#' dynamic configuration to it.
#'
#' @param path_to_config path the config yaml
#'
#' @return The configuration values, as a list
#' @export
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

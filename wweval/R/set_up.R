#' Make dataframe
#'
#' @param config a config file with a list of locations, forecast_dates, and
#' model types
#'
#' @return an expanded dataframe that contains 3 columns with all combinations
#' of locations, forecast_dates and model types
#' @export
make_df <- function(config) {
  df <- as.data.frame(expand.grid(
    location = config$location,
    forecast_date = config$forecast_date,
    model_type = config$model_type
  ))
  return(df)
}

#' flusight_location_table dataset
#'
#' This dataset codifies US states and territories.
#'
#' @format A tibble with 58 rows and 3 columns:
#'   \describe{
#'     \item{location_code}{The name of the location}
#'     \item{short_name}{Two letter code for the location}
#'     \item{long_name}{The full name of the location}
#'   }
#'
#' @source
#' Originally downloaded from
#' <https://www2.census.gov/geo/docs/reference/state.txt>
#'
#' @examples
#' data(flusight_location_table)
#' head(flusight_location_table)
#' @keywords dataset, location
"flusight_location_table"

#' Just a list of things the package needs, for convenience
#'
#' @return the API token/secret/ID, as a character
#' @keywords internal
.get_api_needed <- function() {
  needed_values <- c(
    "covidcast_api_key",
    "NHSN_API_KEY_ID",
    "NHSN_API_KEY_SECRET",
    "nwss_data_token",
    "data_rid"
  )

  return(needed_values)
}

#' Interface for getting information needed for API interfacing
#'
#' @param secret character, name of the API token/secret/ID needed
#'
#' @return the API token/secret/ID, as a character
#' @export
get_secret <- function(secret) {
  val <- Sys.getenv(secret)
  if (identical(val, "")) {
    stop(
      paste0(
        "Requested secret \"", secret, "\" does not exist in the environment. ",
        "Have you run setup_secrets()?"
      )
    )
  }
  return(val)
}

#' Handles values needed to retrieve data from APIs to run pipeline
#'
#' @param yaml_path character, file path to a YAML containing the API keys, tokens, and IDs
#'
#' @return nothing, sets values as environmental variables
#' @export
setup_secrets <- function(yaml_path) {
  needed_values <- .get_api_needed()

  stopifnot("Cannot find file with secrets" = file.exists(yaml_path))

  secrets <- yaml::read_yaml(yaml_path,
    fileEncoding = "UTF-8",
    readLines.warn = FALSE
  )

  to_do <- intersect(names(secrets), needed_values)
  if (!all(needed_values %in% to_do)) {
    warning(
      cat(
        "Cannot find following secrets:",
        paste0(setdiff(needed_values, names(secrets)), collapse = ", "),
        "\n",
        "May not be able to retrieve all requisite data from APIs.",
        sep = ""
      )
    )
  }

  secrets <- secrets[names(secrets) %in% to_do]

  do.call(Sys.setenv, secrets)
}

#' Cleans up environment when secrets and API information is no longer needed
#'
#' @return nothing, unsets values of environmental variables set by setup_secrets
#' @export
cleanup_secrets <- function() {
  for (val in .get_api_needed()) {
    Sys.unsetenv(val)
  }
}

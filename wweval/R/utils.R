#' @title Suppress output and messages for code.
#' @description Used in the pipeline.
#' @return The result of running the code.
#' @param code Code to run quietly.
#' @examples
#' library(cmdstanr)
#' compile_model("stan/model.stan")
#' quiet(fit_model("stan/model.stan", simulate_data_discrete()))
#' out
#' @noRd
quiet <- function(code) {
  sink(nullfile())
  on.exit(sink())
  suppressMessages(code)
}

#' Check whether a required package is installed
#' @param pkg_name Character scalar. The name of the package to check.
#' @return If the package is not available, it returns an error.
#' @noRd
check_package_is_installed <- function(pkg_name) {
  if (!requireNamespace(pkg_name)) {
    stop(
      glue::glue("The R package `{pkg_name}` is not available. "),
      glue::glue("Use `install.packages(\"{pkg_name}\")`.")
    )
  }
}

#' Helper function to pass along dependencies
#'
#' @param x any dependecy
#' @param ... additional args
#'
#' @return NULL
#'
#' @noRd
with_dependencies <- function(x, ...) {
  x
}

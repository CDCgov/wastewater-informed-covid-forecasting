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

#' Helper function to note targets dependencies not explicitly
#' noted in the function call
#'
#' @param output to be passed along (typically the output of a function call
#' @param ... additional arguments, which can be used to indicate additional upstream targets
#' to treat as dependencies when this function is used in a target `command`
#'
#' @return the first argument
#'
#' @export
with_dependencies <- function(x, ...) {
  x
}

#' Save an object to .rds in a given `output_dir`,
#' with programmatic naming.
#'
#' By default, names the `.rds` file according to
#' the object name in the R environment, with an
#' optional string suffix appended.
#' So for example a variable named `my_var` with
#' suffix equal to `"my_suffix"` would be saved to disk
#' as `<output_dir>/my_var_my_suffix.rds`. Alternatively,
#' a custom `save_basename` can be supplied. The `suffix`
#' will still be added.
#'
#' @param object Object to save as an `.rds` file.
#' @param output_dir Directory in which to save the `.rds` file.
#' @param save_basename Base name for the `.rds`, before any
#' `suffix` or the file extension, as a string. If `NULL`,
#' use the name of the object in the R environment. Default `NULL`.
#' @param save_suffix Suffix to append to the save_basename. Default
#' `""` (no suffix).
#' @param ext Extension for the saved file, without the `.`.
#' Default `rds`.
#' @return Nothing, saving the object as a side effect.
#' @export
to_rds_with_suffix <- function(object,
                               output_dir,
                               save_basename = NULL,
                               save_suffix = "",
                               ext = "rds") {
  if (is.null(save_basename)) {
    save_basename <- deparse(substitute(object))
  }
  saveRDS(
    object = object,
    file = fs::path(
      output_dir,
      glue::glue("{save_basename}{save_suffix}"),
      ext = ext
    )
  )
}


#' Generate a standard format output suffix for saving raw output
#' for a location/forecast date/scenario trio.
#'
#' @param location Name of the location, as a string.
#' @param forecast_date The forecast date, as a date or
#' in a format coercible by [as.Date()].
#' @param scenario Name of the scenario, as a string.
#' @return The output suffix, as a string.
#' @export
get_raw_output_suffix <- function(location,
                                  forecast_date,
                                  scenario) {
  return(paste("",
    location,
    format(as.Date(forecast_date), "%Y.%m.%d"),
    scenario,
    sep = "_"
  ))
}

#' Assert that needed environment variables are set
#'
#' @param needed_vars Vector of needed environment
#' variables.
#'
#' @return `NULL`, invisibly on success or raise an error.
#' @examples
#'
#' tryCatch(
#'   assert_needed_env_vars(c(
#'     "WWEVAL_EXAMPLE_ONE",
#'     "WWEVAL_EXAMPLE_TWO"
#'   )),
#'   error = \(e) print(e)
#' )
#'
#' @export
assert_needed_env_vars <- function(needed_vars) {
  vars <- Sys.getenv(needed_vars)
  checkmate::assert_character(vars)
  which_missing <- vars == ""
  if (any(which_missing)) {
    cli::cli_abort(c(
      "Could not find required environment variables ",
      "{names(vars)[which_missing]}"
    ))
  }
  invisible()
}


#' Select columns from one dataframe based on the column
#' spec of a second dataframe
#'
#' @param df Dataframe from which to select columns
#' @param template_df Dataframe to use as a template
#' for the column specification
#' @return The result of calling [dplyr::select()] on
#' `df`, raising an error if not all of the columns from
#' `template_df` can be found.
#' @export
select_like <- function(df, template_df) {
  return(dplyr::select(df, tidyselect::all_of(colnames(template_df))))
}


#' Light wrapper function for converting a dataframe column
#' to an ordered factors.
#'
#' Wraps [dplyr::mutate()]. Useful for pipe chains.
#'
#' @param df Data frame to transform
#' @param col column to transform
#' @param levels levels for the column, in ascending order..
#' @return A copy of the data frame with the `col` column transformed
#' into an ordered factor with levels given by `levels`.
#' @examples
#'
#' df <- tibble::tibble(
#'   x = c("b", "c", "a", "c", "b", "a", "a"),
#'   y = rnorm(7),
#'   z = 5
#' )
#'
#' new_df <- df |>
#'   order_col("x", c("c", "b", "a")) |>
#'   dplyr::select("x", "z") |>
#'   dplyr::arrange(x)
#'
#' new_df
#' @export
order_col <- function(df, col, levels) {
  checkmate::assert_vector(levels, unique = TRUE)
  return(dplyr::mutate(
    df,
    !!col := factor(.data[[col]],
      ordered = TRUE,
      levels = levels
    )
  ))
}

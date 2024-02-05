#' Render diagnostic report
#'
#' @param output path to output .html
#' @param params list of parameters for the Rmd
#' @param report_rmd path to Rmarkdown skeleton
#' @param ... additional parameters passed to `rmarkdown::render`
#'
#' @export
render_diagnostic_report <- function(
    output,
    params,
    report_rmd = system.file(
      "diagnostic_report_non_targets.Rmd",
      package = "cfaforecastrenewalww",
      mustWork = TRUE
    ),
    ...) {
  check_package_is_installed("rmarkdown")

  rmarkdown::render(
    input = report_rmd,
    output_file = basename(output),
    output_dir = dirname(output),
    params = params,
    envir = new.env(),
    ...
  )
}

#' Get the path to the subdirectory in which to
#' save figures for a given top-level output directory
#'
#' @param output_dir Path to the output directory
#'
#' @return path to the figure subdirectory
#' @export
get_figure_output_subdirectory <- function(output_dir) {
  subdir <- file.path(
    output_dir, "figures"
  )
  return(subdir)
}

#' Get the path to the subdirectory in which
#' to save summary output for a given top-level
#' output directory.
#'
#' @param output_dir Path to the output directory
#'
#' @return The path to the subdirectory in which
#' to save the summary PDFs
#' @export
get_pdf_output_subdirectory <- function(output_dir) {
  subdir <- file.path(
    output_dir,
    "cleaned"
  )
  return(subdir)
}

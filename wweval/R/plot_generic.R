#' Generic plotting helper functions

#' Scatterplot of pointintervals, with both x and y intervals.
#'
#' @param data Dataframe to plot, as the output of [ggdist::mean_qi()]
#' or a related function.
#' @param x Name of the x axis variable.
#' @param y Name of the y axis variable.
#' @param ... keyword arguments passed to [ggdist::geom_pointinterval()].
#' @return The plot as a [ggplot2::ggplot()] object.
#' @export
plot_pointinterval_scatter <- function(data, x, y, ...) {
  p <- ggplot(
    data = data,
    mapping = aes(x = .data[[x]], y = .data[[y]])
  ) +
    ggdist::geom_pointinterval(
      aes(
        xmin = .data[[glue::glue("{x}.lower")]],
        xmax = .data[[glue::glue("{x}.upper")]]
      ),
      ...
    ) +
    ggdist::geom_pointinterval(
      aes(
        ymin = .data[[glue::glue("{y}.lower")]],
        ymax = .data[[glue::glue("{y}.upper")]]
      ),
      ...
    ) +
    get_plot_theme()
  return(p)
}

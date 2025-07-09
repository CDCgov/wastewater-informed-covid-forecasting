#' Scatterplot of inferred data trends versus forecast differences.
#'
#' @param data Dataframe to plot, as the output of [ggdist::mean_qi()]
#' or a related function.
#' @param trend_metric Name of the column containing the point estimate
#' of the trend.
#' @param diff_metric Name of the column containing the point estimate of
#' the difference.
#' @param ... keyword arguments passed to [ggdist::geom_pointinterval()].
#' @return The plot as a [ggplot2::ggplot()] object.
#' @export
plot_trend_versus_diff <- function(data, trend_metric, diff_metric, ...) {
  p <- plot_pointinterval_scatter(data, trend_metric, diff_metric, ...) +
    coord_cartesian(
      xlim = forecasttools::sym_limits(data[[trend_metric]]),
      ylim = forecasttools::sym_limits(data[[diff_metric]])
    )
  return(p)
}

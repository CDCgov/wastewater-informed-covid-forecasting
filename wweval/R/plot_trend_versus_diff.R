#' Scatterplot of inferred data trends versus forecast differences.
#'
#' @param data Dataframe to plot, as the output of [ggdist::mean_qi()]
#' or a related function.
#' @param x_metric Name of the column containing the point estimate
#' of the metric to plot on the x axis (typically a trend).
#' @param y_metric Name of the column containing the point estimate of
#' the metric to plot on the y axis (typically a measure of forecast difference.
#' @param fill_metric Name of the column to associate to the fill
#' aesthetic. If `NULL`, do not map the fill aesthetic.
#' @param x_transform Transformation for the x axis scale. Default
#' `"identity"`.
#' @param x_transform Transformation for the y axis scale. Default
#' `"identity"`.
#' @param x_center Center point for the symmetric x axis limits.
#' Default `NULL`.
#' @param ... keyword arguments passed to [ggplot2::geom_point()].
#' @return The plot as a [ggplot2::ggplot()] object.
#' @export
plot_trend_versus_diff <- function(
  data,
  x_metric,
  y_metric,
  fill_metric = NULL,
  x_transform = "identity",
  y_transform = "identity",
  x_center = NULL,
  ...
) {
  p <- ggplot2::ggplot(
    data = data,
    mapping = aes(
      x = .data[[x_metric]],
      y = .data[[y_metric]]
    )
  ) +
    geom_point(...) +
    scale_x_continuous(transform = x_transform) +
    scale_y_continuous(transform = y_transform) +
    coord_cartesian(
      xlim = forecasttools::sym_limits(
        data[[x_metric]],
        transform = x_transform,
        center = x_center
      ),
      ylim = forecasttools::sym_limits(
        data[[y_metric]],
        transform = y_transform
      )
    ) +
    get_plot_theme()

  if (!is.null(fill_metric) && !is.na(fill_metric)) {
    p <- p + aes(fill = .data[[fill_metric]]) + scale_fill_score_ratio()
  }
  return(p)
}

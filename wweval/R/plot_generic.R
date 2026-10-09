#' Side by side slabinterval plot by model type.
#'
#' Wraps [ggdist::stat_slabinterval()].
#' Defaults to a half-eyeplot.
#'
#' @param data Data frame to plot, with a `model_type` column.
#' @param y Column in `data` to plot on the y axis.
#' @param geom `geom` argument passed to [ggdist::stat_slabinterval()].
#' Default `"slabinterval"`.
#' @param shape `shape` argument passed to
#' [ggdist::stat_slabinterval()]. Default `21`.
#' @param point_size `point_size` argument passed to
#' [ggdist::stat_slabinterval()]. Default `10`.
#' @param interval_size_range `interval_size_range` argument
#' passed to [ggdist::stat_slabinterval()]. Default `c(2, 5)`.
#' @param ... Additional keyword arguments passed to
#' [ggdist::stat_slabinterval()] .
#'
#' @return The plot.
#' @export
model_type_slabinterval <- function(
  data,
  y,
  geom = "slabinterval",
  shape = 21,
  point_size = 10,
  interval_size_range = c(2, 5),
  ...
) {
  p <- ggplot2::ggplot(
    data = data,
    mapping = ggplot2::aes(
      x = .data$model_type,
      y = .data[[y]],
      fill = .data$model_type
    )
  ) +
    ggdist::stat_slabinterval(
      geom = geom,
      shape = shape,
      point_size = point_size,
      interval_size_range = interval_size_range,
      ...
    ) +
    scale_fill_model() +
    get_plot_theme() +
    ggplot2::xlab("Model")

  return(p)
}

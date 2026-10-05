#' Side by side eyeplots by model type.
#'
#' @param data Data frame to plot, with a `model_type` column.
#' @param y Column in `data` to plot on the y axis.
#'
#' @return The plot.
#' @export
model_type_eyeplot <- function(data, y) {
  p <- ggplot2::ggplot(
    data = data,
    mapping = ggplot2::aes(
      x = .data$model_type,
      y = .data[[y]],
      fill = .data$model_type
    )
  ) +
    ggdist::stat_halfeye(
      shape = 21,
      point_size = 10,
      interval_size_range = c(2, 5)
    ) +
    scale_fill_model() +
    get_plot_theme()

  return(p)
}

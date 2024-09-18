#' Get a plot of bias over time
#'
#' @param scores a complete df of scores with a column for bias
#' @param fig_subscript subscript to name the figure
#' @param fig_file_dir Path to save figures
#'
#' @return a plot of bias over time averaged across locations and forecast dates,
#' separated by model
#' @export
get_plot_bias_over_time <- function(scores,
                                    fig_subscript,
                                    fig_file_dir) {
  bias_over_time <- scores |>
    dplyr::group_by(forecast_date, model) |>
    dplyr::summarize(
      avg_bias = mean(bias)
    )
  colors <- plot_components()

  p <- ggplot(bias_over_time) +
    geom_line(aes(x = forecast_date, y = avg_bias, color = model)) +
    get_plot_theme(
      x_axis_dates = TRUE,
      y_axis_title_size = 8
    ) +
    scale_x_date(
      date_breaks = "2 weeks",
      labels = scales::date_format("%Y-%m-%d"),
    ) +
    geom_hline(aes(yintercept = 0), linetype = "dashed") +
    scale_color_manual(values = colors$model_colors) +
    xlab("") +
    ylab("Average bias") +
    ggtitle("Average bias over time, across horizons and locations")

  ggsave(p,
    width = 10, height = 5,
    filename = file.path(
      fig_file_dir,
      glue::glue("sfig_bias_over_time_{fig_subscript}.svg")
    )
  )
  ggsave(p,
    width = 10, height = 5,
    filename = file.path(
      fig_file_dir,
      glue::glue("sfig_bias_over_time_{fig_subscript}.png")
    )
  )

  return(p_)
}

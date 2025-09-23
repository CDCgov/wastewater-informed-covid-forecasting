#' Get a plot of bias over time
#'
#' @param scores a complete df of scores with a column for bias
#' @return a plot of bias over time averaged across locations and
#' forecast dates, separated by model
#' @export
plot_bias_t <- function(scores) {
  dat <- scoringutils::summarise_scores(
    scores,
    by = c("forecast_date", "model")
  )

  p <- ggplot(dat) +
    geom_line(aes(
      x = .data$forecast_date,
      y = .data$bias,
      color = .data$model
    )) +
    get_plot_theme(
      x_axis_dates = TRUE,
      y_axis_title_size = 8
    ) +
    scale_x_date(
      date_breaks = "2 weeks",
      date_labels = "%Y-%m-%d"
    ) +
    geom_hline(aes(yintercept = 0), linetype = "dashed") +
    scale_color_model() +
    xlab(NULL) +
    ylab("Average bias") +
    ggtitle("Average bias over time, across horizons and locations")

  return(p)
}

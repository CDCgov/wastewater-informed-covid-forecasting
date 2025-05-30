#' Functions for plots that assess forecast calibration

#' Get quantile-quantile plot
#'
#' @param forecasts df of granular (daily) quantile forecasts
#' @param time_period time period that scores are summarized over
#' @return a ggplot object containing a plot of the proportion of data within
#' each interval for each model.
#' @export
forecast_qq_plot <- function(forecasts, time_period) {
  colors <- plot_components()
  p <- scoringutils::get_coverage(forecasts) |>
    scoringutils::plot_quantile_coverage() +
    scale_y_continuous(
      labels = scales::label_percent()
    ) +
    ggtitle(glue::glue("QQ plot for {time_period}")) +
    get_plot_theme() +
    scale_color_manual(values = colors$model_colors) +
    coord_fixed()
  return(p)
}

#' Plot interval coverage at specified ranges
#'
#' @param forecasts df of granular (daily) quantile forecasts,
#' as the output of [scoringutils::as_forecast_quantile()].
#' @param ranges A numeric vector of credible interval ranges to plot,
#' spanning from 0 to 100.
#' @param time_period string indicating time period of fig to save
#' @param by Columns by which to stratify. Passed as the `by`
#' argument to [scoringutils::get_coverage()]. Default `c("model", "horizon")`.
#' @return A ggplot2 object
#' @export
# nolint start
forecast_interval_coverage_plot <- function(
  # nolint end
  forecasts,
  ranges,
  time_period,
  by = c("model", "horizon")
) {
  to_plot <- scoringutils::get_coverage(
    forecasts,
    by = by
  ) |>
    dplyr::mutate(
      named_facet = glue::glue("{.data$interval_range}%")
    ) |>
    order_horizons() |>
    dplyr::filter(.data$interval_range %in% !!ranges)
  colors <- plot_components()
  p <- ggplot(
    data = to_plot,
    mapping = aes(
      x = .data$horizon,
      y = .data$interval_coverage,
      color = .data$model
    )
  ) +
    geom_line(aes(group = .data$model), linetype = "dashed") +
    geom_point() +
    geom_hline(
      aes(yintercept = .data$interval_range / 100),
      linetype = "dashed"
    ) +
    facet_wrap(~ .data$named_facet, scales = "free_y") +
    labs(
      y = "Proportion of data within interval",
      x = "Forecast horizon",
      col = "Model"
    ) +
    scale_y_continuous(
      expand = expansion(c(0.2, 0.2)),
      labels = scales::label_percent()
    ) +
    scale_x_discrete() +
    get_plot_theme(
      x_axis_dates = TRUE
    ) +
    scale_color_manual(values = colors$model_colors)

  return(p)
}

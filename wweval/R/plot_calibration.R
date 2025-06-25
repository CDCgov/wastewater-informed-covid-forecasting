#' Functions for plots that assess forecast calibration

#' Get quantile-quantile plot
#'
#' Adapted from MIT-licensed [scoringutils::plot_quantile_coverage()].
#'
#' @param forecasts df of granular (daily) quantile forecasts
#' @param model_z_order z-order in which to overplot the individual models,
#' ascending (so the last named model is plotted on top).
#' If `NULL` (default), plot the models in the order they appear in
#' `forecasts`.
#' @param linewidth `width` parameter for the Q-Q lines for
#' individual models. Default 2.
#' @param reference_linecolor `color` parameter for the r
#' eference y = x line. Default `"gray"`.
#' @param reference_linewidth `width` parameter for the
#' reference y = x line. Default `2`.
#' @param reference_linetype `linetype` parameter for the
#' reference y = x line. Default `"dashed"`.
#' @param ... additional keyword arguments passed to [ggplot2::geom_line()]
#' for the Q-Q lines for individual models.
#' @return a ggplot object containing a plot of the proportion of data
#' within each interval for each model.
#' @export
forecast_qq_plot <- function(
  forecasts,
  model_z_order = NULL,
  linewidth = 2,
  reference_linecolor = "gray",
  reference_linewidth = 2,
  reference_linetype = "dashed",
  ...
) {
  colors <- plot_components()
  coverage <- scoringutils::get_coverage(forecasts)

  if (!is.null(model_z_order)) {
    coverage <- order_col(coverage, "model", model_z_order)
  }

  p <- ggplot(
    data = coverage,
    mapping = aes(x = .data$quantile_level, color = .data$model)
  ) +
    geom_polygon(
      data = data.frame(
        x = c(
          0,
          0.5,
          0.5,
          0.5,
          0.5,
          1
        ),
        y = c(
          0,
          0,
          0.5,
          0.5,
          1,
          1
        ),
        g = c("o", "o", "o"),
        stringsAsFactors = TRUE
      ),
      aes(
        x = .data$x,
        y = .data$y,
        group = .data$g,
        fill = .data$g
      ),
      alpha = 0.15,
      colour = "olivedrab3",
      fill = "olivedrab3"
    ) +
    geom_abline(
      color = reference_linecolor,
      linetype = reference_linetype,
      intercept = 0,
      slope = 1,
      linewidth = reference_linewidth
    ) +
    geom_line(aes(y = .data$quantile_coverage), linewidth = linewidth, ...) +
    xlab("Quantile level") +
    ylab("Obs < level") +
    scale_y_continuous(
      labels = scales::label_percent()
    ) +
    scale_x_continuous(
      labels = scales::label_percent()
    ) +
    get_plot_theme() +
    scale_color_manual(values = colors$model_colors) +
    coord_fixed(expand = FALSE)
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

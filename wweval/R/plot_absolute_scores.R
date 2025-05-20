#' Plotting functions for absolute forecast scores

#' Plot average score over time for model comparison
#'
#' @param scores A tibble of scores by location, forecast date, date and model,
#' containing the outputs of `scoringutils::score()` on samples plus metadata
#' transformed into a tibble.
#' @param metric Metric to plot.
#' @param model_z_order z-order in which to overplot the individual models,
#' ascending (so the last named model is plotted on top). If `NULL` (default),
#' plot the the models in order of overall score, so that the lowest (best) scoring
#' models are on top.
#' @param horizon_time_in_weeks horizon time in weeks to summarize over, default
#' is `NULL` which means that the scores are summarized over the nowcast period
#' and the 4 week forecast period
#'
#' @return a ggplot object plotting the magnitude of the avg score across
#' locations at each forecast date
#' @export
plot_score_t <- function(scores,
                         metric,
                         model_z_order = NULL,
                         horizon_time_in_weeks = NULL) {
  if (is.null(model_order)) {
    model_z_order <- scores |>
      scoringutils::summarise_scores(by = "model") |>
      dplyr::arrange(desc(.data[[metric]])) |> # want lowest overall score => plotted on top
      dplyr::pull("model")
  }

  if (!is.null(horizon_time_in_weeks)) {
    by_date <- scores |>
      scoringutils::summarise_scores(
        by = c(
          "forecast_date",
          "horizon",
          "model"
        )
      ) |>
      dplyr::filter(horizon_weeks == !!horizon_time_in_weeks)
  } else {
    by_date <- scores |>
      scoringutils::summarise_scores(by = c(
        "forecast_date",
        "model"
      ))
  }

  by_date <- order_col(by_date, "model", model_z_order)

  colors <- plot_components()
  p <- ggplot(
    by_date,
    aes(
      x = .data$forecast_date,
      y = .data[[metric]],
      color = .data$model
    )
  ) +
    forecasttools::geom_line_point(
      linewidth = 2,
      size = 3
    ) +
    labs(
      ylab = glue::glue("Average {toupper(metric)} across locations"),
      col = "Model",
      xlab = ""
    ) +
    get_plot_theme(
      x_axis_dates = TRUE,
      y_axis_title_size = 8
    ) +
    theme(axis.title.x = element_blank()) +
    scale_x_date(
      date_breaks = "1 week",
      date_labels = "%Y-%m-%d"
    ) +
    ylab(toupper(metric)) +
    scale_color_manual(values = colors$model_colors)

  return(p)
}

#' Make plot of WIS scores in Hub models overall
#'
#' @param scores quantile based scores from the hub
#'
#' @return A plot ordered by WIS over the time period
#' @export
wis_barplot <- function(scores) {
  scores <- scores |>
    dplyr::arrange(.data$wis) |>
    dplyr::mutate(model = factor(.data$model,
      levels = unique(.data$model),
      ordered = TRUE
    ))

  colors <- plot_components()
  p <- ggplot(scores) +
    geom_bar(
      aes(
        x = .data$model,
        y = .data$wis,
        fill = .data$model
      ),
      stat = "identity",
      position = "dodge",
      show.legend = FALSE
    ) +
    get_plot_theme(
      x_axis_dates = TRUE,
      y_axis_title_size = 8
    ) +
    theme(legend.position = "none") +
    scale_fill_manual(values = colors$model_colors) +
    xlab("") +
    ylab("WIS")

  return(p)
}

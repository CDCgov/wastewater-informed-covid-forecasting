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
plot_score_t <- function(
  scores,
  metric,
  model_z_order = NULL,
  horizon_time_in_weeks = NULL
) {
  if (is.null(model_z_order)) {
    model_z_order <- scores |>
      scoringutils::summarise_scores(by = "model") |>
      dplyr::arrange(desc(.data[[metric]])) |> # want lowest (best) overall score plotted on top
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
      scoringutils::summarise_scores(
        by = c(
          "forecast_date",
          "model"
        )
      )
  }

  by_date <- order_col(by_date, "model", model_z_order)

  p <- ggplot(
    by_date,
    aes(
      x = .data$forecast_date,
      y = .data[[metric]],
      color = .data$model,
      shape = .data$model,
      fill = .data$model,
    )
  ) +
    forecasttools::geom_line_point(
      linewidth = 1.25,
      size = 3,
      alpha = 0.75
    ) +
    labs(
      col = "Model",
      xlab = "",
      ylab = toupper(metric)
    ) +
    get_plot_theme(
      rotate_x_ticks = TRUE
    ) +
    theme(axis.title.x = element_blank()) +
    scale_x_date(
      date_breaks = "1 week",
      date_labels = "%Y-%m-%d",
      expand = 0
    ) +
    coord_cartesian(expand = TRUE) +
    scale_color_model() +
    scale_fill_model() +
    scale_shape_model()

  return(p)
}

#' Barplot of WIS or CRPS decomposed into
#' overprediction, underprediction, and dispersion
#'
#' @param scores Output table produced by [scoringutils::score()] or
#' [scoringutils::summarise_scores()].
#' @param x Column containing x values. Default "model".
#' @param width width for the bars. Passed to [geom_decomposed_scores()].
#' Default 0.8.
#' @param position Position for the bars. Passed to
#' [geom_decomposed_scores()]. Default `"dodge2"`.
#' @param ... keyword arguments passed to [geom_decomposed_scores()].
#' @return A plot of decomposed probabilistic scores.
#' @export
plot_score_decomposed_bars <- function(
  scores,
  x = "model",
  width = 0.8,
  position = "dodge2",
  ...
) {
  components <- c("overprediction", "dispersion", "underprediction")
  ## want order along the x or y axis to be under < disp < over,
  ## which requires the factor levels be in the above order.

  checkmate::assert_names(names(scores), must.include = components)

  p <- ggplot(
    data = scores,
    mapping = aes(x = .data[[x]], fill = .data$model)
  ) +
    geom_decomposed_scores(position = position, width = width, ...) +
    get_plot_theme(
      rotate_x_ticks = TRUE
    ) +
    theme(legend.position = "none") +
    scale_fill_model() +
    xlab("") +
    ylab("Score")

  return(p)
}


#' Generate bar plots of probabilitistic score for each model in
#' different locations.
#'
#' Light wrapper of [plot_score_decomposed_bars()].
#'
#' @param scores tibble of crps scores by location, forecast date, model,
#' horizon day
#' @param locs_to_plot Vector of strings indicating the locations to plot,
#' as two-letter USPS abbreviations.
#' @return Figure showing CRPS for multiple locations.
#' @export
plot_score_model_loc <- function(scores, locs_to_plot) {
  p <- scores |>
    dplyr::filter(.data$location %in% !!locs_to_plot) |>
    scoringutils::summarise_scores(by = c("model", "location")) |>
    plot_score_decomposed_bars() +
    facet_wrap(~ .data$location) +
    xlab("") +
    ylab("Mean Score")
  return(p)
}


#' Plot a scatterplot of scores comparing those of one model
#' to those of another, for a given stratification.
#'
#' @param scores Table of unsummarized scores, as the output of
#' [scoringutils::score().
#' @param metric Metric to plot.
#' @param model_x Model whose score should be plotted on the x axis.
#' @param model_y Model whose score should be plotted on the y axis.
#' @param by Summize scores by these columns. Passed as the
#' `by` argument to [forecasttools::summarise_scores_with_baseline()].
#' Default `NULL`.
#' @param label Column to use for labeling points. If `NA` or `NULL`,
#' do not label.
#' @param nudge_x Passed to [ggplot2::geom_text()]. Default `0`.
#' @param nudge_y Passed to [ggplot2::geom_text()]. Default `0`.
#' @param label_color Passed as the `"color"` argument to
#' [ggplot2::geom_text()]. Default `"black"`.
#' @param label_size Passed as the `"size"` argument to
#' [ggplot2::geom_text()]. Default `2`.
#' @param ... Keyword arguments
#' passed to [ggplot2::geom_point()].
#' @return The scatterplot, as ggplot object
#' @export
plot_score_scatter <- function(
  scores,
  metric,
  model_x,
  model_y,
  by = NULL,
  label = NULL,
  nudge_x = 0,
  nudge_y = 0,
  label_size = 2,
  label_color = "black",
  ...
) {
  to_plot <- scores |>
    forecasttools::filter_to_shared_forecasts(
      comparator_values = c(model_x, model_y),
      compare = "model"
    ) |>
    forecasttools::summarise_scores_with_baseline(
      baseline = model_x,
      by = by
    ) |>
    dplyr::filter(.data$model == !!model_y) |>
    dplyr::mutate(
      score_y = .data[[metric]],
      score_x = .data[[metric]] / .data$mean_scores_ratio
    )

  all_vals <- c(to_plot$score_x, to_plot$score_y)
  minval <- min(all_vals)
  maxval <- max(all_vals)

  p <- ggplot(
    data = to_plot,
    mapping = aes(x = .data$score_x, y = .data$score_y)
  ) +
    geom_abline(
      slope = 1,
      intercept = 0,
      linetype = "dashed",
      linewidth = 2
    ) +
    geom_point(...) +
    get_plot_theme() +
    coord_fixed(xlim = c(minval, maxval), ylim = c(minval, maxval)) +
    labs(
      x = glue::glue("{metric} ({model_x})"),
      y = glue::glue("{metric} ({model_y})")
    )

  if (!is.null(label) && !is.na(label)) {
    p <- p +
      geom_text(
        mapping = aes(label = .data[[label]]),
        nudge_x = nudge_x,
        nudge_y = nudge_y,
        size = label_size,
        color = label_color
      )
  }

  return(p)
}

#' Plot a heatmap of avg forecast performance
#' by location and forecast date
#'
#' @param scores A tibble of daily scores by forecast date,
#' location, and model
#' @param metric Metric to plot. One of `"wis"` or `"crps"`
#' @param models_to_plot Character vector of models to plot
#' @return a ggplot object
#' @export
heatmap_scores_by_loc_date <- function(scores, metric, models_to_plot) {
  checkmate::assert_scalar(metric)
  checkmate::assert_names(metric, subset.of = c("wis", "crps"))
  scores_summary <- scores |>
    dplyr::filter(
      model %in% !!models_to_plot
    ) |>
    scoringutils::summarise_scores(
      by = c(
        "forecast_date",
        "location",
        "model"
      )
    )

  p <- ggplot(scores_summary) +
    geom_tile(aes(
      x = .data$forecast_date,
      y = .data$location,
      fill = .data[[metric]]
    )) +
    scale_fill_gradient(
      low = "white",
      high = "darkred",
      guide = "colourbar",
      aesthetics = "fill"
    ) +
    geom_text(
      aes(
        x = .data$forecast_date,
        y = .data$location,
        label = round(.data[[metric]], 2)
      ),
      size = 1.5
    ) +
    facet_wrap(~model) +
    get_plot_theme(
      rotate_x_ticks = TRUE
    ) +
    scale_x_date(
      date_breaks = "1 week",
      labels = scales::date_format("%Y-%m-%d")
    ) +
    coord_cartesian(expand = FALSE) +
    xlab("") +
    ylab("Location") +
    labs(fill = glue::glue("Avg {toupper(metric)}")) +
    ggtitle(glue::glue(
      "Average {toupper(metric)} by forecast date and location"
    ))

  return(p)
}


#' Plot scores by horizon over time
#'
#' @param scores tibble of scores for every location, forecast date, and horizon
#' @param score_type Score to display, as its column name in `scores`.
#' @param score_display_name Display name for the score. If not specified,
#' use the value of `score_type` in uppercase.
#' @return plot of scores over time faceted by horizon
#' @export
plot_score_by_horizon_t <- function(
  scores,
  score_type,
  score_display_name = toupper(score_type)
) {
  scores_by_horizon_and_t <- scoringutils::summarise_scores(
    scores,
    by = c("model", "forecast_date", "horizon")
  ) |>
    dplyr::mutate(
      horizon = factor(
        .data$horizon,
        ordered = TRUE,
        levels = unique(c(
          "nowcast", # put nowcast first
          sort(.data$horizon)
        ))
      )
    )
  p <- ggplot(
    scores_by_horizon_and_t,
    mapping = aes(
      x = .data$forecast_date,
      y = .data[[score_type]],
      color = .data$model
    )
  ) +
    forecasttools::geom_line_point(size = 2, linewidth = 1.5, alpha = 0.5) +
    get_plot_theme(
      rotate_x_ticks = TRUE
    ) +
    facet_wrap(~ .data$horizon, ncol = 1) +
    scale_x_date(
      date_breaks = "2 weeks",
      date_labels = "%Y-%m-%d"
    ) +
    scale_color_model() +
    xlab(NULL) +
    ylab(glue::glue("Average {score_display_name}")) +
    ggtitle(glue::glue(
      "Average {score_display_name} over time, across locations"
    )) +
    theme(legend.position = "bottom") +
    guides(color = guide_legend(nrow = 2))

  return(p)
}

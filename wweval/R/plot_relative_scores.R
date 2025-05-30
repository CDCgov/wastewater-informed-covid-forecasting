#' Internal function for display names of relative metrics
#' @param metric_to_compare Name of the absolute metric,
#' as it would be passed as a `metric_to_compare` argument to
#' [scoringutils::get_pairwise_comparisons()].
#' @return The relative metric display name, as a character string.
.relative_metric_display_name <- function(metric_to_compare) {
  return(glue::glue("Relative {toupper(metric_to_compare)}"))
}

#' internal function for two-model relative score computations
#'
#' @param score table of scores, as the output of
#' [scoringutils::score()]
#' @param target_model Target model (numerator for the relative scores)
#' @param baseline_model Baseline model
#' (denominator for the relative scores)
#' @param by columns to summarize by. Passed as the `by` argument
#' to [forecasttools::summarise_scores_with_baseline()].
#' @param metric_to_compare Metric for which to compute
#' relative scores. One of `"wis"` or `"crps"`. Not case-sensitive.
#' Passed as the `metric_to_compare` argument
#' to [scoringutils::get_pairwise_comparisons()] via
#' [forecasttools::summarise_scores_with_baseline()].
#' @return The relative scores for the target model, as a table.
#' @keywords internal
.target_model_relative_scores <- function(
  scores,
  target_model,
  baseline_model,
  metric_to_compare,
  by = NULL
) {
  checkmate::assert_scalar(metric_to_compare)
  metric_to_compare <- tolower(metric_to_compare)
  checkmate::assert_names(metric_to_compare, subset.of = c("wis", "crps"))
  return(
    dplyr::filter(
      scores,
      .data$model %in% c(!!target_model, !!baseline_model)
    ) |>
      forecasttools::summarise_scores_with_baseline(
        compare = "model",
        baseline = baseline_model,
        by = by,
        metric_to_compare = metric_to_compare
      ) |>
      dplyr::filter(.data$model == !!target_model)
  )
}

#' Make a dotsinterval plot of relative scores
#' stratified by specified variables.
#'
#' @param scores Output of [scoringutils::score()].
#' @param target_model Model for which to plot relative scores
#' @param baseline_model Baseline model for the relative scores
#' computation.
#' @param metric_to_compare Metric for which to plot relative
#' scores. One of `"wis"` or `"crps"`. Not case-sensitive.
#' @param x Variable for the x axis. If `NULL`, plot a single
#' distribution.
#' @param by additional variables defining a single score to plot.
#' Passed along with the variable in `x` as the `by` argument to
#' [forecasttools::summarise_scores_with_baseline()]. Default `NULL`
#' (no additional variables).
#' @param order_x Order the groups on the x-axis by their mean
#' value of the relative_metric (i.e. the point in the pointinterval)?
#' Default `FALSE`.
#' @return a ggplot object with the distributions plotted by
#' forecast date.
#' @export
plot_rel_score_dists <- function(
  scores,
  target_model,
  baseline_model,
  metric_to_compare,
  x = NULL,
  by = NULL,
  order_x = FALSE
) {
  relative_scores <- .target_model_relative_scores(
    scores = scores,
    target_model = target_model,
    baseline_model = baseline_model,
    metric_to_compare = metric_to_compare,
    by = c(x, by)
  )

  if (!is.null(x) && order_x) {
    x_levels <- relative_scores |>
      dplyr::summarise(
        mean_metric = mean(
          # mean_qi operates on the
          # transformed scale
          log10(.data$mean_scores_ratio),
          na.rm = TRUE
        ),
        .by = x
      ) |>
      dplyr::arrange(.data$mean_metric) |>
      dplyr::pull(!!x)
    relative_scores <- relative_scores |>
      dplyr::mutate(
        !!x := factor(.data[[x]], ordered = TRUE, levels = x_levels)
      )
  }

  colors <- plot_components()
  horizon_color <- colors$horizon_colors[["overall"]]

  p <- ggplot(
    data = relative_scores,
    mapping = aes(
      x = if (!is.null(x)) .data[[x]] else NULL,
      y = .data$mean_scores_ratio
    )
  ) +
    tidybayes::stat_dotsinterval(
      point_interval = "mean_qi",
      point_color = "black",
      interval_color = "black",
      position = position_dodge(width = 0.75),
      show.legend = FALSE,
      fill = horizon_color,
      color = horizon_color
    ) +
    geom_hline(yintercept = 1, linetype = "dashed") +
    xlab("") +
    ylab(.relative_metric_display_name(metric_to_compare)) +
    scale_y_continuous(transform = "log10") +
    coord_cartesian(
      ylim = forecasttools::sym_limits(
        relative_scores$mean_scores_ratio,
        transform = "log10"
      )
    ) +
    get_plot_theme(
      x_axis_dates = TRUE,
      y_axis_title_size = 8
    )
  return(p)
}


#' Plot a heatmap of the relative crps by locations and forecast date
#' for the head-to-head comparison
#'
#' @param scores A tibble of daily scores by forecast date, location,
#' and model
#' @param target_model Model for which to plot relative CRPS
#' @param baseline_model Baseline model for the relative CRPS
#' @param metric_to_compare Metric for which to compute the
#' relative score. One of `"wis"` or `"crps"`. Passed as the
#' `metric_to_compare` argument to
#' [scoringutils::get_pairwise_comparisons()] via
#' [forecasttools::summarise_scores_with_baseline()].
#' @return a ggplot object
#' @export
plot_rel_score_heatmap <- function(
  scores,
  target_model,
  baseline_model,
  metric_to_compare
) {
  relative_scores <- .target_model_relative_scores(
    scores = scores,
    target_model = target_model,
    baseline_model = baseline_model,
    by = c("forecast_date", "location"),
    metric_to_compare = metric_to_compare
  )
  rel_metric_name <- .relative_metric_display_name(metric_to_compare)

  p <- ggplot(relative_scores) +
    geom_tile(aes(
      x = .data$forecast_date,
      y = .data$location,
      fill = .data$mean_scores_ratio
    )) +
    scale_fill_gradient2(
      high = "red",
      mid = "white",
      low = "blue",
      transform = "log2",
      midpoint = 1,
      guide = "colourbar",
      aesthetics = "fill",
      labels = scales::number_format(accuracy = 0.01)
    ) +
    geom_text(
      aes(
        x = .data$forecast_date,
        y = .data$location,
        label = round(.data$mean_scores_ratio, 2)
      ),
      size = 1.5
    ) +
    get_plot_theme(
      x_axis_dates = TRUE,
      y_axis_text_size = 4
    ) +
    theme(legend.text = element_text(size = 6)) +
    scale_x_date(
      date_breaks = "1 week",
      labels = scales::date_format("%Y-%m-%d")
    ) +
    xlab("") +
    ylab("Location") +
    labs(
      fill = glue::glue(
        "{rel_metric_name} by ",
        "forecast date and location"
      )
    )

  return(p)
}

#' Get a dotsinterval plot of a relative score distribution
#' for individual location/forecast-date forecast problems.
#'
#' @param scores table of scores by horizon day,
#' forecast date, and location
#' @param target_model Model for which to plot relative CRPS
#' @param baseline_model Baseline model for the relative CRPS
#' @param metric_to_compare Metric for which to compute the
#' relative score. One of `"wis"` or `"crps"`. Passed as the
#' `metric_to_compare` argument to
#' [scoringutils::get_pairwise_comparisons()] via
#' [forecasttools::summarise_scores_with_baseline()].
#'
#' @return ggplot object of distribution of relative CRPS scores
#' @export
plot_rel_score_distribution <- function(
  scores,
  target_model,
  baseline_model,
  metric_to_compare
) {
  relative_scores <- .target_model_relative_scores(
    scores = scores,
    target_model = target_model,
    baseline_model = baseline_model,
    metric_to_compare = metric_to_compare,
    by = c("location", "forecast_date")
  )

  p <- ggplot(data = relative_scores) +
    tidybayes::stat_dotsinterval(
      aes(y = .data$mean_scores_ratio),
      alpha = 0.5,
      position = position_dodge(width = 0.75),
      show.legend = FALSE,
      fill = "darkblue"
    ) +
    geom_hline(aes(yintercept = 1), linetype = "dashed") +
    get_plot_theme() +
    ylab(.relative_metric_display_name(metric_to_compare)) +
    xlab("Count") +
    scale_y_continuous(transform = "log10") +
    coord_cartesian(
      ylim = forecastools::sym_limits(
        relative_scores$mean_scores_ratio,
        transform = "log10"
      )
    )

  return(p)
}


# nolint start
#' Make figure that stratifies across location and forecast dates
#'
#' @param scores output of [scoringutils::score()].
#' @param target_model Model for which to plot relative scores
#' @param baseline_model Baseline model for the relative scores
#' @param metric_to_compare Metric for which to compute the
#' relative score. One of `"wis"` or `"crps"`. Passed as the
#' `metric_to_compare` argument to
#' [scoringutils::get_pairwise_comparisons()] via
#' [forecasttools::summarise_scores_with_baseline()].
#' @param horizons_to_show A vector of strings indicating the names of the
#' `horizon` that we want to show on the plot, must be a subset of
#' `nowcast`, `1 wk`, `2 wks`,`3 wks`, `4 wks` and `overall`
#' @return A ggplot object containing plots of the distribution
#' of relative scores across location and forecast dates
#' @export
plot_rel_score_dists_by_horizon <- function(
  # nolint end
  scores,
  target_model,
  baseline_model,
  metric_to_compare,
  horizons_to_show = c(
    "nowcast",
    "1 wk",
    "4 wks",
    "overall"
  )
) {
  scores_overall <- scores |>
    dplyr::mutate(
      horizon = "overall"
    )

  scores <- dplyr::bind_rows(scores, scores_overall) |>
    dplyr::filter(
      horizon %in% !!horizons_to_show
    )
  relative_scores <- .target_model_relative_scores(
    scores = scores,
    target_model = target_model,
    baseline_model = baseline_model,
    metric_to_compare = metric_to_compare,
    by = c("forecast_date", "location", "horizon")
  )

  colors <- plot_components()

  p <- ggplot(
    data = relative_scores,
    mapping = aes(
      x = .data$horizon,
      y = .data$mean_scores_ratio,
      fill = .data$horizon,
      color = .data$horizon
    )
  ) +
    tidybayes::stat_dotsinterval(
      point_interval = "mean_qi",
      alpha = 0.5,
      position = position_dodge(width = 0.75),
      show.legend = FALSE
    ) +
    geom_hline(yintercept = 1, linetype = "dashed") +
    xlab("Horizon") +
    ylab(.relative_metric_display_name(metric_to_compare)) +
    scale_y_continuous(transform = "log10") +
    coord_cartesian(
      ylim = forecasttools::sym_limits(
        relative_scores$mean_scores_ratio,
        transform = "log10"
      )
    ) +
    get_plot_theme(
      y_axis_title_size = 8,
      x_axis_title_size = 8
    ) +
    scale_fill_manual(values = colors$horizon_colors) +
    scale_color_manual(values = colors$horizon_colors)

  return(p)
}

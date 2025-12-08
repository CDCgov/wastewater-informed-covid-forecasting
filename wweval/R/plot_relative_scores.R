#' Internal function for display names of relative metrics
#' @param metric_to_compare Name of the absolute metric,
#' as it would be passed as a `metric_to_compare` argument to
#' [scoringutils::get_pairwise_comparisons()].
#' @return The relative metric display name, as a character string.
.relative_metric_display_name <- function(metric_to_compare) {
  return(glue::glue("Relative {toupper(metric_to_compare)}"))
}

#' Internal function for abbreviated display names of relative metrics
#' @param metric_to_compare Name of the absolute metric,
#' as it would be passed as a `metric_to_compare` argument to
#' [scoringutils::get_pairwise_comparisons()].
#' @return The relative metric display name, as a character string.
.relative_metric_display_abbr <- function(metric_to_compare) {
  return(glue::glue("r{toupper(metric_to_compare)}"))
}


#' internal function for two-model relative score computations
#'
#' @param scores table of scores, as the output of
#' [scoringutils::score()]
#' @param target_models Target model(s) (numerator for the relative scores)
#' @param baseline_model Baseline model (denominator for the relative scores)
#' @param by columns to summarize by. Passed as the `by` argument
#' to [forecasttools::summarise_scores_with_baseline()].
#' @param metric_to_compare Metric for which to compute
#' relative scores. One of `"wis"` or `"crps"`. Not case-sensitive.
#' Passed as the `metric_to_compare` argument
#' to [scoringutils::get_pairwise_comparisons()] via
#' [forecasttools::summarise_scores_with_baseline()].
#' @param retain_baseline Retain the baseline model (with its
#' relative scores of 1) alongside the target models(s)? Default
#' `FALSE`.
#' @return The relative scores for the target model, as a table.
#' @keywords internal
.target_model_relative_scores <- function(
  scores,
  target_models,
  baseline_model,
  metric_to_compare,
  by = NULL,
  retain_baseline = FALSE
) {
  checkmate::assert_scalar(metric_to_compare)
  metric_to_compare <- tolower(metric_to_compare)
  checkmate::assert_names(metric_to_compare, subset.of = c("wis", "crps"))
  df <- dplyr::filter(
    scores,
    .data$model %in% c(!!target_models, !!baseline_model)
  ) |>
    forecasttools::summarise_scores_with_baseline(
      compare = "model",
      baseline = baseline_model,
      by = by,
      metric_to_compare = metric_to_compare
    )

  if (!retain_baseline) {
    df <- dplyr::filter(df, .data$model != !!baseline_model)
  }

  return(df)
}

#' Plot a timeseries of relative scores versus time
#'
#' @param scores Output of [scoringutils::score()], not yet summarized.
#' @param target_models Model for which to plot relative scores
#' @param baseline_model Baseline model for the relative scores
#' computation.
#' @param metric_to_compare Metric for which to plot relative
#' scores. One of `"wis"` or `"crps"`. Not case-sensitive.
#' @param x Name of the column defining the x axis (typically time)
#' Default `"forecast_date"`.
#' @param by additional variables defining a single score to plot.
#' Passed along with the variable defined by `x` as the `by` argument to
#' [forecasttools::summarise_scores_with_baseline()]. Default `NULL`
#' (do not summarize over additional variables).
#' @param retain_baseline Retain the baseline model (with its
#' relative scores of 1) alongside the target models(s)? Default
#' `FALSE`.
#' @param model_z_order z-order in which to overplot the individual models,
#' ascending (so the last named model is plotted on top). If `NULL` (default),
#' plot the the models in order of overall score, so that the lowest (best) scoring
#' models are on top.
#' @return a ggplot object with the distributions plotted by
#' forecast date.
#' @export
plot_rel_score_t <- function(
  scores,
  target_models,
  baseline_model,
  metric_to_compare,
  x = "forecast_date",
  by = NULL,
  retain_baseline = FALSE,
  model_z_order = NULL
) {
  if (is.null(model_z_order)) {
    model_z_order <- scores |>
      scoringutils::summarise_scores(by = "model") |>
      dplyr::arrange(desc(.data[[metric_to_compare]])) |>
      # want lowest (best) overall score plotted on top
      dplyr::pull("model")
  }

  rel_scores <- .target_model_relative_scores(
    scores = scores,
    target_models = target_models,
    baseline_model = baseline_model,
    metric_to_compare = metric_to_compare,
    by = c(x, by),
    retain_baseline = retain_baseline
  ) |>
    order_col("model", model_z_order)
  p <- ggplot(
    rel_scores,
    aes(
      x = .data$forecast_date,
      y = .data$mean_scores_ratio,
      color = .data$model
    )
  ) +
    geom_hline(yintercept = 1, linetype = "dashed", linewidth = 1.5) +
    forecasttools::geom_line_point(
      linewidth = 2,
      size = 3
    ) +
    labs(
      y = .relative_metric_display_name(metric_to_compare),
      x = x
    ) +
    get_plot_theme(
      rotate_x_ticks = TRUE
    ) +
    theme(axis.title.x = element_blank()) +
    scale_x_weekly_iso_date(expand = 0) +
    scale_y_continuous(transform = "log10") +
    coord_cartesian(
      ylim = forecasttools::sym_limits(
        rel_scores$mean_scores_ratio,
        transform = "log10"
      )
    ) +
    scale_color_model()

  return(p)
}

#' Make a dotsinterval plot of relative scores
#' stratified by specified variables.
#'
#' @param scores Output of [scoringutils::score()].
#' @param target_models Model for which to plot relative scores
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
  target_models,
  baseline_model,
  metric_to_compare,
  x = NULL,
  by = NULL,
  order_x = FALSE
) {
  relative_scores <- .target_model_relative_scores(
    scores = scores,
    target_models = target_models,
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
        !!x := factor(
          .data[[x]],
          ordered = TRUE,
          levels = x_levels
        )
      )
  }

  if (is.null(x)) {
    x_scale <- ggplot2::scale_x_continuous(breaks = NULL)
  } else {
    x_scale <- ggplot2::scale_x_discrete()
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
    ylab(.relative_metric_display_abbr(metric_to_compare)) +
    scale_y_continuous(transform = "log10") +
    x_scale +
    coord_cartesian(
      ylim = forecasttools::sym_limits(
        relative_scores$mean_scores_ratio,
        transform = "log10"
      )
    ) +
    get_plot_theme(
      rotate_x_ticks = TRUE
    )

  return(p)
}


#' Plot a heatmap of the relative crps by locations and forecast date
#' for the head-to-head comparison
#'
#' @param scores A tibble of daily scores by forecast date, location,
#' and model
#' @param target_models Model for which to plot relative CRPS
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
  target_models,
  baseline_model,
  metric_to_compare
) {
  relative_scores <- .target_model_relative_scores(
    scores = scores,
    target_models = target_models,
    baseline_model = baseline_model,
    by = c("forecast_date", "location"),
    metric_to_compare = metric_to_compare
  ) |>
    dplyr::arrange(dplyr::desc(.data$location)) |>
    order_col("location")

  p <- ggplot(
    data = relative_scores,
    mapping = aes(
      x = .data$forecast_date,
      y = .data$location,
      fill = .data$mean_scores_ratio,
      label = round(.data$mean_scores_ratio, 2)
    )
  ) +
    geom_tile() +
    geom_text(size = 1.5) +
    scale_fill_score_ratio(
      name = .relative_metric_display_name(metric_to_compare),
      limits = forecasttools::sym_limits(
        relative_scores$mean_scores_ratio,
        transform = "log10"
      )
    ) +
    scale_x_weekly_iso_date() +
    get_plot_theme(
      rotate_x_ticks = TRUE
    )

  return(p)
}


#' Make figure that stratifies across location and forecast dates
#'
#' @param scores output of [scoringutils::score()].
#' @param target_models Model for which to plot relative scores
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
# nolint start
plot_rel_score_dists_by_horizon <- function(
  # nolint end
  scores,
  target_models,
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
    target_models = target_models,
    baseline_model = baseline_model,
    metric_to_compare = metric_to_compare,
    by = c("forecast_date", "location", "horizon")
  )

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
    get_plot_theme() +
    scale_fill_horizon() +
    scale_color_horizon()

  return(p)
}

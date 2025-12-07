#' Get plot of overall hub performance, grouped by period
#'
#' @param scores df with granular (daily) scores from every model,
#' forecast_date, and location for the entire time period. Includes the
#' two retrospective models
#' @param figure_file_path path to directory to save figures
#' @param all_time_period string indicating the longer time frame we are
#' comparing, e.g. "Oct 2023-Mar 2024"
#' @param real_time_period string indicating the shorter time frame that
#' we submitted our model to the hub e.g. "Feb 2024-Mar 2024"
#' @param models_to_show A vector of charcter strings indicating which models
#' from the COVID-19 forecast hub to include in the plot.
#' @param summarize_across_horizon Boolean indicating whether or not to
#' average the scores across the horizon, default is `FALSE` meaning
#' each day-forecast-date-location score is in the distribution
#' @param baseline_model which model to compute relative WIS compared to, default
#' is `COVIDhub-4_week_ensemble`
#'
#' @return a ggplot object containing distributions of WIS scores grouped by
#' model and the comaprison time period, with the mean plotted alongside the
#' full distribution
#' @export
#'
plot_hub_performance_by_period <- function(
  scores,
  figure_file_path,
  all_time_period,
  real_time_period,
  models_to_show,
  summarize_across_horizon = FALSE,
  baseline_model = "COVIDhub-4_week_ensemble"
) {
  subset_scores <- scores |>
    dplyr::filter(model %in% !!models_to_show)

  if (isTRUE(summarize_across_horizon)) {
    scores_by_model_all_time <- subset_scores |>
      data.table::as.data.table() |>
      scoringutils::summarise_scores(
        by = c(
          "model",
          "forecast_date",
          "location",
          "horizon"
        )
      ) |>
      dplyr::mutate(
        period = {{ all_time_period }}
      )

    scores_by_model_real_time <- subset_scores |>
      dplyr::filter(
        forecast_date >= lubridate::ymd("2024-02-05")
      ) |>
      data.table::as.data.table() |>
      scoringutils::summarise_scores(
        by = c("model", "forecast_date", "location")
      ) |>
      dplyr::mutate(
        period = {{ real_time_period }}
      )
  } else {
    scores_by_model_all_time <- subset_scores |>
      data.table::as.data.table() |>
      dplyr::mutate(
        period = {{ all_time_period }}
      )
    scores_by_model_real_time <- subset_scores |>
      data.table::as.data.table() |>
      dplyr::filter(
        forecast_date >= lubridate::ymd("2024-02-05")
      ) |>
      dplyr::mutate(
        period = {{ real_time_period }}
      )
  }

  scores <- dplyr::bind_rows(
    scores_by_model_all_time,
    scores_by_model_real_time
  )

  # Want to get the mean across all forecast dates and locations for each
  # model during each period
  mean_scores <- scores |>
    scoringutils::summarise_scores(
      by = c("model", "period")
    ) |>
    dplyr::rename(
      mean_score = .data$wis
    ) |>
    dplyr::select(
      model,
      period,
      mean_score
    )

  baseline_scores <- scores |>
    dplyr::filter(model == {{ baseline_model }}) |>
    dplyr::select(location, forecast_date, horizon, .data$wis) |>
    dplyr::rename(baseline_score = .data$wis)

  scores_final <- scores |>
    dplyr::left_join(mean_scores, by = c("model", "period")) |>
    dplyr::left_join(
      baseline_scores,
      by = c(
        "forecast_date",
        "horizon",
        "location"
      )
    ) |>
    dplyr::mutate(
      relative_wis = .data$wis / .data$baseline_score
    ) |>
    dplyr::filter(model != {{ baseline_model }}) |>
    order_periods()

  colors <- plot_components()

  p <- ggplot(scores_final) +
    tidybayes::stat_halfeye(
      aes(
        x = period,
        y = relative_wis + 1e-8,
        fill = model
      ),
      point_interval = "mean_qi",
      alpha = 0.5,
      position = position_dodge(width = 0.75)
    ) +
    guides(fill = guide_legend(nrow = 2)) +
    coord_trans(ylim = c(0, 2)) +
    get_plot_theme(
      rotate_x_ticks = TRUE
    ) +
    theme(
      legend.justification = "left",
      legend.direction = "horizontal",
      legend.position = "top",
      legend.title = element_blank(),
      legend.text = element_text(size = 7)
    ) +
    scale_fill_manual(values = colors$model_colors) +
    scale_color_manual(values = colors$model_colors) +
    xlab("") +
    ylab(glue::glue("Relative WIS compared \n to {baseline_model}"))

  return(p)
}

#' Plot a histogram of individual forecast relative WIS values
#'
#' @param scores Table of raw scores to plot.
#' @param models_to_show Character vector of models to plot.
#' @param baseline_model Name of the baseline model against
#' which to compute relative scores. Default `"COVIDhub-4_week_ensemble"`.
#' @export
relative_wis_histogram <- function(
  scores,
  models_to_show,
  baseline_model = "COVIDhub-4_week_ensemble"
) {
  scores <- scores |>
    dplyr::filter(.data$model %in% !!models_to_show) |>
    forecasttools::summarise_scores_with_baseline(
      baseline = baseline_model,
      by = c(
        "forecast_date",
        "location"
      )
    ) |>
    dplyr::filter(.data$model != !!baseline_model) |>
    dplyr::rename(relative_wis = "mean_scores_ratio")

  colors <- plot_components()

  p <- ggplot(scores) +
    tidybayes::stat_histinterval(
      aes(
        x = .data$model,
        y = .data$relative_wis,
        fill = model
      ),
      point_interval = "mean_qi",
      alpha = 0.5,
      position = position_dodge(width = 0.75),
    ) +
    scale_y_continuous(trans = "log10") +
    get_plot_theme(
      rotate_x_ticks = TRUE
    ) +
    scale_fill_manual(
      values = colors$model_colors,
      guide = "none"
    ) +
    scale_color_manual(
      values = colors$model_colors,
      guide = "none"
    ) +
    xlab("") +
    theme(
      legend.justification = "left",
      legend.direction = "horizontal",
      legend.position = "none",
      legend.title = element_blank(),
      legend.text = element_text(size = 7)
    ) +
    ylab(glue::glue("Relative WIS compared to \n {baseline_model}"))

  return(p)
}


#' Make a heatmap of relative WIS across locations
#'
#' @param scores df of granular (daily) score across models, locations, forecast
#' dates and horizons
#' @param time_period time period that scores are summarized over
#' @param models_to_show A vector of charcter strings indicating which models
#' from the COVID-19 forecast hub to include in the plot.
#' @param baseline_model which model to compute relative WIS compared to, default
#' is `COVIDhub-4_week_ensemble`
#'
#' @return a ggplot with a heatmap with model on the x-axis, location on the y-axis
#' and fill by relative WIS score across forecast dates and horizons
#' @export
#'
plot_hub_heatmap_relative_wis <- function(
  scores,
  time_period,
  models_to_show,
  baseline_model = "COVIDhub-4_week_ensemble"
) {
  message("Computing relative scores. This may take time...")
  rel_scores <- scores |>
    dplyr::filter(
      .data$model %in%
        c(
          !!baseline_model,
          !!models_to_show
        ),
      .data$location != "US"
    ) |>
    forecasttools::summarise_scores_with_baseline(
      baseline = baseline_model,
      compare = "model",
      metric_to_compare = "wis",
      by = "location"
    ) |>
    dplyr::filter(
      .data$model != !!baseline_model,
      .data$location != "US"
    ) |>
    dplyr::mutate(
      display_score = format(
        .data$mean_scores_ratio,
        digits = 2
      )
    )

  message("Plotting heatmap...")
  p <- ggplot(
    rel_scores,
    aes(
      x = .data$model,
      y = .data$location,
      fill = .data$mean_scores_ratio,
      label = .data$display_score
    )
  ) +
    geom_tile() +
    geom_text(size = 1.5) +
    scale_fill_score_ratio(name = "Relative WIS") +
    get_plot_theme(
      rotate_x_ticks = TRUE
    ) +
    ggtitle(glue::glue(
      "rWIS vs {baseline_model}"
    ))

  return(p)
}

.compute_standardized_ranks <- function(scores) {
  std_ranks <- scores |>
    scoringutils::summarise_scores(
      by = c("model", "location", "forecast_date")
    ) |>
    dplyr::group_by(.data$forecast_date, .data$location) |>
    dplyr::mutate(
      std_rank = dplyr::percent_rank(dplyr::desc(.data$wis))
    ) |>
    dplyr::mutate(
      model = stats::reorder(
        .data$model,
        .data$std_rank,
        FUN = function(x) {
          quantile(x, probs = 0.25, na.rm = TRUE)
        }
      )
    )
  return(std_ranks)
}

#' Make a figure of the distribution of standardized WIS rank
#'
#' @description
#' Adapted from https://www.pnas.org/doi/10.1073/pnas.2113561119
#' and https://github.com/reichlab/covid19-forecast-evals/blob/b741b6a24e40c7f2a8ddc41da40c95b23db6df4e/code/figure-model-ranks.R#L11 #nolint
#'
#'
#' @param scores df of granular (daily) score across models, locations,
#' forecast dates and horizons
#' @param models_to_show Character vector indicating which models
#' from the COVID-19 forecast hub to include.
#' @return A ggplot object containing geom denity ridges plots
#' colored by density, indicating the standardized rank for each
#' location-date combo
#' @export
plot_std_rank_distribution <- function(
  scores,
  models_to_show
) {
  ranks <- .compute_standardized_ranks(scores) |>
    dplyr::filter(model %in% !!models_to_show)

  p <- ggplot(
    ranks,
    aes(
      x = std_rank,
      y = model,
      fill = factor(stat(quantile)),
      height = after_stat(density)
    )
  ) +
    ggridges::stat_density_ridges(
      geom = "density_ridges_gradient",
      calc_ecdf = TRUE,
      quantiles = 4,
      quantile_lines = TRUE,
      jittered_points = TRUE,
      position = ggridges::position_points_jitter(
        width = 0.05,
        height = 0
      ),
      point_shape = "|",
      point_size = 3,
      point_alpha = 1,
      alpha = 0.7,
    ) +
    scale_fill_viridis_d(guide = "none") +
    get_plot_theme() +
    scale_x_continuous(
      name = "Standardized Rank",
      limits = c(0, 1)
    ) +
    ylab("")

  return(p)
}

#' Summarize standardized ranks with medians and 25th,75th percentiles
#'
#' @param scores A tibble of the individual day and location's scores
#'
#' @return A table with median, 25th, and 75th percentiles of standard
#' ranking for each model
#' @export
summarize_std_rank <- function(scores) {
  ranks <- .compute_standardized_ranks(scores)
  summarize_std_rank <- ranks |>
    dplyr::group_by(.data$model) |>
    dplyr::summarise(
      median_rank = quantile(.data$std_rank, 0.5),
      quartile_25th = quantile(.data$std_rank, 0.25),
      quartile_75th = quantile(.data$std_rank, 0.75)
    )
  return(summarize_std_rank)
}

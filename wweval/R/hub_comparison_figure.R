#' Make summary table of WIS scores in Hub models overall
#'
#' @param scores quantile based scores from the hub
#' @return A table with the average scores of each model,
#' where model is an ordered factor.
#' @export
hub_average_score_table <- function(scores) {
  avg_scores <- scores |>
    dplyr::group_by(.data$model) |>
    dplyr::summarise(
      avg_wis = mean(.data$wis),
      avg_bias = mean(.data$bias),
      avg_ae = mean(.data$ae_median)
    ) |>
    dplyr::mutate(model = factor(.data$model,
      levels = as.character(.data$model)[order(.data$avg_wis)]
    )) |>
    dplyr::arrange(.data$model)

  return(avg_scores)
}

#' Plot average WIS as a barplot.
#'
#' @param average_scores Scores for models, with
#' `avg_wis` and `model` columns.
#' @param time_period string indicating the time period covered,
#' for labeling the plot.
#' @param fig_file_dir directory to save figure
#' @return The figure, also saving it to disk as a side effect.
#' @export
barplot_avg_wis <- function(average_scores,
                            time_period,
                            fig_file_dir) {
  colors <- plot_components()
  p <- ggplot(average_scores) +
    geom_bar(
      aes(
        x = .data$model,
        y = .data$avg_wis,
        fill = .data$ model
      ),
      stat = "identity",
      position = "dodge"
    ) +
    get_plot_theme(
      x_axis_dates = TRUE,
      y_axis_title_size = 8
    ) +
    theme(legend.position = "none") +
    scale_fill_manual(values = colors$model_colors) +
    xlab("") +
    ylab("Average WIS") +
    ggtitle(glue::glue(
      "Average WIS across forecast dates ",
      "and locations from {time_period} 2024"
    ))

  ggsave(p,
    filename = file.path(
      fig_file_dir,
      glue::glue("fig_hub_avg_wis_{time_period}.png")
    )
  )

  return(p)
}

#' Make plot of WIS scores in Hub models overall
#'
#' @param scores quantile based scores from the hub
#' @param time_period string indicating which time period to make the plot for
#'
#' @return A plot ordered by average wis over the time period
#' @export
wis_barplot <- function(scores,
                        time_period) {
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
    ylab("Average WIS")

  return(p)
}




#' Get plot of WIS over time
#'
#' @param all_scores Scores from entire time period of interest, including
#' the retrospective cfa model, scored with [scoringutils::score()] and
#' summarized across quantiles only with [scoringutils::summarise_scores()]
#' @param cfa_real_time_scores Real-time scores from Feb - Mar for the cfa ww
#' model submitted to the hub, scored with [scoringutils::score()] and
#' summarized across quantiles only with [scoringutils::summarise_scores()],
#' default is `c()`
#' @param models_to_show A vector of charcter strings indicating which models
#' from the COVID-19 forecast hub to include in the plot.
#' @param time_period String indicating the time period this plot pertains to
#' @param horizon_time_in_weeks horizon time in weeks to summarize over, default
#' is `NULL` which means that the scores are summarized over the nowcast period
#' and the 4 week forecast period
#' @param fig_file_dir string indicating where to save fig, default is NULL
#'
#' @return a ggplot object of WIS scores over time colored by model, for the
#' real-time cfa model from Feb - Mar and the retrospective CFA model over
#' all time points
#' @export
plot_wis_t <- function(all_scores,
                       models_to_show,
                       time_period,
                       horizon_time_in_weeks = NULL,
                       fig_file_dir = NULL) {
  subset_model_scores <- all_scores |>
    dplyr::filter(model %in% !!models_to_show)

  if (!is.null(horizon_time_in_weeks)) {
    scores_by_forecast_date <- scores |>
      data.table::as.data.table() |>
      scoringutils::summarise_scores(by = c(
        "forecast_date",
        "model", "horizon"
      )) |>
      dplyr::filter(horizon_weeks == {
        horizon_time_in_weeks
      })
    title <- glue::glue(
      "Average {horizon_time_in_weeks}-week ahead weighted interval scores by model"
    )
  } else {
    scores_by_forecast_date <- scores |>
      data.table::as.data.table() |>
      scoringutils::summarise_scores(by = c(
        "forecast_date",
        "model"
      ))
    title <- glue::glue("Average WIS by model {time_period}")
  }

  colors <- plot_components()
  p <- ggplot(scores_by_forecast_date) +
    geom_line(
      aes(
        x = forecast_date, y = interval_score,
        color = model
      ),
      size = 1
    ) +
    geom_point(aes(
      x = forecast_date, y = interval_score,
      color = model
    )) +
    guides(color = guide_legend(nrow = 2)) +
    xlab("") +
    ylab("Average WIS across locations") +
    get_plot_theme(
      x_axis_dates = TRUE,
      y_axis_title_size = 8
    ) +
    scale_x_date(
      date_breaks = "2 weeks",
      labels = scales::date_format("%Y-%m-%d")
    ) +
    scale_color_manual(values = colors$model_colors) +
    theme(
      legend.position = "top",
      legend.direction = "horizontal",
      legend.title = element_blank(),
      legend.text = element_text(size = 7)
    )

  if (!is.null(fig_file_dir)) {
    p <- p + guides(color = guide_legend(nrow = 3))
    ggsave(p,
      height = 6, width = 11,
      filename = file.path(
        fig_file_dir,
        glue::glue("sfig_wis_over_time_all_models.png")
      )
    )
  }

  return(p)
}

#' Get plot of overall hub performance, grouped by period
#'
#' @param all_scores df with granular (daily) scores from every model,
#' forecast_date, and location for the entire time period. Includes the
#' two retrospective models
#' @param cfa_real_time_scores df with granular (daily) scores from the
#' submitted cfa ww model for the time period when it was submitted.
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
plot_hub_performance_by_period <- function(all_scores,
                                           cfa_real_time_scores,
                                           figure_file_path,
                                           all_time_period,
                                           real_time_period,
                                           models_to_show,
                                           summarize_across_horizon = FALSE,
                                           baseline_model = "COVIDhub-4_week_ensemble") {
  subset_scores <- all_scores |>
    dplyr::filter(model %in% !!models_to_show)

  if (isTRUE(summarize_across_horizon)) {
    scores_by_model_all_time <- subset_scores |>
      data.table::as.data.table() |>
      scoringutils::summarise_scores(
        by = c("model", "forecast_date", "location", "horizon")
      ) |>
      dplyr::mutate(
        period = {{ all_time_period }}
      )

    scores_by_model_real_time <- subset_scores |>
      dplyr::filter(forecast_date >= lubridate::ymd("2024-02-05")) |>
      dplyr::bind_rows(cfa_real_time_scores) |>
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
      dplyr::filter(forecast_date >= lubridate::ymd("2024-02-05")) |>
      dplyr::bind_rows(cfa_real_time_scores) |>
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
      mean_score = interval_score
    ) |>
    dplyr::select(
      model, period, mean_score
    )

  baseline_scores <- scores |>
    dplyr::filter(model == {{ baseline_model }}) |>
    dplyr::select(location, forecast_date, horizon, interval_score) |>
    dplyr::rename(baseline_score = interval_score)

  scores_final <- scores |>
    dplyr::left_join(mean_scores, by = c("model", "period")) |>
    dplyr::left_join(baseline_scores, by = c(
      "forecast_date", "horizon",
      "location"
    )) |>
    dplyr::mutate(relative_wis = interval_score / baseline_score) |>
    dplyr::filter(model != {{ baseline_model }}) |>
    order_periods()


  colors <- plot_components()

  p <- ggplot(scores_final) +
    tidybayes::stat_halfeye(
      aes(
        x = period, y = relative_wis + 1e-8,
        fill = model
      ),
      point_interval = "mean_qi",
      alpha = 0.5,
      position = position_dodge(width = 0.75)
    ) +
    guides(fill = guide_legend(nrow = 2)) +
    coord_trans(ylim = c(0, 2)) +
    get_plot_theme(
      x_axis_dates = TRUE,
      y_axis_title_size = 8
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
#' @param raw_scores Table of raw scores to plot.
#' @param models_to_show Character vector of models to plot.
#' @export
relative_wis_histogram <- function(raw_scores,
                                   models_to_show,
                                   baseline_model =
                                     "COVIDhub-4_week_ensemble") {
  scores <- raw_scores |>
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

  p <- ggplot(scores_final) +
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
      x_axis_dates = TRUE,
      y_axis_title_size = 8
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
plot_heatmap_relative_wis <- function(scores,
                                      time_period,
                                      models_to_show,
                                      baseline_model = "COVIDhub-4_week_ensemble") {
  summarized_scores <- scores |>
    data.table::as.data.table() |>
    scoringutils::summarise_scores(
      by = c("model", "location")
    ) |>
    dplyr::left_join(wweval::flusight_location_table,
      by = c("location" = "location_code")
    )

  baseline_score <- summarized_scores |>
    dplyr::filter(model == {{ baseline_model }}) |>
    dplyr::rename(
      baseline_wis = interval_score
    ) |>
    dplyr::select(location, baseline_wis)

  relative_scores <- summarized_scores |>
    dplyr::left_join(baseline_score,
      by = c("location")
    ) |>
    dplyr::mutate(
      relative_interval_score = interval_score / baseline_wis
    ) |>
    dplyr::filter(
      model != {{ baseline_model }},
      location != "US",
      model %in% !!models_to_show
    ) # exclude the US bc not available for
  # retro model


  p <- ggplot(relative_scores) +
    geom_tile(aes(x = model, y = short_name, fill = relative_interval_score)) +
    scale_fill_gradient2(
      high = "red", mid = "white", low = "blue", transform = "log2",
      midpoint = 1, guide = "colourbar", aesthetics = "fill"
    ) +
    geom_text(aes(
      x = model, y = short_name,
      label = round(relative_interval_score, 2)
    ), size = 1.5) +
    get_plot_theme(
      x_axis_dates = TRUE,
      y_axis_text_size = 3
    ) +
    xlab("") +
    ylab("") +
    labs(fill = "Relative WIS") +
    ggtitle(glue::glue("Relative WIS compared to \n {baseline_model}"))


  return(p)
}


#' Make a figure of the distribution of standardized WIS rank
#'
#' @description
#' Adapted from https://www.pnas.org/doi/10.1073/pnas.2113561119
#' and https://github.com/reichlab/covid19-forecast-evals/blob/b741b6a24e40c7f2a8ddc41da40c95b23db6df4e/code/figure-model-ranks.R#L11 #nolint
#'
#'
#' @param scores df of granular (daily) score across models, locations, forecast
#' dates and horizons
#' @param models_to_show A vector of charcter strings indicating which models
#' from the COVID-19 forecast hub to include in the plot.
#' @param time_period time period that scores are summarized over
#' @param tp_fp string indicating short name for time period to save figure
#' @param fig_file_dir directory to save figure
#'
#' @return A ggplot object containing geomridges plots colored by density,
#' indicating the standardized rank for each location-date combo
#' @export
make_fig5_density_rank <- function(scores,
                                   models_to_show,
                                   time_period,
                                   tp_fp,
                                   fig_file_dir) {
  summarized_scores <- scores |>
    data.table::as.data.table() |>
    scoringutils::summarise_scores(
      by = c("model", "location", "forecast_date")
    )

  scores_ranked <- summarized_scores |>
    tibble() |>
    dplyr::group_by(forecast_date, location) |>
    dplyr::mutate(
      rank = dplyr::dense_rank(dplyr::desc(interval_score)),
      std_rank = rank / max(rank)
    ) |>
    dplyr::mutate(model = stats::reorder(model, rank,
      FUN = function(x) {
        quantile(x, probs = 0.25, na.rm = TRUE)
      }
    ))

  fq <- scores_ranked |>
    dplyr::group_by(model) |>
    dplyr::summarize(first_quantile = quantile(std_rank,
      probs = 0.25,
      na.rm = TRUE
    )) |>
    dplyr::arrange(first_quantile) |>
    dplyr::mutate(
      fig_order = dplyr::row_number()
    )

  # Can't externally compute fig_order here because is dependent on the scores
  # based on the quantile ranking
  scores_ranked_ordered <- scores_ranked |>
    dplyr::left_join(fq, by = "model") |>
    dplyr::mutate(
      model = forcats::fct_reorder(model, fig_order)
    ) |>
    dplyr::filter(model %in% !!models_to_show)


  p <- ggplot(
    scores_ranked_ordered,
    aes(
      x = std_rank, y = model,
      fill = factor(stat(quantile)),
      height = after_stat(density)
    )
  ) +
    ggridges::stat_density_ridges(
      geom = "density_ridges_gradient", calc_ecdf = TRUE,
      quantiles = 4, quantile_lines = TRUE,
      jittered_points = TRUE,
      position = ggridges::position_points_jitter(width = 0.05, height = 0),
      point_shape = "|", point_size = 3, point_alpha = 1, alpha = 0.7,
    ) +
    scale_fill_viridis_d(guide = "none") +
    get_plot_theme() +
    scale_x_continuous(
      name = "Standardized rank", limits = c(0, 1)
    ) +
    ylab("")

  ggsave(p,
    filename = file.path(
      fig_file_dir,
      glue::glue("sfig_density_rank_{time_period}.png")
    )
  )

  return(p)
}

#' Summarize standardize rank with medians and 25th,75th percentiles
#'
#' @param scores A tibble of the individual day and location's scores
#'
#' @return A table with median, 25th, and 75th percentiles of standard
#' ranking for each model
#' @export
summarize_std_rank <- function(scores) {
  summarized_scores <- scores |>
    data.table::as.data.table() |>
    scoringutils::summarise_scores(
      by = c("model", "location", "forecast_date")
    )

  scores_ranked <- summarized_scores |>
    tibble() |>
    dplyr::group_by(forecast_date, location) |>
    dplyr::mutate(
      rank = dplyr::dense_rank(dplyr::desc(interval_score)),
      std_rank = rank / max(rank)
    ) |>
    dplyr::mutate(model = stats::reorder(model, rank,
      FUN = function(x) {
        quantile(x, probs = 0.25, na.rm = TRUE)
      }
    ))

  summarize_std_rank <- scores_ranked |>
    dplyr::group_by(model) |>
    dplyr::summarise(
      median_rank = quantile(std_rank, 0.5),
      quartile_25th = quantile(std_rank, 0.25),
      quartile_75th = quantile(std_rank, 0.75)
    )
  return(summarize_std_rank)
}



#' Make a figure showing comparisons to Hub models across
#' the entire season and from February onward.
#'
#' @param plot_wis_t average wis over time
#' across locations for ach model in real-time (feb-mar)
#' @param hist_rwis Histogram of relative WIS
#' across location, forecast_date, day, and model
#' @param barplot_wis bar chart in order of average WIS
#' @param heatmap_rel_wis heatmap comparing WIS across
#' forecast dates for each location.
#' @param qq_plot qq plot comparing model coverage
#' @param figure_name Filename for the figure, without the extension.
#' @param fig_file_dir Directory in which to save figures
#'
#' @return a patchwork object containing all the figures combined. As a
#' side effect, saves the figure to disk both as a png and as an svg.
#' @export
#'
compose_and_save_hub_figure <- function(plot_wis_t,
                                        hist_rwis,
                                        barplot_wis,
                                        heatmap_rel_wis,
                                        qq_plot,
                                        figure_name,
                                        fig_file_dir) {
  layout <- "
AABBBB
CCDDEE
"
  composed_fig <- patchwork::wrap_plots(
    hist_rwis,
    plot_wis_t,
    heatmap_rel_wis,
    qq_plot,
    barplot_wis
  ) +
    patchwork::plot_layout(
      design = layout,
      axes = "collect",
      guides = "collect"
    ) & theme(
    legend.position = "bottom"
  )
  # legend.justification = "left" #nolint
  # ) #+ plot_annotation(tag_levels = "A") #nolint, not working


  ggsave(composed_fig,
    filename = fs::path(fig_file_dir,
      figure_name,
      ext = "png"
    ),
    width = 12, height = 10
  )
  ggsave(composed_fig,
    filename = fs::path(fig_file_dir,
      figure_name,
      ext = "svg"
    ),
    width = 12, height = 10
  )

  return(composed_fig)
}

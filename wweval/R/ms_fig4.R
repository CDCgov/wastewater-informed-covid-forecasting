#' Make a figure of overall admissions (summed across locations) for context
#'
#' @param eval_hosp_data Hospital admissions data for evaluating against
#' for all locations
#' @param first_forecast_date The first forecast date we are evaluating
#' @param last_forecast_date The last forecast date we are evaluating
#'
#' @return ggplot object displaying a timeseries of total
#' hospital admissions.
#' @export
plot_total_admissions <- function(eval_hosp_data,
                                  first_forecast_date,
                                  last_forecast_date) {
  hosp_data <- eval_hosp_data |>
    dplyr::distinct(
      .data$location,
      .data$daily_hosp_admits,
      .data$date
    ) |>
    dplyr::group_by(.data$date) |>
    dplyr::summarise(total_hosp = sum(daily_hosp_admits))

  max_total_hosp <- max(total_hosp$total_hosp)

  date_lims <- c(
    as.Date(first_forecast_date),
    as.Date(last_forecast_date)
  )

  p <- ggplot(
    data = hosp_data,
    aes(
      x = .data$date,
      y = .data$total_hosp
    )
  ) +
    geom_point() +
    get_plot_theme(x_axis_dates = TRUE) +
    xlab("") +
    ylab("National admissions") +
    get_plot_theme(
      y_axis_title_size = 8,
      x_axis_dates = TRUE
    ) +
    scale_x_date(
      date_breaks = "1 week",
      date_labels = "%Y-%m-%d",
      limits = date_lims
    )
  return(p)
}


#' Make a summary of the with and without wastewater comparison scores
#'
#' @param scores a tibble of scores for each model, horizon day, forecast date
#' and location for the subset of forecasts used in the head-to-head comparison
#'
#' @return a list of two tables with summary scores, one overall and one by
#' forecast vs nowcast
#' @export
get_score_summary_tables <- function(scores) {
  # Overall avg crps, bias, absolute error etc
  scores_overall <- scoringutils::summarise_scores(scores)
  scores_by_period <- scoringutils::summarise_scores(
    scores,
    by = "period"
  )

  scores_tables <- list(
    scores_overall = scores_overall,
    scores_by_period = scores_by_period
  )

  return(scores_tables)
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
#' @return The relative scores for the target model, as a table.
#' @keywords internal
.target_model_relative_scores <- function(scores,
                                          target_model,
                                          baseline_model,
                                          by = NULL) {
  return(dplyr::filter(
    scores,
    .data$model %in% c(!!target_model, !!baseline_model)
  ) |>
    forecasttools::summarise_scores_with_baseline(
      compare = "model",
      baseline = baseline_model,
      by = by
    ) |>
    dplyr::filter(.data$model == !!target_model))
}

#' Make a CRPS dotsinterval plot stratified by time
#'
#' @param scores A tibble of scores by location, forecast date,
#' date and model,
#' containing the outputs of `scoringutils::score()` on samples,
#' plus metadata, transformed into a tibble.
#' @param target_model Model for which to plot relative CRPS
#' @param baseline_model Baseline model for the relative CRPS
#' computation.
#' @return a ggplot object with the distributions plotted by
#' forecast date.
#' @export
plot_rel_crps_distribution_t <- function(scores,
                                         target_model,
                                         baseline_model) {
  relative_scores <- .target_model_relative_scores(
    scores = scores,
    target_model = target_model,
    baseline_model = baseline_model,
    by = c("forecast_date", "location")
  )

  colors <- plot_components()
  horizon_color <- colors$horizon_colors$overall

  date_lims <- c(range(scores$forecast_date))

  p <- ggplot(data = relative_scores) +
    tidybayes::stat_dotsinterval(
      aes(
        x = .data$forecast_date,
        y = .data$mean_scores_ratio
      ),
      point_interval = "mean_qi",
      alpha = 0.5,
      position = position_dodge(width = 0.75),
      show.legend = FALSE,
      fill = horizon_color,
      color = horizon_color
    ) +
    geom_hline(aes(yintercept = 1), linetype = "dashed") +
    xlab("") +
    ylab("Relative CRPS") +
    scale_x_date() +
    scale_y_continuous(trans = "log10") +
    coord_cartesian(ylim = c(0.5, 2)) +
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
#' @param fig_file_dir A string indicating the directory to save
#' the figures in
#'
#' @return a ggplot object
#' @export
plot_rel_crps_heatmap <- function(scores,
                                  target_model,
                                  baseline_model,
                                  fig_file_dir) {
  relative_scores <- .target_model_relative_scores(
    scores = scores,
    target_model = target_model,
    baseline_model = baseline_model,
    by = c("forecast_date", "location")
  )

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
    labs(fill = "Relative CRPS") +
    ggtitle(glue::glue("Relative CRPS by forecast date and location"))

  return(p)
}

#' Get a dotsinterval plot of the relative CRPS distribution
#' for individual location/forecast-date forecast problems.
#'
#' @param scores table of scores by horizon day,
#' forecast date, and location
#' @param target_model Model for which to plot relative CRPS
#' @param baseline_model Baseline model for the relative CRPS
#' @param fig_file_dir directory to save figure in
#'
#' @return ggplot object of distribution of relative CRPS scores
#' @export
plot_rel_crps_distribution <- function(scores,
                                       target_model,
                                       baseline_model,
                                       fig_file_dir) {
  relative_scores <- .target_model_relative_scores(
    scores = scores,
    target_model = target_model,
    baseline_model = baseline_model,
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
    ylab("Relative CRPS") +
    xlab("Count") +
    scale_y_continuous(trans = "log10") +
    coord_cartesian(ylim = c(1 / 3.5, 3.5))

  return(p)
}



#' Make figure that stratifies scores by location across forecast dates
#'
#' @param scores A tibble of scores by location, forecast date, date and model,
#' containing the outputs of `scoringutils::score()` on samples plus metadata
#' transformed into a tibble.
#' @param target_model Model for which to plot relative CRPS
#' @param baseline_model Baseline model for the relative CRPS
#' @return A ggplot object containing plots of the distribution of relative
#' CRPS scores by location, across forecast dates, colored by location
#' @export
plot_rel_crps_by_location <- function(scores) {
  relative_scores <- .target_model_relative_scores(
    scores = scores,
    target_model = target_model,
    baseline_model = baseline_model,
    by = c("forecast_date", "location")
  )

  colors <- plot_components()
  horizon_color <- colors$horizon_colors$overall

  p <- ggplot(relative_scores) +
    tidybayes::stat_dotsinterval(
      aes(x = .data$location, y = .data$mean_scores_ratio),
      point_interval = "mean_qi",
      alpha = 0.5,
      position = position_dodge(width = 0.75),
      fill = horizon_color,
      show.legend = FALSE
    ) +
    geom_hline(aes(yintercept = 1), linetype = "dashed") +
    theme_bw() +
    get_plot_theme(
      y_axis_title_size = 8,
      x_axis_dates = TRUE
    ) + # bc we want them smaller and turned
    xlab("") +
    ylab("Relative CRPS") +
    scale_y_continuous(trans = "log10") +
    coord_cartesian(ylim = c(0.5, 2)) +
    scale_fill_manual(values = colors$horizon_colors) +
    scale_color_manual(values = colors$horizon_colors)


  return(p)
}

#' Make figure that stratifies across location and forecast dates
#'
#' @param scores A tibble of scores by location, forecast date, date and model,
#' containing the outputs of `scoringutils::score()` on samples plus metadata
#' transformed into a tibble.
#' @param target_model Model for which to plot relative CRPS
#' @param baseline_model Baseline model for the relative CRPS
#' @param horizons_to_show A vector of strings indicating the names of the
#' `horizon` that we want to show on the plot, must be a subset of
#' `nowcast`, `1 wk`, `2 wks`,`3 wks`, `4 wks` and `overall`
#' @param fig_file_dir string indicating directory to save fig, default is NULL
#' @param write_files boolean indicating whether or not to save file, default
#' is FALSE
#'
#' @return A ggplot object containing plots of the distribution of relative
#' CRPS scores across location and forecast dates
#' @export
plot_rel_crps_dists_by_horizon <- function(scores,
                                           target_model,
                                           baseline_model,
                                           horizons_to_show = c(
                                             "nowcast",
                                             "1 wk",
                                             "4 wks",
                                             "overall"
                                           ),
                                           fig_file_dir = NULL,
                                           write_files = FALSE) {
  scores_by_horizon <- scores
  scores_overall <- scores |>
    dplyr::mutate(
      horizon = "overall"
    )

  scores_comb <- dplyr::bind_rows(scores_by_horizon, scores_overall) |>
    dplyr::filter(
      horizon %in% !!horizons_to_show
    )
  relative_scores <- .target_model_relative_scores(
    scores = scores_comb,
    target_model = target_model,
    baseline_model = baseline_model,
    by = c("forecast_date", "location", "horizon")
  )

  colors <- plot_components()


  p <- ggplot(relative_scores) +
    tidybayes::stat_dotsinterval(
      aes(
        x = .data$horizon,
        y = .data$mean_scores_ratio,
        fill = .data$horizon,
        color = .data$horizon
      ),
      point_interval = "mean_qi",
      alpha = 0.5,
      position = position_dodge(width = 0.75),
      show.legend = FALSE
    ) +
    geom_hline(aes(yintercept = 1), linetype = "dashed") +
    xlab("Horizon") +
    ylab("Relative CRPS") +
    scale_y_continuous(trans = "log10") +
    coord_cartesian(ylim = c(0.5, 2)) +
    get_plot_theme(
      y_axis_title_size = 8,
      x_axis_title_size = 8
    ) +
    scale_fill_manual(values = colors$horizon_colors) +
    scale_color_manual(values = colors$horizon_colors)

  if (isTRUE(write_files)) {
    ggsave(p,
      filename = file.path(
        fig_file_dir,
        glue::glue("dist_rel_crps_all_time.png")
      ),
      height = 4,
      width = 6
    )
  }

  return(p)
}


#' Plot average CRPS over time for model comparison
#'
#' @param scores A tibble of scores by location, forecast date, date and model,
#' containing the outputs of `scoringutils::score()` on samples plus metadata
#' transformed into a tibble.
#' @param horizon_time_in_weeks horizon time in weeks to summarize over, default
#' is `NULL` which means that the scores are summarized over the nowcast period
#' and the 4 week forecast period
#'
#' @return a ggplot object plotting the magnitude of the avg crps across
#' locations at each forecast date
#' @export
plot_crps_t <- function(scores,
                        horizon_time_in_weeks = NULL) {
  if (!is.null(horizon_time_in_weeks)) {
    scores_by_forecast_date <- scores |>
      scoringutils::summarise_scores(
        by = c(
          "forecast_date",
          "horizon"
        )
      ) |>
      dplyr::filter(horizon_weeks == !!horizon_time_in_weeks)
  } else {
    scores_by_forecast_date <- scores |>
      scoringutils::summarise_scores(by = c(
        "forecast_date",
        "model"
      ))
  }

  date_lims <- c(
    min(scores$forecast_date),
    max(scores$forecast_date)
  )
  colors <- plot_components()
  p <- ggplot(scores_by_forecast_date) +
    geom_line(
      aes(
        x = .data$forecast_date,
        y = .data$crps,
        color = .data$model
      ),
      size = 1
    ) +
    geom_point(aes(
      x = .data$forecast_date,
      y = .data$crps,
      color = .data$model
    )) +
    labs(
      ylab = "Average CRPS across locations",
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
      date_labels = "%Y-%m-%d",
      limits = date_lims
    ) +
    ylab("CRPS") +
    scale_color_manual(values = colors$model_colors)

  return(p)
}


#' Get a density plot of the relative WIS distribution
#'
#' @param wis_scores tibble of scores by horizon day, forecast date, and location
#'
#' @return ggplot object of distribution of relative CRPS scores
get_plot_rel_wis_distrib <- function(wis_scores,
                                     baseline) {
  relative_wis_by_forecast <- wis_scores |>
    forecasttools::summarise_scores_with_baseline(
      baseline = baseline,
      by = c(
        "location",
        "forecast_date"
      )
    ) |>
    dplyr::rename(
      rel_wis = "model_scores_ratio"
    )

  p <- ggplot(relative_wis_by_forecast) +
    tidybayes::stat_dotsinterval(
      aes(
        y = rel_wis
      ),
      alpha = 0.5,
      position = position_dodge(width = 0.75),
      show.legend = FALSE,
      fill = "darkblue"
    ) +
    geom_hline(aes(yintercept = 1), linetype = "dashed") +
    get_plot_theme() +
    ylab("Relative WIS") +
    xlab("Count") +
    scale_y_continuous(trans = "log10") +
    coord_cartesian(ylim = c(1 / 3.5, 3.5))

  return(p)
}

#' Get a plot of the relative wis from the real-time models
#'
#' @param wis_scores tibble containing the wis for each forecast
#' date and location and horizon day and model
#' @param time_period string indicating dates of analysis, either "Feb-Mar",
#' or "Oct-Mar"
#' @param analysis_type string indicating whether analysis is Real-time
#  or Retrospective
#' @param fig_file_dir string indicating the directory to save the figure
#'
#' @return ggplot object of a heatmap of the realtive wis
make_fig4_heatmap_rel_wis <- function(wis_scores,
                                      time_period,
                                      analysis_type,
                                      fig_file_dir) {
  avg_rel_scores <- wis_scores |>
    dplyr::group_by(forecast_date, location, model) |>
    dplyr::summarize(mean_wis = mean(interval_score)) |>
    tidyr::pivot_wider(
      names_from = model,
      values_from = mean_wis,
      id_cols = c("forecast_date", "location")
    ) |>
    dplyr::mutate(
      rel_mean_wis = ww / hosp
    )
  p <- ggplot(avg_rel_scores) +
    geom_tile(aes(x = forecast_date, y = location, fill = rel_mean_wis)) +
    scale_fill_gradient2(
      high = "red", mid = "white", low = "blue",
      transform = "log2",
      midpoint = 1,
      guide = "colourbar", aesthetics = "fill"
    ) +
    geom_text(aes(
      x = forecast_date, y = location,
      label = round(rel_mean_wis, 2)
    ), size = 1.5) +
    get_plot_theme(
      x_axis_dates = TRUE,
      y_axis_text_size = 4
    ) +
    scale_x_date(
      date_breaks = "1 week",
      labels = scales::date_format("%Y-%m-%d")
    ) +
    xlab("") +
    ylab("") +
    labs(fill = "Relative WIS") +
    ggtitle(glue::glue(" Relative WIS by forecast date and location")) # nolint


  return(p)
}

#' Plot average WIS over time for model comparison
#'
#' @param wis_scores A tibble of scores by location, forecast date, date and model,
#' containing the outputs of `scoringutils::score()` on samples plus metadata
#' transformed into a tibble.
#' @param horizon_time_in_weeks horizon time in weeks to summarize over, default
#' is `NULL` which means that the scores are summarized over the nowcast period
#' and the 4 week forecast period
#'
#' @return a ggplot object plotting the magnitude of the avg wiss across
#' locations at each forecast date
#' @export
make_fig4_avg_wis_over_time <- function(wis_scores,
                                        horizon_time_in_weeks = NULL) {
  wis_scores_renamed <- wis_scores |>
    dplyr::mutate(
      model = case_when(
        model == "ww" ~ "cfa-wwrenewal(real-time)",
        model == "hosp" ~ "cfa-hosponlyrenewal(real-time)*"
      )
    )

  if (!is.null(horizon_time_in_weeks)) {
    scores_by_forecast_date <- wis_scores_renamed |>
      data.table::as.data.table() |>
      scoringutils::summarise_scores(by = c(
        "forecast_date",
        "model", "horizon"
      )) |>
      dplyr::filter(horizon_weeks == {
        horizon_time_in_weeks
      })
  } else {
    scores_by_forecast_date <- wis_scores_renamed |>
      data.table::as.data.table() |>
      scoringutils::summarise_scores(by = c(
        "forecast_date",
        "model"
      ))
  }

  date_lims <- c(
    min(wis_scores$forecast_date),
    max(wis_scores$forecast_date)
  )
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
    labs(
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
      date_labels = "%Y-%m-%d",
      limits = date_lims
    ) +
    ylab("WIS") +
    scale_color_manual(values = colors$model_colors)

  return(p)
}

#' Make a CRPS density plot for a subset of locations
#'
#' @param wis_scores A tibble of scores by location, forecast date, date and model,
#' containing the outputs of `scoringutils::score()` on samples plus metadata
#' transformed into a tibble.
#' @return a ggplot object that is a vertical facet of violin plots colored
#' by model type and broken down my horizon
#' @export
make_fig4_rel_wis_over_time <- function(wis_scores) {
  scores_overall <- wis_scores |>
    dplyr::mutate(
      horizon = "overall"
    )

  relative_wis <- scores_overall |>
    data.table::as.data.table() |>
    scoringutils::summarise_scores(by = c(
      "forecast_date",
      "location",
      "model",
      "horizon"
    )) |>
    tidyr::pivot_wider(
      names_from = model,
      values_from = interval_score,
      id_cols = c("horizon", "forecast_date", "location")
    ) |>
    dplyr::mutate(
      rel_wis = ww / hosp
    ) |>
    dplyr::filter(!is.na(rel_wis)) |>
    order_locations(score_name = "rel_wis")


  colors <- plot_components()
  date_lims <- c(range(wis_scores$forecast_date))

  p <- ggplot(relative_wis) +
    tidybayes::stat_dotsinterval(
      aes(
        x = as.factor(forecast_date), y = rel_wis,
        fill = horizon
      ),
      point_interval = "mean_qi",
      alpha = 0.5,
      position = position_dodge(width = 0.75),
      show.legend = FALSE
    ) +
    geom_hline(aes(yintercept = 1), linetype = "dashed") +
    xlab("") +
    ylab("Relative WIS") +
    scale_y_continuous(trans = "log10") +
    coord_cartesian(ylim = c(0.5, 2)) +
    get_plot_theme(
      x_axis_dates = TRUE,
      y_axis_title_size = 8
    ) +
    scale_fill_manual(values = colors$horizon_colors) +
    scale_color_manual(values = colors$horizon_colors)

  return(p)
}

#' Make figure that stratifies scores by location across forecast dates
#'
#' @param wis_scores A tibble of scores by location, forecast date, date and model,
#' containing the outputs of `scoringutils::score()` on samples plus metadata
#' transformed into a tibble.
#'
#' @return A ggplot object containing plots of the distribution of relative
#' WIS scores by location, across forecast dates, colored by location
#' @export
make_fig4_rel_wis_by_location <- function(wis_scores) {
  scores_overall <- wis_scores |>
    dplyr::mutate(
      horizon = "overall"
    )

  relative_wis <- scores_overall |>
    data.table::as.data.table() |>
    scoringutils::summarise_scores(by = c(
      "forecast_date",
      "location",
      "model",
      "horizon"
    )) |>
    tidyr::pivot_wider(
      names_from = model,
      values_from = interval_score,
      id_cols = c("horizon", "forecast_date", "location")
    ) |>
    dplyr::mutate(
      rel_wis = ww / hosp
    ) |>
    dplyr::filter(!is.na(rel_wis)) |>
    order_locations(score_name = "rel_wis")

  colors <- plot_components()

  p <- ggplot(relative_wis) +
    tidybayes::stat_dotsinterval(
      aes(
        x = location, y = rel_wis,
        fill = horizon
      ),
      point_interval = "mean_qi",
      alpha = 0.5,
      position = position_dodge(width = 0.75),
      show.legend = FALSE
    ) +
    geom_hline(aes(yintercept = 1), linetype = "dashed") +
    theme_bw() +
    get_plot_theme(
      y_axis_title_size = 8,
      x_axis_dates = TRUE
    ) + # bc we want them smaller and turned
    xlab("") +
    ylab("Relative WIS") +
    scale_y_continuous(trans = "log10") +
    coord_cartesian(ylim = c(0.5, 2)) +
    scale_fill_manual(values = colors$horizon_colors) +
    scale_color_manual(values = colors$horizon_colors)


  return(p)
}




#' Make a multi-panel figure summarizing relative
#' performance.
#'
#' @param rel_crps_heatmap heatmap of relative crps by forecast date
#' and location
#' @param rel_crps_dist distibution plot comparing overall distribution
#' of crps scores across forecast_date, date, location, and model
#' @param abs_crps_over_time avg crps across locations by forecast date
#' @param natl_admissions total admissions by day
#' @param rel_crps_dist_over_time plots of the distribution of location
#' -specific relative crps values over time (by forecast date).
#' @param rel_crps_by_location avg crps across forecast dates by state
#' @param time_period string to save fig as, either "real_time" or
#' "all_time"
#' @param fig_file_dir Path to save figures
#'
#' @return ggplot object with all the elements combined
#' @export
compose_rel_performance_fig <- function(rel_crps_heatmap,
                                        rel_crps_dist,
                                        abs_crps_over_time,
                                        total_admissions,
                                        rel_crps_dist_over_time,
                                        rel_crps_by_location,
                                        time_period,
                                        fig_file_dir) {
  layout <- "
AACCC
AADDD
BBEEE
BBFFF
"

  fig <- patchwork::wrap_plots(
    rel_crps_heatmap,
    rel_crps_dist,
    total_admissions,
    abs_crps_over_time,
    rel_crps_dist_over_time,
    rel_crps_by_location,
    design = layout,
    axes = "collect"
  ) & theme(
    legend.position = "top",
    legend.justification = "left"
  )

  fs::dir_create(fig_file_dir)

  ggsave(fig,
    filename = file.path(
      fig_file_dir,
      glue::glue("retro_rel_performance_{time_period}.png")
    ),
    width = 10, height = 8
  )

  ggsave(fig,
    filename = file.path(
      fig_file_dir,
      glue::glue("retro_rel_performance_{time_period}.svg")
    ),
    width = 10, height = 8
  )

  return(fig)
}

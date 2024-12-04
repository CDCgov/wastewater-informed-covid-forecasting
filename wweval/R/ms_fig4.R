#' Make a summary of the with and without wastewater comparison scores
#'
#' @param scores a tibble of scores for each model, horizon day, forecast date
#' and location for the subset of forecasts used in the head-to-head comparison
#'
#' @return a list of two tables with summary scores, one overall and one by
#' forecast vs nowcast
#' @export
make_fig4_results_table <- function(scores) {
  # Overall avg crps, bias, absolute error etc
  scores_overall <- scores |>
    dplyr::group_by(model) |>
    dplyr::summarise(
      avg_crps = mean(crps),
      avg_bias = mean(bias),
      avg_ae = mean(ae_median)
    )

  # Above was averaged across models, get avg of rel_crps
  overall_all_time_rel_crps <- scores |>
    compute_relative_crps(id_cols = c(
      "location",
      "forecast_date", "date", "horizon"
    )) |>
    dplyr::summarize(mean_rel_crps = mean(rel_crps, na.rm = TRUE))


  # By period (nowcast vs forecast)
  scores_by_period <- scores |>
    dplyr::group_by(model, period) |>
    dplyr::summarise(
      avg_crps = mean(crps),
      avg_bias = mean(bias),
      avg_ae = mean(ae_median)
    )

  scores_tables <- list(
    scores_overall = scores_overall,
    scores_by_period = scores_by_period,
    overall_all_time_rel_crps = overall_all_time_rel_crps
  )

  return(scores_tables)
}


#' Make a CRPS density plot for a subset of locations
#'
#' @param scores A tibble of scores by location, forecast date, date and model,
#' containing the outputs of `scoringutils::score()` on samples plus metadata
#' transformed into a tibble.
#' @return a ggplot object that is a vertical facet of violin plots colored
#' by model type and broken down my horizon
#' @export
make_fig4_rel_crps_over_time <- function(scores) {
  scores_overall <- scores |>
    dplyr::mutate(
      horizon = "overall"
    )

  relative_crps <- scores_overall |>
    dplyr::group_by(forecast_date, location, model, horizon) |>
    dplyr::summarize(mean_crps = mean(crps)) |>
    tidyr::pivot_wider(
      names_from = model,
      values_from = mean_crps,
      id_cols = c("horizon", "forecast_date", "location")
    ) |>
    dplyr::mutate(
      rel_crps = ww / hosp
    )


  colors <- plot_components()
  date_lims <- c(range(scores$forecast_date))

  p <- ggplot(relative_crps) +
    tidybayes::stat_dotsinterval(
      aes(
        x = as.factor(forecast_date), y = rel_crps,
        fill = horizon
      ),
      point_interval = "mean_qi",
      alpha = 0.5,
      position = position_dodge(width = 0.75),
      show.legend = FALSE
    ) +
    geom_hline(aes(yintercept = 1), linetype = "dashed") +
    xlab("") +
    ylab("Relative CRPS") +
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

#' Make a figure of the percent of locations with a better forecast with ww
#' over time
#'
#' @param scores A tibble of scores by location, forecast date, date and model,
#' containing the outputs of `scoringutils::score()` on samples plus metadata
#' transformed into a tibble.
#' @param eval_hosp_data The retrospective hospital admissions data for each
#' state and for all dates, used to generate national hospital admissions
#' @param days_to_show_prev_data An ingeger indicating how many days before the
#' last forecast date to show the data, default is `14`
#'
#' @return a ggplot object containing a stacked bar chart of the percent of
#' states with imprved forecasts from wastewater by forecast date alongside
#' daily national hospital admissions
#' @export
make_fig4_pct_better_w_ww <- function(scores,
                                      eval_hosp_data,
                                      days_to_show_prev_data = 14) {
  pct_better_w_ww <- scores |>
    data.table::as.data.table() |>
    scoringutils::summarise_scores(by = c(
      "forecast_date", "location",
      "model"
    )) |>
    tidyr::pivot_wider(
      id_cols = c(
        forecast_date, location
      ),
      names_from = model,
      values_from = crps
    ) |>
    dplyr::mutate(
      better_w_ww = ifelse(ww < hosp, 1, 0)
    ) |>
    dplyr::group_by(forecast_date) |>
    dplyr::summarise(
      pct_better_w_ww = 100 * mean(better_w_ww),
      pct_better_w_hosp = 100 - pct_better_w_ww
    ) |>
    tidyr::pivot_longer(!forecast_date)

  total_hosp <- eval_hosp_data |>
    dplyr::filter(date >=
      min(pct_better_w_ww$forecast_date) - lubridate::days(
        !!days_to_show_prev_data
      )) |>
    distinct(location, daily_hosp_admits, date) |>
    dplyr::group_by(date) |>
    dplyr::summarise(
      total_hosp = sum(daily_hosp_admits)
    )
  max_total_hosp <- max(total_hosp$total_hosp)


  p <- ggplot() +
    geom_bar(
      data = pct_better_w_ww,
      aes(x = forecast_date, y = value, fill = name),
      stat = "identity", position = "stack"
    ) +
    geom_point(
      data = total_hosp,
      aes(x = date, y = 100 * total_hosp / max(total_hosp))
    ) +
    get_plot_theme(x_axis_dates = TRUE) +
    scale_y_continuous(
      # don't expand y scale at the lower end
      expand = expansion(mult = c(0, 0.05))
    )
  xlab("") +
    scale_y_continuous(
      "Percent better with wastewater",
      sec.axis = sec_axis(~ . * max_total_hosp / 100, name = "National admissions")
    )

  return(p)
}

#' Make a figure of overall admissions for context
#'
#' @param eval_hosp_data Hospital admissions data for evaluating against,
#' for all locations
#' @param first_forecast_date The first forecast date we are evaluating
#' @param last_forecast_date The last forecast date we are evaluating
#'
#' @return ggplot object containing total hospital admissions in the US
#' @export
make_fig4_admissions_overall <- function(eval_hosp_data,
                                         first_forecast_date,
                                         last_forecast_date) {
  total_hosp <- eval_hosp_data |>
    distinct(location, daily_hosp_admits, date) |>
    dplyr::group_by(date) |>
    dplyr::summarise(
      total_hosp = sum(daily_hosp_admits)
    )

  max_total_hosp <- max(total_hosp$total_hosp)

  date_lims <- c(
    as.Date(first_forecast_date),
    as.Date(last_forecast_date)
  )

  p <- ggplot() +
    geom_point(
      data = total_hosp,
      aes(x = date, y = total_hosp)
    ) +
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

#' Get the mean relative crps for a location
#'
#' @param scores tibble of scores by day forecast day model
#' @param locs loc to get mean relative score for
#'
#' @return table of mean relative score for each location
get_loc_rel_crps <- function(scores, locs) {
  relative_crps <- scores |>
    dplyr::filter(location %in% locs) |>
    compute_relative_crps(id_cols = c(
      "location", "forecast_date", "date"
    )) |>
    dplyr::group_by(location) |>
    dplyr::summarise(mean = mean(rel_crps))

  return(relative_crps)
}

#' Plot a heatmap of the relative crps by locations and forecast date
#' for the head-to-head comparison
#'
#' @param scores A tibble of daily scores by forecast date, location, and model
#' @param fig_file_dir A string indicating the directory to save the figures in
#'
#' @return a ggplot object
#' @export
get_plot_rel_crps_heatmap <- function(scores,
                                      fig_file_dir) {
  scores_summary <- scores |>
    compute_relative_crps(id_cols = c(
      "location",
      "forecast_date", "date"
    )) |>
    dplyr::group_by(location, forecast_date) |>
    dplyr::summarize(
      mean_rel_crps = mean(rel_crps)
    )


  p <- ggplot(scores_summary) +
    geom_tile(aes(x = forecast_date, y = location, fill = mean_rel_crps)) +
    scale_fill_gradient2(
      high = "red", mid = "white", low = "blue",
      transform = "log2",
      midpoint = 1,
      guide = "colourbar", aesthetics = "fill"
    ) +
    geom_text(aes(
      x = forecast_date, y = location,
      label = round(mean_rel_crps, 2)
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
    ylab("Location") +
    labs(fill = "Relative CRPS") +
    ggtitle(glue::glue("Relative CRPS by forecast date and location"))

  return(p)
}

#' Get a density plot of the relative CRPS distribution
#'
#' @param scores tibble of scores by horizon day, forecast date, and location
#' @param fig_file_dir directory to save figure in
#'
#' @return ggplot object of distribution of relative CRPS scores
get_plot_rel_crps_distrib <- function(scores,
                                      fig_file_dir) {
  relative_crps_by_forecast <- scores |>
    dplyr::group_by(location, model, forecast_date) |>
    dplyr::summarize(crps = mean(crps)) |>
    tidyr::pivot_wider(
      names_from = model,
      values_from = crps,
      id_cols = c(
        "location", "forecast_date"
      )
    ) |>
    dplyr::mutate(
      pct_change_crps = (ww - hosp) / hosp,
      rel_crps = ww / hosp
    )

  p_log <- ggplot(relative_crps_by_forecast) +
    tidybayes::stat_dotsinterval(
      aes(
        y = rel_crps
      ),
      alpha = 0.5,
      position = position_dodge(width = 0.75),
      show.legend = FALSE,
      fill = "darkblue"
    ) +
    geom_hline(aes(yintercept = 1), linetype = "dashed") +
    get_plot_theme() +
    ylab("Relative CRPS") +
    xlab("Density") +
    scale_y_continuous(trans = "log10") +
    coord_cartesian(ylim = c(1 / 3.5, 3.5))

  return(p_log)
}



#' Make figure that stratifies scores by location across forecast dates
#'
#' @param scores A tibble of scores by location, forecast date, date and model,
#' containing the outputs of `scoringutils::score()` on samples plus metadata
#' transformed into a tibble.
#'
#' @return A ggplot object containing plots of the distribution of relative
#' CRPS scores by location, across forecast dates, colored by location
#' @export
make_fig4_rel_crps_by_location <- function(scores) {
  scores_overall <- scores |>
    dplyr::mutate(
      horizon = "overall"
    )


  relative_crps <- scores_overall |>
    dplyr::group_by(forecast_date, location, model, horizon) |>
    dplyr::summarize(mean_crps = mean(crps)) |>
    tidyr::pivot_wider(
      names_from = model,
      values_from = mean_crps,
      id_cols = c("horizon", "forecast_date", "location")
    ) |>
    dplyr::mutate(
      rel_crps = ww / hosp
    ) |>
    order_locations(score_name = "rel_crps")

  colors <- plot_components()

  p <- ggplot(relative_crps) +
    tidybayes::stat_dotsinterval(
      aes(
        x = location, y = rel_crps,
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
make_fig4_rel_crps_overall <- function(scores,
                                       horizons_to_show = c(
                                         "nowcast",
                                         "1 wk", "4 wks",
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

  relative_crps <- scores_comb |>
    dplyr::group_by(forecast_date, location, model, horizon) |>
    dplyr::summarize(mean_crps = mean(crps)) |>
    tidyr::pivot_wider(
      names_from = model,
      values_from = mean_crps,
      id_cols = c("horizon", "forecast_date", "location")
    ) |>
    dplyr::mutate(
      rel_crps = ww / hosp
    ) |>
    order_horizons()

  colors <- plot_components()



  p <- ggplot(relative_crps) +
    tidybayes::stat_dotsinterval(
      aes(
        x = horizon, y = rel_crps,
        fill = horizon, color = horizon
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
        glue::glue("sfig_hist_overall_rel_crps_all_time.png")
      ),
      height = 4,
      width = 6
    )
  }

  return(p)
}

#' Make a qq plot
#' @description
#' Using [scoringutils::plot_quantile_coverage()]
#'
#'
#' @param scores_quantiles A tibble of scores by location, forecast date,
#' date and model, containing the outputs of `scoringutils::score()` on
#' quantiles plus metadata transformed into a tibble.
#' @param fig_file_dir string indicating directory to save figure,
#' default is NULL
#' @param write_files boolean indicating whether to save the file, default is
#' `FALSE`
#'
#' @return a ggplot object with the overall QQ plot colored by model.
#' @export
make_qq_plot_overall <- function(scores_quantiles,
                                 fig_file_dir = NULL,
                                 write_files = FALSE) {
  colors <- plot_components()
  p <- scores_quantiles |>
    data.table::as.data.table() |>
    scoringutils::summarise_scores(by = c("model", "quantile")) |>
    scoringutils::plot_quantile_coverage() +
    ggtitle(glue::glue("QQ plot all-time")) +
    get_plot_theme() +
    labs(
      ylab = "Percent of data below quantile",
      col = "Model"
    ) +
    theme(legend.position = "none") +
    scale_color_manual(values = colors$model_colors)
  if (isTRUE(write_files)) {
    ggsave(p,
      filename = file.path(
        fig_file_dir,
        glue::glue("sfig_qq_plot_retro_all_time.png")
      )
    )
  }


  return(p)
}

#' Plot coverage at specified ranges
#'
#' @param scores_quantiles A tibble of scores by location, forecast date,
#' date and model, containing the outputs of `scoringutils::score()` on
#' quantiles plus metadata transformed into a tibble.
#'
#' @param ranges A numeric vector of credible interval ranges to plot,
#' spanning from 0 to 100.
#' @param fig_file_dir string indicating directory to save figure in,
#' default is `NULL`
#' @param write_files boolean indicating whether to save file, default is FALSE
#'
#' @return A ggplot2 object
#'
make_plot_coverage_range <- function(scores_quantiles,
                                     ranges,
                                     fig_file_dir = NULL,
                                     write_files = FALSE) {
  scores_by_horizon <- scores_quantiles |>
    dplyr::mutate(
      horizon_weeks = dplyr::case_when(
        horizon_days <= -7 ~ -2,
        horizon_days > -7 & horizon_days <= 0 ~ -1,
        horizon_days > 0 & horizon_days <= 6 ~ 1,
        horizon_days > 6 & horizon_days <= 13 ~ 2,
        horizon_days > 13 & horizon_days <= 21 ~ 3,
        horizon_days > 21 ~ 4,
        TRUE ~ NA
      )
    )
  coverage_summarized <- scores_by_horizon |>
    dplyr::filter(range %in% c(!!ranges)) |>
    dplyr::group_by(horizon, model, range) |>
    dplyr::summarise(pct_interval_coverage = 100 * mean(coverage)) |>
    order_horizons()

  if (nrow(coverage_summarized |> dplyr::filter(is.na(horizon))) > 0) {
    warning("Horizon is missing for some data points")
  }

  coverage_summarized <- coverage_summarized |>
    dplyr::filter(!is.na(horizon)) |>
    dplyr::mutate(
      named_facet = glue::glue("{range}%")
    )

  colors <- plot_components()
  p <- ggplot(coverage_summarized) +
    aes(
      x = horizon, y = pct_interval_coverage, color = model,
      group = model
    ) +
    geom_line() +
    geom_point() +
    geom_hline(aes(yintercept = range), linetype = "dashed") +
    facet_wrap(~named_facet, scales = "free_y") +
    labs(
      y = "Proportion of data within interval",
      x = "Forecast horizon",
      col = "Model"
    ) +
    scale_y_continuous(expand = expansion(c(0, 0.2))) +
    get_plot_theme(
      x_axis_dates = TRUE
    ) +
    scale_color_manual(values = colors$model_colors)

  if (isTRUE(write_files)) {
    ggsave(p,
      filename = file.path(
        fig_file_dir,
        glue::glue("sfig_coverage_range_retro_all_time.png")
      ),
      height = 4,
      width = 10
    )
  }
  return(p)
}

#' Make figure that plots distribution of relative crps stratified by
#' epidemic phase
#'
#' @param scores A tibble of scores by location, forecast date, date and model,
#' containing the outputs of `scoringutils::score()` on samples plus metadata
#' transformed into a tibble.
#'
#' @return A ggplot object containing plots of the distribution of relative
#' CRPS scores stratified by epidemic phase, across
#' locations and forecast dates
#' @export
make_fig4_rel_crps_by_phase <- function(scores) {
  scores_w_fig_order <- scores |>
    order_phases()

  # Quick warning if there are NAs in epidemic phases
  missing_phases <- scores |>
    dplyr::filter(is.na(phase))

  if (nrow(missing_phases) > 0) {
    warning("There are dates missing epidemic phases")
  }

  relative_crps <- scores_w_fig_order |>
    compute_relative_crps(id_cols = c(
      "location",
      "date", "forecast_date",
      "horizon", "phase"
    )) |>
    dplyr::filter(!is.na(phase)) |>
    dplyr::filter(!is.na(horizon)) |>
    order_phases() |>
    order_horizons()


  colors <- plot_components()

  p <- ggplot(relative_crps) +
    tidybayes::stat_halfeye(
      aes(
        x = as.factor(phase), y = rel_crps,
        fill = phase
      ),
      point_interval = "mean_qi",
      alpha = 0.5,
      position = position_dodge(width = 0.75),
      show.legend = FALSE
    ) +
    geom_hline(aes(yintercept = 1), linetype = "dashed") +
    xlab("Epidemic phase") +
    ylab("Relative CRPS") +
    scale_y_continuous(trans = "log10", limits = c(0.5, 2)) +
    get_plot_theme(
      x_axis_title_size = 8,
      y_axis_title_size = 8
    ) +
    scale_fill_manual(values = colors$phase_colors)

  return(p)
}
#' Make figure that plots distribution of absolute crps stratified by
#' epidemic phase
#'
#' @param scores A tibble of scores by location, forecast date, date and model,
#' containing the outputs of `scoringutils::score()` on samples plus metadata
#' transformed into a tibble.
#'
#' @return A ggplot object containing plots of the distribution of
#' continuous ranked probability scores, colored by model, stratified by epidemic phase, across
#' locations and forecast dates
#' @export
make_sfig_crps_by_phase <- function(scores) {
  scores_w_fig_order <- scores |>
    order_phases()

  # Quick warning if there are NAs in epidemic phases
  missing_phases <- scores |>
    dplyr::filter(is.na(phase))

  if (nrow(missing_phases) > 0) {
    warning("There are dates missing epidemic phases")
  }

  # Going to keep this in there for now
  scores_to_plot <- scores_w_fig_order |>
    dplyr::filter(!is.na(phase))

  p <- ggplot(scores_to_plot) +
    tidybayes::stat_halfeye(
      aes(
        x = as.factor(phase), y = crps,
        fill = model
      ),
      point_interval = "mean_qi",
      alpha = 0.5,
      position = position_dodge(width = 0.75),
    ) +
    xlab("Epidemic phase") +
    ylab("CRPS") +
    scale_y_continuous(limits = c(0.0, 1)) +
    get_plot_theme(
      x_axis_title_size = 8,
      y_axis_title_size = 8
    ) +
    scale_fill_manual(values = colors$model_colors)
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
make_fig4_avg_crps_over_time <- function(scores,
                                         horizon_time_in_weeks = NULL) {
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
  } else {
    scores_by_forecast_date <- scores |>
      data.table::as.data.table() |>
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
        x = forecast_date, y = crps,
        color = model
      ),
      size = 1
    ) +
    geom_point(aes(
      x = forecast_date, y = crps,
      color = model
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
get_plot_rel_wis_distrib <- function(wis_scores) {
  relative_wis_by_forecast <- wis_scores |>
    data.table::as.data.table() |>
    scoringutils::summarise_scores(by = c(
      "location",
      "model",
      "forecast_date"
    )) |>
    tidyr::pivot_wider(
      names_from = model,
      values_from = interval_score,
      id_cols = c(
        "location", "forecast_date"
      )
    ) |>
    dplyr::mutate(
      pct_change_crps = (ww - hosp) / hosp,
      rel_wis = ww / hosp
    )
  p_log <- ggplot(relative_wis_by_forecast) +
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
    xlab("Density") +
    scale_y_continuous(trans = "log10") +
    coord_cartesian(ylim = c(1 / 3.5, 3.5))

  return(p_log)
}

#' Get a plot of the relative wis from the real-time models
#'
#' @param rel_scores tibble containing the relative wis for each forecast
#' date and location and horizon day
#' @param time_period string indicating dates of analysis, either "Feb-Mar",
#' or "Oct-Mar"
#' @param analysis_type string indicating whether analysis is Real-time
#  or Retrospective
#' @param fig_file_dir string indicating the directory to save the figure
#'
#' @return ggplot object of a heatmap of the realtive wis
make_fig4_heatmap_rel_wis <- function(rel_scores,
                                      time_period,
                                      analysis_type,
                                      fig_file_dir) {
  avg_rel_scores <- rel_scores |>
    dplyr::group_by(forecast_date, location) |>
    dplyr::summarise(mean_rel_wis = mean(rel_wis)) |>
    dplyr::filter(!is.na(mean_rel_wis))

  p <- ggplot(avg_rel_scores) +
    geom_tile(aes(x = forecast_date, y = location, fill = mean_rel_wis)) +
    scale_fill_gradient2(
      high = "red", mid = "white", low = "blue",
      transform = "log2",
      midpoint = 1,
      guide = "colourbar", aesthetics = "fill"
    ) +
    geom_text(aes(
      x = forecast_date, y = location,
      label = round(mean_rel_wis, 2)
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
        model == "hosp" ~ "cfa-hosponlyrenewal(real-time)"
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




#' Make Figure 4
#'
#' @param fig4_rel_crps_heatmap heatmap of relative crps by forecast date
#' and location
#' @param fig4_rel_crps_hist histogram comparing overall distribution
#' of crps scores across forecast_date, date, location, and model
#' @param fig4_avg_crps avg crps across locations by forecast date
#' @param fig4_natl_admissions national admissions by day
#' @param fig4_rel_crps_over_time relative crps across locations by forecast
#' date
#' @param fig4_rel_crps_by_location avg crps across forecast dates by state
#' @param time_period string to save fig as, either "real_time" or "all_time"
#' @param fig_file_dir Path to save figures
#'
#' @return ggplot object with all the elements combined
#' @export
make_fig4 <- function(fig4_rel_crps_heatmap,
                      fig4_rel_crps_hist,
                      fig4_avg_crps,
                      fig4_natl_admissions,
                      fig4_rel_crps_over_time,
                      fig4_rel_crps_by_location,
                      time_period,
                      fig_file_dir) {
  layout <- "
AACCC
AADDD
BBEEE
BBFFF
"

  fig4 <- fig4_rel_crps_heatmap +
    fig4_rel_crps_hist +
    fig4_natl_admissions +
    fig4_avg_crps +
    fig4_rel_crps_over_time +
    fig4_rel_crps_by_location +
    patchwork::plot_layout(
      design = layout,
      axes = "collect"
    ) & theme(
    legend.position = "top",
    legend.justification = "left"
  ) #+ plot_annotation(tag_levels = "A") #nolint not working
  fig4

  fs::dir_create(fig_file_dir)

  ggsave(fig4,
    filename = file.path(
      fig_file_dir,
      glue::glue("fig4_{time_period}.png")
    ),
    width = 10, height = 8
  )

  ggsave(fig4,
    filename = file.path(
      fig_file_dir,
      glue::glue("fig4_{time_period}.svg")
    ),
    width = 10, height = 8
  )

  return(fig4)
}

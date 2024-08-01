#' Make a CRPS density plot for a subset of locations
#'
#' @param scores A tibble of scores by location, forecast date, date and model,
#' containing the outputs of `scoringutils::score()` on samples plus metadata
#' transformed into a tibble.
#' @param horizons_to_show A vector of strings indicating the names of the
#' `horizon` that we want to show on the plot, must be a subset of
#' `nowcast`, `1 wk`, `2 wks`,`3 wks`, `4 wks` and `overall`
#'
#' @return a ggplot object that is a vertical facet of violin plots colored
#' by model type and broken down my horizon
#' @export
make_fig4_rel_crps_over_time <- function(scores,
                                         horizons_to_show = c(
                                           "nowcast",
                                           "1 wk", "4 wks",
                                           "overall"
                                         )) {
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
    dplyr::select(
      location, forecast_date, date, model, horizon, crps
    ) |>
    dplyr::filter(!is.na(horizon)) |>
    tidyr::pivot_wider(
      names_from = model,
      values_from = crps,
      id_cols = c(location, forecast_date, date, horizon)
    ) |>
    dplyr::mutate(
      rel_crps = ww / hosp
    ) |>
    order_horizons()

  colors <- plot_components()

  p <- ggplot(
    relative_crps,
    aes(
      x = as.factor(forecast_date), y = rel_crps, color = horizon,
      fill = horizon
    ),
    show.legend = FALSE
  ) +
    tidybayes::stat_halfeye(
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
    scale_y_continuous(trans = "log10", limits = c(0.5, 2)) +
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
#'
#' @return ggplot object containing total hospital admissions in the US
#' @export
make_fig4_admissions_overall <- function(eval_hosp_data) {
  total_hosp <- eval_hosp_data |>
    distinct(location, daily_hosp_admits, date) |>
    dplyr::group_by(date) |>
    dplyr::summarise(
      total_hosp = sum(daily_hosp_admits)
    )
  max_total_hosp <- max(total_hosp$total_hosp)

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
      labels = scales::date_format("%Y-%m-%d"),
      limits = as.Date(c("2023-10-16", "2024-03-11")),
      expand = expansion(c(0.03, 0.03))
    )
  return(p)
}

#' Make figure that stratifies scores by location across forecast dates
#'
#' @param scores A tibble of scores by location, forecast date, date and model,
#' containing the outputs of `scoringutils::score()` on samples plus metadata
#' transformed into a tibble.
#' @param horizons_to_show A vector of strings indicating the names of the
#' `horizon` that we want to show on the plot, must be a subset of
#' `nowcast`, `1 wk`, `2 wks`,`3 wks`, `4 wks` and `overall`
#'
#' @return A ggplot object containing plots of the distribution of relative
#' CRPS scores by location, across forecast dates, colored by location
#' @export
make_fig4_rel_crps_by_location <- function(scores,
                                           horizons_to_show = c(
                                             "nowcast",
                                             "1 wk", "4 wks",
                                             "overall"
                                           )) {
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
    dplyr::select(location, forecast_date, date, model, horizon, crps) |>
    dplyr::filter(!is.na(horizon)) |>
    tidyr::pivot_wider(
      names_from = model,
      values_from = crps,
      id_cols = c(location, forecast_date, date, horizon)
    ) |>
    dplyr::mutate(
      rel_crps = ww / hosp
    ) |>
    order_horizons()

  colors <- plot_components()

  p <- ggplot(relative_crps, aes(
    x = location, y = rel_crps, color = horizon,
    fill = horizon,
    show.legend = FALSE
  )) +
    tidybayes::stat_halfeye(
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
    scale_y_continuous(trans = "log10", limits = c(0.5, 2)) +
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
#'
#' @return A ggplot object containing plots of the distribution of relative
#' CRPS scores across location and forecast dates
#' @export
make_fig4_rel_crps_overall <- function(scores,
                                       horizons_to_show = c(
                                         "nowcast",
                                         "1 wk", "4 wks",
                                         "overall"
                                       )) {
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
    dplyr::select(location, forecast_date, date, model, horizon, crps) |>
    dplyr::filter(!is.na(horizon)) |>
    tidyr::pivot_wider(
      names_from = model,
      values_from = crps,
      id_cols = c(location, forecast_date, horizon, date)
    ) |>
    dplyr::mutate(
      rel_crps = ww / hosp
    ) |>
    order_horizons()

  colors <- plot_components()


  p <- ggplot(relative_crps) +
    tidybayes::stat_halfeye(
      aes(
        x = horizon, y = rel_crps,
        fill = horizon
      ),
      point_interval = "mean_qi",
      alpha = 0.5,
      position = position_dodge(width = 0.75),
      show.legend = FALSE
    ) +
    geom_hline(aes(yintercept = 1), linetype = "dashed") +
    xlab("Horizon") +
    ylab("Relative CRPS") +
    scale_y_continuous(trans = "log10", limits = c(0.5, 2)) +
    get_plot_theme(
      y_axis_title_size = 8,
      x_axis_title_size = 8
    ) +
    scale_fill_manual(values = colors$horizon_colors) +
    scale_color_manual(values = colors$horizon_colors)

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
#'
#' @return a ggplot object with the overall QQ plot colored by model.
#' @export
make_qq_plot_overall <- function(scores_quantiles) {
  colors <- plot_components()
  p <- scores_quantiles |>
    data.table::as.data.table() |>
    scoringutils::summarise_scores(by = c("model", "quantile")) |>
    scoringutils::plot_quantile_coverage() +
    # ggtitle(glue::glue("QQ plot")) +
    get_plot_theme() +
    labs(
      ylab = "Percent of data below quantile",
      col = "Model"
    ) +
    theme(legend.position = "none") +
    scale_color_manual(values = colors$model_colors)

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
#'
#' @return A ggplot2 object
#'
make_plot_coverage_range <- function(scores_quantiles, ranges) {
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
    facet_wrap(~named_facet, scales = "free_y", ncol = 1) +
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
    dplyr::select(
      location, date, forecast_date, model, horizon, phase, crps
    ) |>
    dplyr::filter(!is.na(horizon)) |>
    tidyr::pivot_wider(
      names_from = model,
      values_from = crps,
      id_cols = c(location, date, forecast_date, horizon, phase)
    ) |>
    dplyr::mutate(
      rel_crps = ww / hosp
    ) |>
    order_phases() |>
    order_horizons() |>
    dplyr::filter(!is.na(phase)) # Exclude NAs in plot

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
#' CRPS scores, colored by model, stratified by epidemic phase, across
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
      date_breaks = "2 weeks",
      labels = scales::date_format("%Y-%m-%d")
    ) +
    scale_color_manual(values = colors$model_colors)

  return(p)
}

#' Make Figure 4
#'
#' @param fig4_rel_crps_overall density plot comparing overall distribution
#' of crps scores across forecast_date, date, location, and model
#' @param fig4_avg_crps avg crps across locations by forecast date
#' @param fig4_natl_admissions national admissions by day
#' @param fig4_rel_crps_over_time relative crps across locations by forecast
#' date
#' @param fig4_rel_crps_by_location avg crps across forecast dates by state
#' @param fig4_qq_plot_overall overall qq plot
#' @param fig4_plot_coverage_range interval coverage plots at 3 intervals
#' @param fig_file_dir Path to save figures
#'
#' @return ggplot object with all the elements combined
#' @export
make_fig4 <- function(fig4_rel_crps_overall,
                      fig4_avg_crps,
                      fig4_natl_admissions,
                      fig4_rel_crps_over_time,
                      fig4_rel_crps_by_location,
                      fig4_qq_plot_overall,
                      fig4_plot_coverage_range,
                      fig_file_dir) {
  layout <- "
AAAA
BBBB
CCCC
DDDD
EEEE
FGGG
FGGG
"

  fig4 <- fig4_rel_crps_overall +
    fig4_natl_admissions +
    fig4_avg_crps +
    fig4_rel_crps_over_time +
    fig4_rel_crps_by_location +
    fig4_plot_coverage_range +
    fig4_qq_plot_overall +
    patchwork::plot_layout(
      design = layout,
      axes = "collect"
    ) & theme(
    legend.position = "top",
    legend.justification = "left"
  ) #+ plot_annotation(tag_levels = "A") #nolint not working
  fig4
  ggsave(fig4,
    filename = file.path(fig_file_dir, "fig4.png"),
    width = 8, height = 11
  )


  return(fig4)
}

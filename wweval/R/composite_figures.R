#' Functions for creating multi-panel
#' composite figures

#' Make a figure showing comparisons between CFA models
#' and other COVIDHub models
#'
#' @param plot_wis_t average wis over time
#' across locations for ach model in real-time (feb-mar)
#' @param hist_rwis Histogram of relative WIS
#' across location, forecast_date, day, and model
#' @param barplot_wis bar chart in order of average WIS
#' @param heatmap_rel_wis heatmap comparing WIS across
#' forecast dates for each location.
#' @param qq_plot qq plot comparing model coverage
#' @return a patchwork object containing all the figures combined.
#' @export
compose_hub_fig <- function(
  plot_wis_t,
  hist_rwis,
  barplot_wis,
  heatmap_rel_wis,
  qq_plot
) {
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
    ) &
    theme(
      legend.position = "bottom"
    )
  return(composed_fig)
}


#' Compose figure showing predictions and observations
#' for three example forecasts.
#'
#' @param hosp1 first hospital admissions predicted-actual figure
#' @param hosp2 second hospital admissions predicted-actual figure
#' @param hosp3 third hospital admissions predicted-actual figure
#' @param ww1 first wastewater predicted-actual figure
#' @param ww2 second wastewater predicted-actual figure
#' @param ww3 third wastewater predicted-actual figure
#' @return a combined ggplot object
#' @export
compose_pred_actual_fig <- function(hosp1, hosp2, hosp3, ww1, ww2, ww3) {
  fig <- patchwork::wrap_plots(
    hosp1,
    ww1,
    hosp2,
    ww2,
    hosp3,
    ww3,
    guides = "collect",
    nrow = 3,
    ncol = 2,
    axes = "collect",
    widths = c(1, 1.5)
  ) &
    theme(
      legend.position = "top",
      legend.justification = "left"
    )

  return(fig)
}


#' Compose figure showing examples of forecasts and their scores.
#'
#' @param score_single_loc1 first states score density plot
#' @param forecast_comparison_nowcast1 first states nowcast comparison
#' @param forecast_comparison_1wk1 first states 1 wk forecast comparison
#' @param forecast_comparison_4wk1 first states 4 wk forecast comparison
#' @param score_underlay_nowcast1 first states score nowcast underlay
#' @param score_underlay_1wk1 first states score 1 wk underlay
#' @param score_underlay_4wk1 first states score 4wk underlay
#' @param score_single_loc2 second states score density plot
#' @param forecast_comparison_nowcast2 second states nowcast comparison
#' @param forecast_comparison_1wk2 second states 1 wk forecast comparison
#' @param forecast_comparison_4wk2 second states 4 wk forecast comparison
#' @param score_underlay_nowcast2 second states score nowcast underlay
#' @param score_underlay_1wk2 second states score 1 wk underlay
#' @param score_underlay_4wk2 second states score 4wk underlay
#' @param score_single_loc3 first state's score density plot
#' @param forecast_comparison_nowcast3 third states nowcast comparison
#' @param forecast_comparison_1wk3 third states 1 wk forecast comparison
#' @param forecast_comparison_4wk3 third states 4 wk forecast comparison
#' @param score_underlay_nowcast3 third states score nowcast underlay
#' @param score_underlay_1wk3 third states score 1 wk underlay
#' @param score_underlay_4wk3 third states score 4wk underlay
#'
#' @return ggplot object that is a combination of 3 states overall score
#' distributions comparing the two model types +
#' forecast comparisons across horizons with an underlay
#' indicating the score.
#' @export
compose_example_scores_fig <- function(
  score_single_loc1,
  forecast_comparison_nowcast1, # nolint
  forecast_comparison_1wk1,
  forecast_comparison_4wk1,
  score_underlay_nowcast1,
  score_underlay_1wk1,
  score_underlay_4wk1,
  score_single_loc2,
  forecast_comparison_nowcast2, # nolint
  forecast_comparison_1wk2,
  forecast_comparison_4wk2,
  score_underlay_nowcast2,
  score_underlay_1wk2,
  score_underlay_4wk2,
  score_single_loc3,
  forecast_comparison_nowcast3, # nolint
  forecast_comparison_1wk3,
  forecast_comparison_4wk3,
  score_underlay_nowcast3,
  score_underlay_1wk3,
  score_underlay_4wk3
) {
  layout <- "
ABCD
AEFG
HIJK
HLMN
OPQR
OSTU
"
  fig <- patchwork::wrap_plots(
    score_single_loc1,
    forecast_comparison_nowcast1,
    forecast_comparison_1wk1,
    forecast_comparison_4wk1,
    score_underlay_nowcast1,
    score_underlay_1wk1,
    score_underlay_4wk1,
    score_single_loc2,
    forecast_comparison_nowcast2,
    forecast_comparison_1wk2,
    forecast_comparison_4wk2,
    score_underlay_nowcast2,
    score_underlay_1wk2,
    score_underlay_4wk2,
    score_single_loc3,
    forecast_comparison_nowcast3,
    forecast_comparison_1wk3,
    forecast_comparison_4wk3,
    score_underlay_nowcast3,
    score_underlay_1wk3,
    score_underlay_4wk3,
    design = layout,
    guides = "collect",
    axes = "collect"
  ) &
    theme(
      legend.position = "top",
      legend.justification = "left"
    )

  return(fig)
}


#' Make a multi-panel figure summarizing relative
#' performance.
#'
#' @param rel_score_heatmap heatmap of relative score by forecast date
#' and location
#' @param rel_score_dist distibution plot comparing overall distribution
#' of relative scores across forecast_date, date, location, and model
#' @param abs_score_by_time timeseries plot of absolute score across
#' by forecast date
#' @param total_admissions timeseries plot of total hospital
#' admissions by day
#' @param rel_score_dist_by_time plot of the distribution of (location
#' -specific) relative score values by forecast_dat.
#' @param rel_score_dist_by_location plot of the distribution of (date-
#' specific) relative score values by location.
#' @param time_period string to save fig as, either "real_time" or
#' "all_time"
#' @return ggplot object with all the elements combined
#' @export
compose_rel_performance_fig <- function(
  rel_score_heatmap,
  rel_score_dist,
  abs_score_by_time,
  total_admissions,
  rel_score_dist_by_time,
  rel_score_dist_by_location,
  time_period
) {
  layout <- "
AACCC
AADDD
BBEEE
BBFFF
"

  fig <- patchwork::wrap_plots(
    rel_score_heatmap,
    rel_score_dist,
    total_admissions,
    abs_score_by_time,
    rel_score_dist_by_time,
    rel_score_dist_by_location,
    design = layout,
    axes = "collect"
  ) &
    theme(
      legend.position = "top",
      legend.justification = "left"
    )

  return(fig)
}

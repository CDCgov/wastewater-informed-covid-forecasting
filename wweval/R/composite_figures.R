#' Get the x and y limits of an individual ggplot,
#' on the raw data scale.
#'
#' Adapted from this stackoverflow implementation
#' https://stackoverflow.com/a/40304848
#'
#' @param plot plot whose limits to extract
#' @return The x and y limits, as a named list with
#' entries xmin, xmax, ymin, and ymax
get_plot_xy_raw_limits <- function(plot) {
  p_built <- ggplot_build(plot)
  x_inv <- p_built$layout$panel_scales_x[[1]]$trans$inverse %||% identity
  y_inv <- p_built$layout$panel_scales_y[[1]]$trans$inverse %||% identity
  xlim <- x_inv(p_built$layout$panel_params[[1]]$x.range)
  ylim <- y_inv(p_built$layout$panel_params[[1]]$y.range)
  return(list(
    xmin = xlim[1],
    xmax = xlim[2],
    ymin = ylim[1],
    ymax = ylim[2]
  ))
}

#' Get shared (spanning) x and y limits for a set of
#' ggplot objects, on the raw data scale.
#'
#' @param list_of_plots List of ggplot objects for which to obtain
#' the spanning limits
#' @return The shared limits, as a named list with entries
#' entries xmin, xmax, ymin, and ymax
get_shared_xy_raw_limits <- function(list_of_plots) {
  limits <- purrr::map_df(list_of_plots, get_plot_xy_raw_limits) |>
    dplyr::summarise(
      xmin = min(.data$xmin),
      xmax = max(.data$xmax),
      ymin = min(.data$ymin),
      ymax = max(.data$ymax)
    ) |>
    as.list()

  return(limits)
}

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
  design <- "
11
AB
AB
CD
CD
CE
CE
CE
"
  ymax_wis <- max(
    get_plot_xy_raw_limits(barplot_wis)$ymax,
    get_plot_xy_raw_limits(plot_wis_t)$ymax
  )

  shared_y_wis <- ggplot2::scale_y_continuous(
    limits = c(0, ymax_wis)
  )

  no_guides <- ggplot2::guides(fill = "none", color = "none", shape = "none")
  no_xlab <- ggplot2::theme(axis.title.x = ggplot2::element_blank())

  fig <- patchwork::wrap_plots(
    patchwork::guide_area(),
    A = barplot_wis + shared_y_wis + no_guides + ggplot2::ylab("WIS"),
    B = plot_wis_t +
      shared_y_wis +
      ggplot2::labs(
        x = "Forecast date",
        y = "WIS"
      ),
    C = heatmap_rel_wis + no_xlab,
    D = hist_rwis + no_guides,
    E = qq_plot + no_guides
  ) +
    patchwork::plot_layout(
      design = design,
      axes = "collect",
      guides = "collect",
      widths = 1,
      heights = 1
    ) +
    patchwork::plot_annotation(tag_levels = "A") &
    theme(
      legend.position = "top",
      legend.justification = "center",
      legend.title.position = "top"
    )

  return(fig)
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
  ) +
    patchwork::plot_annotation(tag_levels = "A") &
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
  ) +
    patchwork::plot_annotation(tag_levels = "A") &
    theme(
      legend.position = "top",
      legend.justification = "center",
      legend.title.position = "top"
    )

  return(fig)
}


#' Make a multi-panel figure summarizing relative
#' performance.
#'
#' @param rel_score_heatmap heatmap of relative score by forecast date
#' and location
#' @param rel_score_dist distibution plot comparing overall distribution
#' of relative scores by forecast_date and location
#' @param rel_score_by_time timeseries plot of relative score
#' by forecast date.
#' @param total_admissions timeseries plot of total hospital
#' admissions by day.
#' @param abs_scores_by_time plot (location-specific) score values by forecast_date.
#' @param abs_scores_by_location plot of (date-specific) score values by location.
#' @return ggplot object with all the elements combined
#' @export
compose_rel_performance_fig <- function(
  rel_score_heatmap,
  rel_score_dist,
  rel_score_by_time,
  total_admissions,
  abs_scores_by_time,
  abs_scores_by_location
) {
  design <- "
11111
AAAEE
AAAEE
BBBEE
BBBEE
CCCFF
CCCFF
DDDFF
DDDFF
"
  shared_lims <- get_shared_xy_raw_limits(list(
    rel_score_by_time,
    total_admissions,
    abs_scores_by_time
  ))

  date_lims <- as.Date(c(shared_lims$xmin, shared_lims$xmax))

  shared_x_dates <- scale_x_weekly_iso_date(
    name = "Forecast date",
    limits = date_lims,
    expand = 0)

  ymax_score <- max(
    get_plot_xy_raw_limits(abs_scores_by_time)$ymax,
    get_plot_xy_raw_limits(abs_scores_by_location)$ymax
  )

  shared_y_score <- ggplot2::scale_y_continuous(
    limits = c(0, ymax_score)
  )
  no_xlab <- ggplot2::theme(axis.title.x = ggplot2::element_blank())

  fig <- patchwork::wrap_plots(
    patchwork::guide_area(),
    A = total_admissions + shared_x_dates + no_xlab,
    B = rel_score_by_time +
      shared_x_dates +
      ggplot2::guides(fill = "none", color = "none") +
      no_xlab,
    C = abs_scores_by_time + shared_x_dates + shared_y_score,
    D = abs_scores_by_location + shared_y_score,
    E = rel_score_dist,
    F = rel_score_heatmap + scale_x_weekly_iso_date(
                                name = "Forecast date",
                                expand = 0),
    design = design,
    axes = "collect_x",
    guides = "collect"
  ) +
    patchwork::plot_annotation(tag_levels = "A") &
    theme(
      legend.position = "top",
      legend.justification = "center",
      legend.title.position = "top"
    )

  return(fig)
}

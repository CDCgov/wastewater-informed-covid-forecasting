#' Generate a bar plots of CRPS for each model in different locations
#'
#' @param scores tibble of crps scores by location, forecast date, model,
#' horizon day
#' @param locs_to_plot the locations we want summaries for
#' @param fig_file_dir string indicating directory to save fig in
#' @return Figure showing CRPS for multiple locations.
#' @export
multi_location_crps_figure <- function(scores,
                                       locs_to_plot,
                                       fig_file_dir) {
  scores_locs_long <- scores |>
    dplyr::filter(.data$location %in% !!locs_to_plot) |>
    scoringutils::summarise_scores(
      by = c("model", "location")
    )

  colors <- plot_components()
  p <- ggplot(scores_locs_long) +
    geom_bar(
      aes(
        x = .data$model,
        y = .data$crps,
        fill = .data$model
      ),
      stat = "identity",
      position = "dodge"
    ) +
    scale_fill_manual(values = colors$model_colors) +
    facet_wrap(~ .data$location) +
    get_plot_theme(
      x_axis_dates = TRUE,
      y_axis_title_size = 8,
      y_axis_text_size = 6
    ) +
    xlab("") +
    ylab("Mean CRPS")

  ggsave(p,
    filename = file.path(
      fig_file_dir,
      "plot_multi_location_crps.png"
    ),
    width = 7, height = 4
  )

  return(p)
}

#' Get an individual forecast score summary for a particular
#' forecast date and location
#'
#' @param scores A tibble of all forecast date- location- model
#' forecast perfomance scores
#' @param loc the location of interest
#' @param this_forecast_date the forecast date of interest
#'
#' @return A tibble of mean scores by models
#' @export
get_ind_forecast_score <- function(scores,
                                   loc,
                                   this_forecast_date) {
  ind_score <- scores |>
    dplyr::filter(
      .data$location == !!loc,
      .data$forecast_date == !!this_forecast_date
    ) |>
    scoringutils::summarize_scores(
      by = "model"
    )

  return(ind_score)
}


#' Make head to head CRPS distribution comparison plot for a single location
#'
#' @param scores A tibble of scores by location, forecast date, date and model,
#' containing the outputs of `scoringutils::score()` on samples plus metadata
#' transformed into a tibble.
#' @param loc_to_plot A  string indicating the state abbreviations of the state
#' to plot
#' @param baseline Name of the model to use a sa baseline. Default
#' `"cfa-hosponlyrenewal(retro)"`
#' @param horizons_to_show A vector of strings indicating the names of the
#' `horizon` that we want to show on the plot, must be a subset of
#' `nowcast`, `1 wk`, `2 wks`,`3 wks`, `4 wks` and `overall`
#'
#' @return a ggplot object containing violin plots comparing the distribution
#' of crps scores across forecast dates for a single location, grouped by
#' horizon and colored by model
#' @export
make_fig3_single_loc_comp <- function(scores,
                                      loc_to_plot,
                                      baseline =
                                        "cfa-hosponlyrenewal(retro)",
                                      horizons_to_show = c(
                                        "nowcast",
                                        "1 wk", "4 wks",
                                        "overall"
                                      )) {
  scores_by_horizon <- scores |>
    dplyr::filter(
      .data$location == !!loc_to_plot,
      .data$horizon %in% !!horizons_to_show
    )
  scores_overall <- scores |>
    dplyr::filter(.data$location == !!loc_to_plot) |>
    dplyr::mutate(horizon = "overall")

  scores_comb <- dplyr::bind_rows(scores_by_horizon, scores_overall) |>
    dplyr::filter(horizon %in% !!horizons_to_show)

  relative_crps <- scores_comb |>
    forecasttools::summarise_scores_with_baseline(
      baseline = baseline,
      by = c("horizon", "forecast_date", "location")
    ) |>
    dplyr::rename(rel_crps = "mean_scores_ratio") |>
    order_horizons()

  colors <- plot_components()

  p <- ggplot(relative_crps) +
    tidybayes::stat_dotsinterval(
      aes(
        x = .data$horizon,
        y = .data$rel_crps,
        fill = .data$horizon
      ),
      point_interval = "mean_qi",
      alpha = 0.5,
      position = position_dodge(width = 0.75),
      show.legend = FALSE
    ) +
    xlab("") +
    ylab("Relative CRPS") +
    theme_bw() +
    scale_color_manual(values = colors$horizon_colors) +
    scale_fill_manual(values = colors$horizon_colors) +
    geom_hline(aes(yintercept = 1), linetype = "dashed") +
    get_plot_theme(
      y_axis_title_size = 8,
      x_axis_text_size = 6
    ) +
    scale_y_continuous(trans = "log10") + # , limits = c(0.25, 4.0)) +
    labs(color = "Model") +
    coord_cartesian(ylim = c(1 / 6, 6))


  return(p)
}

#' Make hospital forecast comparison figure
#'
#' @param hosp_quantiles A tibble containing the calibrated hospital admissions
#' data, the evaluation hospital admissions data, and the quantiles of the
#' calibrated and forecasted admissions
#' @param loc_to_plot A  string indicating the state abbreviations of the state
#' to plot
#' @param horizon_to_plot A string indicating what horizon period to plot,
#' one of `nowcast`, `1 wk`, or `4 wks`
#' @param horizon_days_ahead An integer corresponding to the horizon days to
#' plot
#' @param days_to_show_prev_data An ingeger indicating how many days before the
#' last forecast date to show the data, default is `14`
#'
#' @return A ggplot object containing a plot of the retrospective hospital
#' admissions data compared to the nowcasted/forecasted quantiles and median
#' for the specified horizon to plot, colored by the model type
#' @export
make_fig3_forecast_comp_fig <- function(hosp_quantiles,
                                        loc_to_plot,
                                        horizon_to_plot,
                                        horizon_days_ahead,
                                        days_to_show_prev_data = 14) {
  hosp_quants_horizons <- hosp_quantiles |>
    dplyr::filter(location == !!loc_to_plot) |>
    dplyr::filter(date >=
      min(forecast_date) - lubridate::days(
        !!days_to_show_prev_data
      ))

  hosp <- hosp_quants_horizons |>
    dplyr::filter(horizon == !!horizon_to_plot) |>
    dplyr::filter(quantile %in% c(0.025, 0.25, 0.5, 0.75, 0.975)) |>
    tidyr::pivot_wider(
      id_cols = c(
        forecast_date, model_type,
        date, eval_data
      ),
      names_from = quantile,
      values_from = value
    )
  colors <- plot_components()

  max_obs <- max(hosp_quants_horizons$eval_data)

  date_lims <- c(
    min(hosp_quantiles$forecast_date) + lubridate::days(horizon_days_ahead - 9),
    max(hosp_quantiles$forecast_date) + lubridate::days(horizon_days_ahead + 5)
  )
  p <- ggplot(hosp) +
    geom_point(
      data = hosp_quants_horizons,
      aes(x = date, y = eval_data),
      fill = "black", size = 0.3, shape = 21,
      show.legend = FALSE
    ) +
    geom_ribbon(
      data = hosp,
      aes(
        x = date, ymin = `0.025`, ymax = `0.975`,
        group = interaction(forecast_date, model_type),
        fill = model_type
      ), alpha = 0.1,
      show.legend = FALSE
    ) +
    geom_ribbon(
      data = hosp,
      aes(
        x = date, ymin = `0.25`, ymax = `0.75`,
        group = interaction(forecast_date, model_type),
        fill = model_type
      ), alpha = 0.1,
      show.legend = FALSE
    ) +
    geom_line(
      data = hosp,
      aes(
        x = date, y = `0.5`,
        group = interaction(forecast_date, model_type),
        color = model_type,
        show.legend = FALSE
      ),
    ) +
    xlab("") +
    ylab("Daily hospital \n admissions") +
    scale_color_manual(values = colors$model_colors) +
    scale_fill_manual(values = colors$model_colors) +
    scale_x_date(
      date_breaks = "2 weeks",
      labels = scales::date_format("%Y-%m-%d"),
      limits = date_lims
    ) +
    get_plot_theme(
      x_axis_dates = TRUE,
      y_axis_title_size = 6,
      y_axis_text_size = 6
    ) +
    guides(fill = "none", color = "none") +
    ylim(0, 2 * max_obs)

  return(p)
}

#' Make CRPS underlay figure
#'
#' @param scores A tibble of scores by location, forecast date, date and model,
#' containing the outputs of `scoringutils::score()` on samples plus metadata
#' transformed into a tibble.
#' @param loc_to_plot A  string indicating the state abbreviations of the state
#' to plot
#' @param horizon_to_plot A string indicating what horizon period to plot,
#' one of `nowcast`, `1 wk`, or `4 wks`
#' @param horizon_days_ahead An integer corresponding to the horizon days to
#' plot
#' @param days_to_shift An integer corresponding to the number of days to shift
#' the x axis of the underly plot to line up with the corresponding forecast
#' horizon, default is 0
#'
#' @return A ggplot object containing a bar chart of the crps score averaged
#' across the horizon for each forecast date, colored by the model type
#' @export
make_fig3_crps_underlay_fig <- function(scores,
                                        loc_to_plot,
                                        horizon_to_plot,
                                        horizon_days_ahead,
                                        days_to_shift = 0) {
  scores_filtered <- scores |>
    dplyr::filter(location == !!loc_to_plot) |>
    data.table::as.data.table() |>
    scoringutils::summarise_scores(by = c(
      "forecast_date", "location",
      "model", "horizon"
    ))
  max_crps <- max(scores_filtered$crps)

  scores_by_horizon <- scores_filtered |>
    dplyr::filter(horizon == !!horizon_to_plot) |>
    dplyr::mutate(
      forecast_date_shifted = lubridate::ymd(forecast_date) +
        lubridate::days(days_to_shift)
    )

  colors <- plot_components()
  date_lims <- c(
    min(scores$forecast_date) + lubridate::days(horizon_days_ahead - 9),
    max(scores$forecast_date) +
      lubridate::days(horizon_days_ahead + 5)
  )

  p <- ggplot(scores_by_horizon) +
    geom_bar(aes(x = forecast_date_shifted, y = crps, fill = model),
      stat = "identity", position = "dodge", show.legend = FALSE
    ) +
    xlab("") +
    ylab("CRPS") +
    theme_bw() +
    scale_fill_manual(values = colors$model_colors) +
    scale_x_date(
      date_breaks = "2 weeks",
      labels = scales::date_format("%Y-%m-%d"),
      limits = date_lims
    ) +
    coord_cartesian(ylim = c(0, 1)) +
    get_plot_theme(
      x_axis_dates = TRUE,
      y_axis_title_size = 8,
      y_axis_text_size = 6
    )


  return(p)
}

#' Title
#'
#' @param fig3_crps_single_loc1 first states crps density plot
#' @param fig3_forecast_comparison_nowcast1 first states nowcast comparison
#' @param fig3_forecast_comparison_1wk1 first states 1 wk forecast comparison
#' @param fig3_forecast_comparison_4wks1 first states 4 wk forecast comparison
#' @param fig3_crps_underlay_nowcast1 first states crps nowcast underlay
#' @param fig3_crps_underlay_1wk1 first states crps 1 wk underlay
#' @param fig3_crps_underlay_4wks1 first states crps 4wk underlay
#' @param fig3_crps_single_loc2 second states crps density plot
#' @param fig3_forecast_comparison_nowcast2 second states nowcast comparison
#' @param fig3_forecast_comparison_1wk2 second states 1 wk forecast comparison
#' @param fig3_forecast_comparison_4wks2 second states 4 wk forecast comparison
#' @param fig3_crps_underlay_nowcast2 second states crps nowcast underlay
#' @param fig3_crps_underlay_1wk2 second states crps 1 wk underlay
#' @param fig3_crps_underlay_4wks2 second states crps 4wk underlay
#' @param fig3_crps_single_loc3 first state's crps density plot
#' @param fig3_forecast_comparison_nowcast3 third states nowcast comparison
#' @param fig3_forecast_comparison_1wk3 third states 1 wk forecast comparison
#' @param fig3_forecast_comparison_4wks3 third states 4 wk forecast comparison
#' @param fig3_crps_underlay_nowcast3 third states crps nowcast underlay
#' @param fig3_crps_underlay_1wk3 third states crps 1 wk underlay
#' @param fig3_crps_underlay_4wks3 third states crps 4wk underlay
#' @param fig_file_dir Path to save figures
#'
#' @return ggplot object that is a combination of 3 states overall crps
#' distributions comparing the two model types +
#' forecast comparisons across horizons with an underlay indicating the crps
#' score
#' @export
make_fig3 <- function(fig3_crps_single_loc1,
                      fig3_forecast_comparison_nowcast1, # nolint
                      fig3_forecast_comparison_1wk1,
                      fig3_forecast_comparison_4wks1,
                      fig3_crps_underlay_nowcast1,
                      fig3_crps_underlay_1wk1,
                      fig3_crps_underlay_4wks1,
                      fig3_crps_single_loc2,
                      fig3_forecast_comparison_nowcast2, # nolint
                      fig3_forecast_comparison_1wk2,
                      fig3_forecast_comparison_4wks2,
                      fig3_crps_underlay_nowcast2,
                      fig3_crps_underlay_1wk2,
                      fig3_crps_underlay_4wks2,
                      fig3_crps_single_loc3,
                      fig3_forecast_comparison_nowcast3, # nolint
                      fig3_forecast_comparison_1wk3,
                      fig3_forecast_comparison_4wks3,
                      fig3_crps_underlay_nowcast3,
                      fig3_crps_underlay_1wk3,
                      fig3_crps_underlay_4wks3,
                      fig_file_dir) {
  layout <- "
ABCD
AEFG
HIJK
HLMN
OPQR
OSTU
"
  fig3 <- fig3_crps_single_loc1 + fig3_forecast_comparison_nowcast1 +
    fig3_forecast_comparison_1wk1 +
    fig3_forecast_comparison_4wks1 + fig3_crps_underlay_nowcast1 +
    fig3_crps_underlay_1wk1 + fig3_crps_underlay_4wks1 +
    fig3_crps_single_loc2 + fig3_forecast_comparison_nowcast2 +
    fig3_forecast_comparison_1wk2 +
    fig3_forecast_comparison_4wks2 + fig3_crps_underlay_nowcast2 +
    fig3_crps_underlay_1wk2 + fig3_crps_underlay_4wks2 +
    fig3_crps_single_loc3 + fig3_forecast_comparison_nowcast3 +
    fig3_forecast_comparison_1wk3 +
    fig3_forecast_comparison_4wks3 + fig3_crps_underlay_nowcast3 +
    fig3_crps_underlay_1wk3 + fig3_crps_underlay_4wks3 +
    patchwork::plot_layout(
      design = layout,
      guides = "collect",
      axes = "collect"
    ) & theme(
    legend.position = "top",
    legend.justification = "left"
  ) #+ plot_annotation(tag_levels = "A") #nolint , not working

  fig3
  fs::dir_create(fig_file_dir)
  ggsave(fig3,
    filename = file.path(fig_file_dir, "fig3.png"),
    width = 10, height = 7
  )
  ggsave(fig3,
    filename = file.path(fig_file_dir, "fig3.svg"),
    width = 10, height = 7
  )

  return(fig3)
}

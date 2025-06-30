#' Generate bar plots of CRPS for each model in different locations
#'
#' @param scores tibble of crps scores by location, forecast date, model,
#' horizon day
#' @param locs_to_plot Vector of strings indicating the locations to plot,
#' as two-letter USPS abbreviations.
#' @param fig_file_dir string indicating directory to save fig in
#' @return Figure showing CRPS for multiple locations.
#' @export
multi_location_crps_figure <- function(scores, locs_to_plot, fig_file_dir) {
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

  ggsave(
    p,
    filename = file.path(
      fig_file_dir,
      "plot_multi_location_crps.png"
    ),
    width = 7,
    height = 4
  )

  return(p)
}

#' Get an individual forecast score summary for a particular
#' forecast date and location
#'
#' @param scores A tibble of all forecast date- location- model
#' forecast perfomance scores
#' @param loc A string indicating the location to plot, as a
#' two-letter USPS abbreviation.
#' @param this_forecast_date the forecast date of interest
#'
#' @return A tibble of mean scores by models
#' @export
get_ind_forecast_score <- function(scores, loc, this_forecast_date) {
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


#' Make head to head CRPS distribution comparison
#' plot for a single location
#'
#' @param scores A tibble of scores by location, forecast date,
#' date and model,
#' containing the outputs of `scoringutils::score()` on samples
#' plus metadata
#' transformed into a tibble.
#' @param loc_to_plot A string indicating the location to plot, as a
#' two-letter USPS abbreviation.
#' @param baseline Name of the model to use as a baseline. Default
#' `"cfa-hosponlyrenewal(retro)"`
#' @param horizons_to_show A vector of strings indicating the names of the
#' `horizon` that we want to show on the plot, must be a subset of
#' `nowcast`, `1 wk`, `2 wks`,`3 wks`, `4 wks` and `overall`
#'
#' @return a ggplot object containing violin plots comparing the
#' distribution of crps across forecast dates for a single location,
#' grouped by horizon and colored by model
#' @export
plot_rel_crps_by_horizon <- function(
  scores,
  loc_to_plot,
  baseline = "cfa-hosponlyrenewal(retro)",
  horizons_to_show = c(
    "nowcast",
    "1 wk",
    "4 wks",
    "overall"
  )
) {
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


#' Compare two hospital admissions forecasts over time.
#'
#' @param hosp_quantiles A tibble containing the calibrated hospital
#' admissions data, the evaluation hospital admissions data, and
#' the quantiles of the calibrated and forecasted admissions
#' @param loc_to_plot A string indicating the location to plot, as a
#' two-letter USPS abbreviation.
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
plot_forecast_comparison_t <- function(
  hosp_quantiles,
  loc_to_plot,
  horizon_to_plot,
  horizon_days_ahead,
  days_to_show_prev_data = 14
) {
  checkmate::assert_names(
    horizon_to_plot,
    subset.of = c("nowcast", "1 wk", "4 wks")
  )
  needed_quantiles <- c(0.025, 0.25, 0.5, 0.75, 0.975)
  hosp_quants_horizons <- hosp_quantiles |>
    dplyr::filter(location == !!loc_to_plot) |>
    dplyr::filter(
      date >=
        min(forecast_date) -
          lubridate::days(
            !!days_to_show_prev_data
          )
    )

  hosp <- hosp_quants_horizons |>
    dplyr::filter(
      horizon == !!horizon_to_plot,
      .data$quantile_level %in% !!needed_quantiles
    ) |>
    dplyr::select(
      "forecast_date",
      "date",
      "model_type",
      "quantile_level",
      "predicted"
    ) |>
    tidyr::pivot_wider(
      names_from = "quantile_level",
      values_from = "predicted"
    )
  colors <- plot_components()

  max_obs <- max(hosp_quants_horizons$observed)

  date_lims <- c(
    min(hosp_quantiles$forecast_date) + lubridate::days(horizon_days_ahead - 9),
    max(hosp_quantiles$forecast_date) + lubridate::days(horizon_days_ahead + 5)
  )
  p <- ggplot(hosp) +
    geom_point(
      data = hosp_quants_horizons,
      aes(
        x = .data$date,
        y = .data$observed
      ),
      fill = "black",
      size = 0.3,
      shape = 21,
      show.legend = FALSE
    ) +
    geom_ribbon(
      aes(
        x = .data$date,
        ymin = .data$`0.025`,
        ymax = .data$`0.975`,
        group = interaction(
          .data$forecast_date,
          .data$model_type
        ),
        fill = .data$model_type
      ),
      alpha = 0.1,
      show.legend = FALSE
    ) +
    geom_ribbon(
      aes(
        x = .data$date,
        ymin = .data$`0.25`,
        ymax = .data$`0.75`,
        group = interaction(.data$forecast_date, .data$model_type),
        fill = .data$model_type
      ),
      alpha = 0.1,
      show.legend = FALSE
    ) +
    geom_line(
      aes(
        x = .data$date,
        y = .data$`0.5`,
        group = interaction(.data$forecast_date, .data$model_type),
        color = .data$model_type,
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
plot_crps_underlay <- function(
  scores,
  loc_to_plot,
  horizon_to_plot,
  horizon_days_ahead,
  days_to_shift = 0
) {
  scores_filtered <- scores |>
    dplyr::filter(.data$location == !!loc_to_plot) |>

    scoringutils::summarise_scores(
      by = c(
        "forecast_date",
        "location",
        "model",
        "horizon"
      )
    )

  scores_by_horizon <- scores_filtered |>
    dplyr::filter(horizon == !!horizon_to_plot) |>
    dplyr::mutate(
      forecast_date_shifted = lubridate::ymd(forecast_date) +
        lubridate::days(days_to_shift)
    )

  date_lims <- c(
    min(scores$forecast_date) + lubridate::days(horizon_days_ahead - 9),
    max(scores$forecast_date) +
      lubridate::days(horizon_days_ahead + 5)
  )

  p <- plot_score_decomposed_bars(
    scores_by_horizon,
    x = "forecast_date_shifted",
    position = position_dodge2(padding = 0),
    width = 5
  )

  return(p)
}

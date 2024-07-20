#' Make a CRPS density plot for a subset of locations
#'
#' @param scores A tibble of scores by location, forecast date, date and model,
#' the ouput of `scoringutils::score()` on samples.
#' @param horizons_to_show A vector of strings indicating the names of the
#' `horizon` that we want to show on the plot, must be a subset of
#' `nowcast`, `1 wk`, `2 wks`,`3 wks`, `4 wks` and `overall`
#'
#' @return a ggplot object that is a vertical facet of violin plots colored
#' by model type and broken down my horizon
#' @export
make_fig4_crps_density <- function(scores,
                                   horizons_to_show = c(
                                     "nowcast",
                                     "1 wk", "4 wks",
                                     "overall"
                                   )) {
  scores_by_horizon <- scores |>
    data.table::as.data.table() |>
    scoringutils::summarise_scores(by = c(
      "forecast_date", "location",
      "model", "horizon"
    ))

  scores_overall <- scores |>
    data.table::as.data.table() |>
    scoringutils::summarise_scores(by = c(
      "forecast_date", "location",
      "model"
    )) |>
    dplyr::mutate(horizon = "overall")

  scores_comb <- dplyr::bind_rows(scores_by_horizon, scores_overall) |>
    dplyr::filter(
      horizon %in% !!horizons_to_show
    ) |>
    dplyr::mutate(
      fig_order = dplyr::case_when(
        horizon == "nowcast" ~ 1,
        horizon == "1 wk" ~ 2,
        horizon == "2 wks" ~ 3,
        horizon == "3 wks" ~ 4,
        horizon == "4 wks" ~ 5,
        horizon == "overall" ~ 6
      )
    )

  relative_crps <- scores_comb |>
    dplyr::select(
      location, forecast_date, model, horizon,
      fig_order, crps
    ) |>
    dplyr::filter(!is.na(horizon)) |>
    tidyr::pivot_wider(
      names_from = model,
      values_from = crps,
      id_cols = c(location, forecast_date, horizon, fig_order)
    ) |>
    dplyr::mutate(
      rel_crps = ww / hosp
    ) |>
    dplyr::mutate(
      horizon = forcats::fct_reorder(horizon, fig_order)
    )

  p <- ggplot(
    relative_crps,
    aes(
      x = as.factor(forecast_date), y = rel_crps, color = horizon,
      fill = horizon
    )
  ) +
    geom_violin(alpha = 0.3) +
    geom_hline(aes(yintercept = 1), linetype = "dashed") +
    theme_bw() +
    theme(
      axis.text.x = element_text(
        size = 8, vjust = 1,
        hjust = 1, angle = 45
      ),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 10),
      plot.title = element_text(
        size = 10,
        vjust = 0.5, hjust = 0.5
      )
    ) +
    ggtitle("CRPS across forecast dates") +
    xlab("") +
    ylab("Relative CRPS, lower is better") +
    scale_y_continuous(trans = "log")

  return(p)
}

#' Make a figure of the percent of locations with a better forecast with ww
#' over time
#'
#' @param scores A tibble of scores by location, forecast date, date and model,
#' the ouput of `scoringutils::score()` on samples.
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
    tibble::as_tibble() |>
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
        days_to_show_prev_data
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
    theme_bw() +
    theme(
      axis.text.x = element_text(
        size = 8, vjust = 1,
        hjust = 1, angle = 45
      ),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 10),
      plot.title = element_text(
        size = 10,
        vjust = 0.5, hjust = 0.5
      )
    ) +
    xlab("") +
    scale_y_continuous(
      "Percent better with wastewater",
      sec.axis = sec_axis(~ . * max_total_hosp / 100, name = "National admissions")
    )

  return(p)
}

#' Make figure that stratifies scores by location across forecast dates
#'
#' @param scores A tibble of scores by location, forecast date, date and model,
#' the ouput of `scoringutils::score()` on samples.
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
  scores_by_horizon <- scores |>
    data.table::as.data.table() |>
    scoringutils::summarise_scores(by = c(
      "forecast_date", "location",
      "model", "horizon"
    ))

  scores_overall <- scores |>
    data.table::as.data.table() |>
    scoringutils::summarise_scores(by = c(
      "forecast_date", "location",
      "model"
    )) |>
    dplyr::mutate(horizon = "overall")

  scores_comb <- dplyr::bind_rows(scores_by_horizon, scores_overall) |>
    dplyr::filter(
      horizon %in% horizons_to_show
    ) |>
    dplyr::mutate(
      fig_order = dplyr::case_when(
        horizon == "nowcast" ~ 1,
        horizon == "1 wk" ~ 2,
        horizon == "2 wks" ~ 3,
        horizon == "3 wks" ~ 4,
        horizon == "4 wks" ~ 5,
        horizon == "overall" ~ 6
      )
    )

  relative_crps <- scores_comb |>
    dplyr::select(location, forecast_date, model, horizon, fig_order, crps) |>
    dplyr::filter(!is.na(horizon)) |>
    tidyr::pivot_wider(
      names_from = model,
      values_from = crps,
      id_cols = c(location, forecast_date, horizon, fig_order)
    ) |>
    dplyr::mutate(
      rel_crps = ww / hosp
    ) |>
    dplyr::mutate(
      horizon = forcats::fct_reorder(horizon, fig_order)
    )


  p <- ggplot(relative_crps, aes(
    x = rel_crps, y = location, color = horizon,
    fill = horizon
  )) +
    geom_violin(alpha = 0.3) +
    geom_vline(aes(xintercept = 1), linetype = "dashed") +
    theme_bw() +
    theme(
      axis.text.x = element_text(
        size = 8, vjust = 1,
        hjust = 1, angle = 45
      ),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 10),
      plot.title = element_text(
        size = 10,
        vjust = 0.5, hjust = 0.5
      )
    ) +
    ggtitle("Distribution of CRPS by location, across forecast dates") +
    ylab("") +
    xlab("Relative CRPS, lower is better") +
    scale_x_continuous(trans = "log")

  return(p)
}

#' Make figure that stratifies across location and forecast dates
#'
#' @param scores A tibble of scores by location, forecast date, date and model,
#' the ouput of `scoringutils::score()` on samples.
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
  scores_by_horizon <- scores |>
    data.table::as.data.table() |>
    scoringutils::summarise_scores(by = c(
      "forecast_date", "location",
      "model", "horizon"
    ))

  scores_overall <- scores |>
    data.table::as.data.table() |>
    scoringutils::summarise_scores(by = c(
      "forecast_date", "location",
      "model"
    )) |>
    dplyr::mutate(horizon = "overall")

  scores_comb <- dplyr::bind_rows(scores_by_horizon, scores_overall) |>
    dplyr::filter(
      horizon %in% horizons_to_show
    ) |>
    dplyr::mutate(
      fig_order = dplyr::case_when(
        horizon == "nowcast" ~ 1,
        horizon == "1 wk" ~ 2,
        horizon == "2 wks" ~ 3,
        horizon == "3 wks" ~ 4,
        horizon == "4 wks" ~ 5,
        horizon == "overall" ~ 6
      )
    )

  relative_crps <- scores_comb |>
    dplyr::select(location, forecast_date, model, horizon, fig_order, crps) |>
    dplyr::filter(!is.na(horizon)) |>
    tidyr::pivot_wider(
      names_from = model,
      values_from = crps,
      id_cols = c(location, forecast_date, fig_order, horizon)
    ) |>
    dplyr::mutate(
      rel_crps = ww / hosp
    ) |>
    dplyr::mutate(
      horizon = forcats::fct_reorder(horizon, fig_order)
    )


  p <- ggplot(relative_crps, aes(
    x = rel_crps, y = horizon, color = horizon,
    fill = horizon
  )) +
    geom_violin(alpha = 0.3) +
    geom_vline(aes(xintercept = 1), linetype = "dashed") +
    theme_bw() +
    theme(
      axis.text.x = element_text(
        size = 8, vjust = 1,
        hjust = 1, angle = 45
      ),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 10),
      plot.title = element_text(
        size = 10,
        vjust = 0.5, hjust = 0.5
      )
    ) +
    ggtitle("Distribution of CRPS across location and forecast dates") +
    ylab("") +
    xlab("Relative CRPS, lower is better") +
    scale_x_continuous(trans = "log")

  return(p)
}

#' Make a qq plot
#' @description
#' Using [scoringutils::plot_interval_coverage()]
#'
#'
#' @param scores_quantiles A tibble of scores by location, forecast date,
#'  date and model, the ouput of `scoringutils::score()` on the quantiles.
#'
#' @return a ggplot object with the overall QQ plot colored by model.
#' @export
make_qq_plot_overall <- function(scores_quantiles) {
  p <- scores_quantiles |>
    data.table::as.data.table() |>
    scoringutils::summarise_scores(by = c("model", "quantile")) |>
    scoringutils::plot_quantile_coverage() +
    ggtitle(glue::glue("QQ plot"))

  return(p)
}

#' Plot coverage at specified ranges
#'
#' @param scores_quantiles A tibble of scores by location, forecast date,
#' date and model, the ouput of `scoringutils::score()` on the quantiles.
#'
#' @param ranges A numeric vector of credible interval ranges to plot.
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
    dplyr::filter(quantile %in% c(ranges / 100)) |>
    dplyr::group_by(horizon, model, quantile) |>
    dplyr::summarise(pct_coverage = 100 * mean(coverage))


  p <- ggplot(coverage_summarized) +
    aes(
      x = horizon, y = pct_coverage, color = model,
      group = model
    ) +
    geom_line() +
    geom_point() +
    geom_hline(aes(yintercept = 100 * quantile), linetype = "dashed") +
    # scale_y_continuous(labels = percent) +
    facet_wrap(~quantile, scales = "free_y") +
    # theme_scoringutils() +
    labs(
      y = "Proportion of data within forecast interval",
      x = "Forecast horizon (weeks)",
      col = "Model"
    ) +
    theme_bw()
  return(p)
}

#' Make figure that stratifies scores by epidemic phase across
#'
#' @param scores A tibble of scores by location, forecast date, date and model,
#' the ouput of `scoringutils::score()` on samples.
#' @param horizons_to_show A vector of strings indicating the names of the
#' `horizon` that we want to show on the plot, must be a subset of
#' `nowcast`, `1 wk`, `2 wks`,`3 wks`, `4 wks` and `overall`
#'
#' @return A ggplot object containing plots of the distribution of relative
#' CRPS scores, colored by horizon, stratified by epidemic phase, across
#' locations and forecast dates
#' @export
make_fig4_rel_crps_by_phase <- function(scores,
                                        horizons_to_show = c(
                                          "nowcast",
                                          "1 wk", "4 wks",
                                          "overall"
                                        )) {
  scores_w_fig_order <- scores |>
    dplyr::mutate(
      fig_order = dplyr::case_when(
        horizon == "nowcast" ~ 1,
        horizon == "1 wk" ~ 2,
        horizon == "2 wks" ~ 3,
        horizon == "3 wks" ~ 4,
        horizon == "4 wks" ~ 5,
        horizon == "overall" ~ 6
      )
    )

  # Quick warning if there are NAs in epidemic phases
  missing_phases <- scores |>
    dplyr::filter(is.na(phase))

  if (nrow(missing_phases) > 0) {
    warning("There are dates missing epidemic phases")
  }


  relative_crps <- scores_w_fig_order |>
    dplyr::select(
      location, date, forecast_date, model, horizon,
      fig_order, phase, crps
    ) |>
    dplyr::filter(!is.na(horizon)) |>
    tidyr::pivot_wider(
      names_from = model,
      values_from = crps,
      id_cols = c(location, date, forecast_date, fig_order, horizon, phase)
    ) |>
    dplyr::mutate(
      rel_crps = ww / hosp
    ) |>
    dplyr::mutate(
      horizon = forcats::fct_reorder(horizon, fig_order)
    ) |>
    dplyr::filter(
      horizon %in% horizons_to_show,
      !is.na(phase)
    ) # Exclude NAs in plot

  p <- ggplot(
    relative_crps,
    aes(
      x = as.factor(phase), y = rel_crps, color = horizon,
      fill = horizon
    )
  ) +
    geom_violin(alpha = 0.3) +
    geom_hline(aes(yintercept = 1), linetype = "dashed") +
    theme_bw() +
    theme(
      axis.text.x = element_text(
        size = 8, vjust = 1,
        hjust = 1, angle = 45
      ),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 10),
      plot.title = element_text(
        size = 10,
        vjust = 0.5, hjust = 0.5
      )
    ) +
    ggtitle("CRPS across forecast dates") +
    xlab("Epidemic phase") +
    ylab("Relative CRPS, lower is better") +
    scale_y_continuous(trans = "log")

  return(p)
}

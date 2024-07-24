#' Make head to head CRPS distribution comparison plot for a single location
#'
#' @param scores A tibble of scores by location, forecast date, date and model,
#' containing the outputs of `scoringutils::score()` on samples plus metadata
#' transformed into a tibble.
#' @param loc_to_plot A  string indicating the state abbreviations of the state
#' to plot
#' @param horizons_to_show A vector of strings indicating the names of the
#' `horizon` that we want to show on the plot, must be a subset of
#' `nowcast`, `1 wk`, `2 wks`,`3 wks`, `4 wks` and `overall`
#'
#' @return a ggplot object containing violin plots comparing the distribution
#' of crps scores across forecast dates for a single location, grouped by
#' horizon and colored by model
make_fig3_single_loc_comp <- function(scores,
                                      loc_to_plot,
                                      horizons_to_show = c(
                                        "nowcast",
                                        "1 wk", "4 wks",
                                        "overall"
                                      )) {
  scores_by_horizon <- scores |>
    dplyr::filter(location == !!loc_to_plot) |>
    data.table::as.data.table() |>
    scoringutils::summarise_scores(by = c(
      "forecast_date", "location",
      "model", "horizon"
    )) |>
    dplyr::filter(horizon %in% !!horizons_to_show)
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
    order_horizons()

  p <- ggplot(scores_comb) +
    tidybayes::stat_halfeye(
      aes(
        x = horizon, y = crps,
        fill = model
      ),
      point_interval = "mean_qi",
      alpha = 0.5,
      position = position_dodge(width = 0.75)
    ) +
    xlab("") +
    ylab("CRPS") +
    ggtitle(glue::glue(
      "{loc_to_plot}"
    )) +
    theme_bw() +
    scale_color_discrete() +
    scale_fill_discrete() +
    get_plot_theme(y_axis_title_size = 8) +
    scale_y_continuous(trans = "log10") +
    labs(color = "Model", fill = "Model")

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
        date, t, eval_data
      ),
      names_from = quantile,
      values_from = value
    )

  p <- ggplot(hosp) +
    geom_point(
      data = hosp_quants_horizons,
      aes(x = date, y = eval_data),
      fill = "black", size = 1, shape = 21,
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
      ),
    ) +
    xlab("") +
    ylab("Daily hospital admissions") +
    ggtitle(glue::glue(
      "{horizon_to_plot}"
    )) +
    scale_color_discrete() +
    scale_fill_discrete() +
    scale_x_date(
      date_breaks = "2 weeks",
      labels = scales::date_format("%Y-%m-%d"),
      limits = as.Date(c("2023-10-02", "2024-03-18"))
    ) +
    get_plot_theme(
      x_axis_dates = TRUE,
      y_axis_title_size = 8
    ) +
    labs(color = "Model", fill = "Model")

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
                                        days_to_shift = 0) {
  scores_by_horizon <- scores |>
    dplyr::filter(location == !!loc_to_plot) |>
    data.table::as.data.table() |>
    scoringutils::summarise_scores(by = c(
      "forecast_date", "location",
      "model", "horizon"
    )) |>
    dplyr::filter(horizon == !!horizon_to_plot) |>
    dplyr::mutate(
      forecast_date_shifted = lubridate::ymd(forecast_date) +
        lubridate::days(days_to_shift)
    )

  p <- ggplot(scores_by_horizon) +
    geom_bar(aes(x = forecast_date_shifted, y = crps, fill = model),
      stat = "identity", position = "dodge", show.legend = FALSE
    ) +
    xlab("") +
    ylab("CRPS scores") +
    theme_bw() +
    scale_color_discrete() +
    scale_fill_discrete() +
    scale_x_date(
      date_breaks = "2 weeks",
      labels = scales::date_format("%Y-%m-%d"),
      limits = as.Date(c("2023-10-02", "2024-03-18"))
    ) +
    get_plot_theme(
      x_axis_dates = TRUE,
      y_axis_title_size = 8
    ) +
    scale_y_continuous(
      # don't expand y scale at the lower end
      expand = expansion(mult = c(0, 0.05))
    )


  return(p)
}

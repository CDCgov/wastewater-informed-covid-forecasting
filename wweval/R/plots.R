#' Get plot of wastewater data compared to model draws
#'
#' @param draws_w_data A long tidy dataframe containing draws from the model of
#' the estimated wastewater concentrations in each site joined with both the data
#' the model was calibrated to and the later observed data for evaluating the
#' future predicted concentrations against.
#' @param location the jurisdiction the data is from
#' @param model_type type of model the output is from, default is `ww`
#' @param n_draws number of draws to plot, default = 100
#'
#' @return a ggplot object faceted by site showing the draws
#' @export
get_plot_ww_data_comparison <- function(draws_w_data,
                                        location,
                                        model_type = "ww",
                                        n_draws = 100) {
  sampled_draws <- sample(1:max(draws_w_data$draw), n_draws)
  draws_w_data_subsetted <- draws_w_data |>
    dplyr::filter(
      draw %in% !!sampled_draws,
      name == "pred_ww"
    )

  p <- ggplot(draws_w_data_subsetted) +
    geom_line(aes(x = date, y = value, group = draw, color = site_lab_name),
      linewidth = 0.1, alpha = 0.1,
      show.legend = FALSE
    ) +
    geom_point(aes(x = date, y = eval_data),
      fill = "white", size = 1, shape = 21,
      show.legend = FALSE
    ) +
    geom_point(aes(x = date, y = calib_data),
      color = "black",
      show.legend = FALSE
    ) +
    geom_vline(aes(xintercept = lubridate::ymd(forecast_date)),
      linetype = "dashed"
    ) +
    scale_y_continuous(trans = "log10") +
    facet_wrap(~site_lab_name, scales = "free") +
    geom_point(
      data = draws_w_data_subsetted |> filter(below_LOD == 1),
      aes(x = date, y = calib_data), color = "red", size = 1.1
    ) +
    geom_point(
      data = draws_w_data_subsetted |> filter(flag_as_ww_outlier == 1),
      aes(x = date, y = calib_data), color = "blue", size = 1.1
    ) +
    xlab("") +
    ylab("Genome copies per mL") +
    ggtitle(glue::glue(
      "Site-level expected observed wastewater concentration in {location} from {model_type} model"
    )) +
    theme_bw() +
    scale_color_discrete() +
    scale_fill_discrete() +
    scale_x_date(
      date_breaks = "2 weeks",
      labels = scales::date_format("%Y-%m-%d")
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(
        size = 8, vjust = 1,
        hjust = 1, angle = 45
      ),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      plot.title = element_text(
        size = 10,
        vjust = 0.5, hjust = 0.5
      )
    )
  return(p)
}

#' Get plot of hospital admissions data compared to model draws
#'
#' @param draws_w_data A long tidy dataframe containing draws from the model of
#' the estimated hospital admissions joined with both the data
#' the model was calibrated to and the later observed data for evaluating the
#' future predicted concentrations against.
#' @param location the jursidiction the data is from
#' @param model_type type of model the output is from, options are
#' "ww" or "hosp"
#' @param n_draws number of draws to plot, default = 100
#'
#' @return a ggplot object showing the model draws of hospital admissions
#' alongside the calibration and forecast data
#' @export
get_plot_hosp_data_comparison <- function(draws_w_data,
                                          location,
                                          model_type = c("ww", "hosp"),
                                          n_draws = 100) {
  model_type <- arg_match(model_type)
  sampled_draws <- sample(1:max(draws_w_data$draw), n_draws)
  draws_w_data_subsetted <- draws_w_data |>
    dplyr::filter(
      draw %in% !!sampled_draws,
      name == "pred_hosp"
    )

  plot_color <- ifelse(model_type == "ww", "cornflowerblue", "purple4")


  p <- ggplot(draws_w_data_subsetted) +
    geom_line(aes(x = date, y = value, group = draw),
      color = plot_color,
      linewidth = 0.2, alpha = 0.4,
      show.legend = FALSE
    ) +
    geom_point(aes(x = date, y = eval_data),
      fill = "white", size = 1, shape = 21,
      show.legend = FALSE
    ) +
    geom_point(aes(x = date, y = calib_data),
      color = "black",
      show.legend = FALSE
    ) +
    geom_vline(aes(xintercept = lubridate::ymd(forecast_date)),
      linetype = "dashed"
    ) +
    # scale_y_continuous(trans = "log10") +
    xlab("") +
    ylab("Daily hospital admissions") +
    ggtitle(glue::glue(
      "Site-level expected observed hospital admissions in {location} from {model_type} model"
    )) +
    theme_bw() +
    scale_color_discrete() +
    scale_fill_discrete() +
    scale_x_date(
      date_breaks = "2 weeks",
      labels = scales::date_format("%Y-%m-%d")
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(
        size = 8, vjust = 1,
        hjust = 1, angle = 45
      ),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      plot.title = element_text(
        size = 10,
        vjust = 0.5, hjust = 0.5
      )
    )
  return(p)
}

#' Get plot quantile comparison
#'
#' @param hosp_quantiles dataframe of hospital admissions quantiles
#' @param eval_data Hospital admissions data for visual comparison
#' @param figure_file_path Outer directory for plots to go in
#' @param days_to_show_forecast Number of days to show the forecast, default
#' vlaue is 28
#' @param save_files Whether or not to write to a folder of plots,
#' default is `TRUE`
#'
#' @return a ggplot object with forecasts overlaid with evaluation data for
#' each scenarion in a specific location for visual comparison
#' @export
#'
get_plot_quantile_comparison <- function(hosp_quantiles,
                                         eval_data,
                                         figure_file_path,
                                         days_to_show_forecast = 28,
                                         save_files = TRUE) {
  location <- hosp_quantiles |>
    pull(location) |>
    unique()
  n_scenarios <- hosp_quantiles |>
    dplyr::pull(scenario) |>
    unique() |>
    length()

  eval_data_subsetted <- eval_data |>
    dplyr::filter(
      location == !!location,
      date <= max(hosp_quantiles$date),
      date >= min(hosp_quantiles$date[hosp_quantiles$period == "nowcast"])
    )



  quantiles_wide <- hosp_quantiles |>
    dplyr::filter(period != "calibration") |>
    dplyr::filter(quantile %in% c(0.025, 0.25, 0.5, 0.75, 0.975)) |>
    tidyr::pivot_wider(
      id_cols = c(
        forecast_date, period, scenario,
        date, t, eval_data
      ),
      names_from = quantile,
      values_from = value
    )


  p <- ggplot(quantiles_wide) +
    geom_point(
      data = eval_data_subsetted,
      aes(x = date, y = daily_hosp_admits),
      color = "black", alpha = 0.3
    ) +
    geom_line(
      data = eval_data_subsetted,
      aes(x = date, y = daily_hosp_admits),
      color = "black", alpha = 0.3
    ) +
    geom_line(
      data = quantiles_wide |> filter(
        date >= forecast_date,
        date <= forecast_date + days_to_show_forecast
      ),
      aes(
        x = date, y = `0.5`, group = forecast_date,
        color = scenario
      )
    ) +
    geom_ribbon(
      data = quantiles_wide |> filter(
        date >= forecast_date,
        date <= forecast_date + days_to_show_forecast
      ),
      aes(
        x = date, ymin = `0.025`, ymax = `0.975`,
        fill = scenario, group = forecast_date
      ), alpha = 0.1,
      show.legend = FALSE
    ) +
    geom_ribbon(
      data = quantiles_wide |> filter(
        date >= forecast_date,
        date <= forecast_date + days_to_show_forecast
      ),
      aes(
        x = date, ymin = `0.25`, ymax = `0.75`,
        fill = scenario, group = forecast_date
      ), alpha = 0.1,
      show.legend = FALSE
    ) +
    facet_wrap(~scenario, nrow = n_scenarios) +
    theme_bw() +
    xlab("") +
    ylab("Daily hospital admissions") +
    ggtitle(glue::glue(
      "Forecasted vs later observed hospital admissions in {location}"
    )) +
    scale_color_discrete() +
    scale_fill_discrete() +
    scale_x_date(
      date_breaks = "2 weeks",
      labels = scales::date_format("%Y-%m-%d")
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
    )

  if (isTRUE(save_files)) {
    full_file_path <- file.path(figure_file_path, "quantile_comparison")
    wwinference::create_dir(full_file_path)
    ggsave(
      file.path(
        full_file_path,
        glue::glue("quantiles_{location}.png")
      ),
      plot = p,
      width = 9,
      height = 9,
      units = "in",
      bg = "white"
    )
  }

  return(p)
}



get_plot_ww_comparison <- function(ww_quantiles,
                                   days_to_show_forecast = 28) {
  location <- ww_quantiles |>
    pull(location) |>
    unique()
  scenario <- ww_quantiles |>
    pull(scenario) |>
    unique()


  quantiles_wide <- ww_quantiles |>
    dplyr::filter(period != "calibration") |>
    dplyr::filter(quantile %in% c(0.025, 0.25, 0.5, 0.75, 0.975)) |>
    tidyr::pivot_wider(
      id_cols = c(
        forecast_date, period, scenario,
        date, t, eval_data, site_lab_name
      ),
      names_from = quantile,
      values_from = value
    )

  p <- ggplot(quantiles_wide) +
    geom_point(
      data = quantiles_wide |> filter(
        date >= forecast_date,
        date <= forecast_date + days_to_show_forecast
      ),
      aes(x = date, y = eval_data),
      color = "black"
    ) +
    geom_line(
      data = quantiles_wide |> filter(
        date >= forecast_date,
        date <= forecast_date + days_to_show_forecast
      ),
      aes(
        x = date, y = `0.5`, group = forecast_date,
        color = site_lab_name
      ),
      show.legend = FALSE
    ) +
    geom_ribbon(
      data = quantiles_wide |> filter(
        date >= forecast_date,
        date <= forecast_date + days_to_show_forecast
      ),
      aes(
        x = date, ymin = `0.025`, ymax = `0.975`,
        fill = site_lab_name, group = forecast_date
      ), alpha = 0.1,
      show.legend = FALSE
    ) +
    geom_ribbon(
      data = quantiles_wide |> filter(
        date >= forecast_date,
        date <= forecast_date + days_to_show_forecast
      ),
      aes(
        x = date, ymin = `0.25`, ymax = `0.75`,
        fill = site_lab_name, group = forecast_date
      ), alpha = 0.1,
      show.legend = FALSE
    ) +
    facet_wrap(~site_lab_name) +
    theme_bw() +
    xlab("") +
    ylab("Wastewater conncentration (genome copies per mL)") +
    ggtitle(glue::glue(
      "Forecasted vs later observed wastewater concentraion in {location} under {scenario} scenario"
    )) +
    scale_color_discrete() +
    scale_fill_discrete() +
    scale_y_continuous(trans = "log10") +
    scale_x_date(
      date_breaks = "2 weeks",
      labels = scales::date_format("%Y-%m-%d")
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
        size = 8,
        vjust = 0.5, hjust = 0.5
      )
    )

  return(p)
}


get_plot_final_scores <- function(final_scores,
                                  score_metric = "crps") {
  p <- ggplot(final_scores) +
    geom_bar(aes(x = scenario, y = .data[[score_metric]], fill = scenario),
      position = "dodge", stat = "identity"
    ) +
    theme_bw() +
    ylab(glue::glue("{score_metric} across forecast dates and locations"))
  return(p)
}

#' Get plot raw scores
#'
#' @param all_scores a dataframe containing daily scores for each scenario,
#' and forecast date, for a single location
#' @param score_metric A string indicating the score metric to plot,
#' default is "crps"
#'
#' @return a ggplot object plotting all of the scores over time
#' @export
#'
get_plot_raw_scores <- function(all_scores,
                                score_metric = "crps") {
  p <- ggplot(all_scores) +
    geom_point(aes(
      x = date, y = .data[[score_metric]],
      color = scenario, group = c(forecast_date)
    )) +
    facet_grid(forecast_date ~ location, scales = "free") +
    theme_bw()
  return(p)
}


#' Get plot scores with evaluation data overlaid
#'
#' @param all_scores a dataframe containing daily scores for each scenario,
#' and forecast date, for a single location
#' @param eval_data a dataframe containing the hospital admissions data
#' that the forecasts are evaluated against
#' @param figure_file_path Outer directory for plots to go in
#' @param score_metric A string indicating the score metric to plot,
#' default is "crps"
#' @param save_files Whether or not to write to a folder of plots,
#' default is `TRUE`
#'
#' @return a ggplot object plotting the scores summarized by forecast date
#' and scenario over time with the data overlaid
#' @export
#'
get_plot_scores_w_data <- function(all_scores,
                                   eval_data,
                                   figure_file_path,
                                   score_metric = "crps",
                                   save_files = TRUE) {
  location <- all_scores |>
    pull(location) |>
    unique()


  eval_data_subsetted <- eval_data |>
    dplyr::filter(
      location == !!location,
      date <= max(all_scores$date),
      date >= min(all_scores$forecast_date) - lubridate::days(10)
    )
  coeff <- (
    median(eval_data_subsetted$daily_hosp_admits, na.rm = TRUE) /
      median(all_scores |>
        dplyr::select({{ score_metric }}) |> # nolint
        dplyr::pull(), na.rm = TRUE)
  )


  scores <- all_scores |>
    dplyr::select(-tar_group) |>
    data.table::as.data.table()

  summarized_scores <- all_scores |>
    mutate(forecast_date = lubridate::ymd(forecast_date)) |>
    data.table::as.data.table() |>
    scoringutils::summarize_scores(
      by = c("scenario", "forecast_date", "location")
    )

  baseline_scores <- summarized_scores |>
    tibble::as_tibble() |>
    dplyr::filter(scenario == "no_wastewater") |>
    rename(baseline_score = {{ score_metric }}) |>
    select(location, forecast_date, baseline_score)


  summary_across_dates <- all_scores |>
    data.table::as.data.table() |>
    scoringutils::summarize_scores(
      by = c("location", "scenario")
    )

  coeff <- (
    median(eval_data_subsetted$daily_hosp_admits, na.rm = TRUE) /
      median(summarized_scores |>
        dplyr::select({{ score_metric }}) |> # nolint
        dplyr::pull(), na.rm = TRUE)
  )

  p <- ggplot(summarized_scores) +
    geom_bar(aes(x = forecast_date, y = .data[[score_metric]] * coeff, fill = scenario),
      stat = "identity", position = "dodge", alpha = 0.5
    ) +
    geom_point(
      data = eval_data_subsetted,
      aes(x = date, y = daily_hosp_admits),
      color = "black", alpha = 0.3
    ) +
    geom_line(
      data = eval_data_subsetted,
      aes(x = date, y = daily_hosp_admits),
      color = "black", alpha = 0.3
    ) +
    theme_bw() +
    xlab("") +
    ggtitle(glue::glue(
      "Score comparison over time in {location}"
    )) +
    scale_y_continuous(
      # Features of the first axis
      name = "Daily hospital admissions",

      # Add a second axis and specify its features
      sec.axis = sec_axis(
        trans = ~ . / coeff,
        name = glue::glue("{score_metric} by forecast date and scenario")
      )
    ) +
    scale_color_discrete() +
    scale_fill_discrete() +
    scale_x_date(
      date_breaks = "4 weeks",
      labels = scales::date_format("%Y-%m-%d")
    ) +
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
    )

  if (isTRUE(save_files)) {
    full_file_path <- file.path(figure_file_path, "scores_w_data_overlaid")
    wwinference::create_dir(full_file_path)
    ggsave(
      file.path(
        full_file_path,
        glue::glue("scores_{location}.png")
      ),
      plot = p,
      width = 9,
      height = 5,
      units = "in",
      bg = "white"
    )
  }
  return(p)
}

#' Get plot summarized scores
#'
#' @param all_scores a dataframe containing daily scores for each scenario,
#' and forecast date, for a single location
#' @param score_metric A string indicating the score metric to plot,
#' default is "crps"
#'
#' @return a ggplot object plotting the scores summarized by forecast date
#' and scenario over time
#' @export
#'
get_plot_summarized_scores <- function(all_scores,
                                       score_metric = "crps") {
  scores <- all_scores |>
    dplyr::select(-tar_group) |>
    data.table::as.data.table()

  summarized_scores <- all_scores |>
    mutate(forecast_date = lubridate::ymd(forecast_date)) |>
    data.table::as.data.table() |>
    scoringutils::summarize_scores(
      by = c("scenario", "period", "forecast_date", "location")
    )

  n_periods <- summarized_scores |>
    dplyr::pull(period) |>
    unique() |>
    length()
  location <- summarized_scores |>
    pull(location) |>
    unique()

  summary_across_dates <- all_scores |>
    data.table::as.data.table() |>
    scoringutils::summarize_scores(
      by = c("period", "location", "scenario")
    )

  p <- ggplot(summarized_scores) +
    geom_bar(aes(x = forecast_date, y = .data[[score_metric]], fill = scenario),
      stat = "identity", position = "dodge", alpha = 0.5
    ) +
    geom_hline(
      data = summary_across_dates,
      aes(yintercept = .data[[score_metric]], color = scenario)
    ) +
    facet_wrap(~period, nrow = n_periods) +
    theme_bw() +
    xlab("") +
    ylab(glue::glue("{score_metric} by forecast date and scenario")) +
    ggtitle(glue::glue(
      "Score comparison over time in {location}"
    )) +
    scale_color_discrete() +
    scale_fill_discrete() +
    scale_x_date(
      date_breaks = "4 weeks",
      labels = scales::date_format("%Y-%m-%d")
    ) +
    theme(
      axis.text.x = element_text(
        size = 8, vjust = 1,
        hjust = 1, angle = 45
      ),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      plot.title = element_text(
        size = 10,
        vjust = 0.5, hjust = 0.5
      )
    )

  return(p)
}


#' Get heatmap of scores by scenario
#'
#' @param mock_submission_scores The dataframe containing the daily scores for a
#' all of the forecast dates and locations
#' @param score_metric  A string indicating the score metric to plot,
#' default is "crps"
#'
#' @return a ggplot object that generates a heatmap colored by score, across
#' forecast dates (x axis) and locations (y-axis)
#' @export
#'
get_heatmap_scores <- function(mock_submission_scores,
                               score_metric = "crps") {
  summary_across_forecast_dates <- mock_submission_scores |>
    data.table::as.data.table() |>
    scoringutils::summarize_scores(
      by = c("location", "scenario", "forecast_date")
    )

  p <- ggplot(
    data = summary_across_forecast_dates,
    aes(x = forecast_date, y = location)
  ) +
    geom_tile(aes(fill = .data[[score_metric]]), colour = "white") +
    scale_fill_gradient(low = "white", high = "red") +
    facet_wrap(~scenario) +
    theme_bw() +
    theme(
      axis.text.x = element_text(
        size = 8, vjust = 1,
        hjust = 1, angle = 45
      ),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      plot.title = element_text(
        size = 10,
        vjust = 0.5, hjust = 0.5
      )
    ) +
    scale_x_date(
      date_breaks = "4 weeks",
      labels = scales::date_format("%Y-%m-%d")
    ) +
    xlab("Forecast date") +
    ylab("Location")

  return(p)
}

#' Get box plot by date and scenario
#'
#' @param all_scores the full set of scores for each location, forecast date,
#' scenario, and date
#' @param figure_file_path Outer directory for plots to go in
#' @param baseline_scenario the scenario to compute the difference in
#' crps scores relative to, default is `no_wastewater`
#' @param save_files Whether or not to write to a folder of plots,
#' default is `TRUE`
#'
#' @return p a ggplot object with a box plot showing the average improvement in
#' scores by forecast date and scenario for each location
#' @export
#'
get_box_plot <- function(all_scores,
                         figure_file_path,
                         baseline_scenario = "no_wastewater",
                         save_files = TRUE) {
  scores_by_date_scen_loc <- scoringutils::summarize_scores(all_scores,
    by = c(
      "scenario",
      "location",
      "forecast_date"
    )
  )

  baseline_only <- scores_by_date_scen_loc |>
    dplyr::filter(scenario == {{ baseline_scenario }}) |>
    rename(baseline_score = crps) |>
    select(location, forecast_date, baseline_score)

  overall_scores <- scores_by_date_scen_loc |>
    dplyr::left_join(baseline_only,
      by = c("location", "forecast_date")
    ) |>
    group_by(location, forecast_date, scenario) |>
    dplyr::summarize(
      relative_score = crps / baseline_score,
      diff_in_score = baseline_score - crps,
      mean_crps = mean(crps)
    )

  # Make a violin plot

  p <- ggplot(
    overall_scores,
    aes(
      x = factor(forecast_date), y = diff_in_score,
      fill = scenario, color = scenario
    )
  ) +
    geom_boxplot(position = "dodge", alpha = 0.5) +
    xlab("") +
    ylab(glue::glue(
      "CRPS improvement (`{baseline_scenario}` - scenario score)"
    )) +
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
    )

  if (isTRUE(save_files)) {
    full_file_path <- file.path(figure_file_path)
    wwinference::create_dir(full_file_path)
    ggsave(
      file.path(
        full_file_path,
        glue::glue("crps_improvement_over_time.png")
      ),
      plot = p,
      width = 9,
      height = 9,
      units = "in",
      bg = "white"
    )
  }

  return(p)
}

#' Get bar chart of the number of states improved compared to without wastewater
#' over each forecast date
#'
#' @param all_scores the full set of scores for each location, forecast date,
#' scenario, and date
#' @param figure_file_path Outer directory for plots to go in
#' @param baseline_scenario the scenario to compute the difference in
#' crps scores relative to, default is `no_wastewater`
#' @param threshold_for_improvement the relative reduction that we will consider
#' as an improvement over the baseline,
#' @param save_files Whether or not to write to a folder of plots,
#' default is `TRUE`
#'
#' @return p a ggplot object with a bar chart showing the number of
#' jurisdictions with an improvement compared to the forecast without
#' wastewater under each scenario.
#' @export
#'
get_n_states_improved_plot <- function(all_scores,
                                       figure_file_path,
                                       baseline_scenario = "no_wastewater",
                                       threshold_for_improvement = 1,
                                       save_files = TRUE) {
  scores_by_date_scen_loc <- scoringutils::summarize_scores(all_scores,
    by = c(
      "scenario",
      "location",
      "forecast_date"
    )
  )

  baseline_only <- scores_by_date_scen_loc |>
    dplyr::filter(scenario == {{ baseline_scenario }}) |>
    rename(baseline_score = crps) |>
    select(location, forecast_date, baseline_score)

  overall_scores <- scores_by_date_scen_loc |>
    dplyr::left_join(baseline_only,
      by = c("location", "forecast_date")
    ) |>
    group_by(location, forecast_date, scenario) |>
    dplyr::summarize(
      relative_score = crps / baseline_score,
      diff_in_score = baseline_score - crps,
      mean_crps = mean(crps)
    ) |>
    dplyr::mutate(
      is_improved = ifelse(relative_score < threshold_for_improvement, 1, 0)
    )

  summarize_scenarios <- overall_scores |>
    dplyr::group_by(scenario, forecast_date) |>
    dplyr::summarize(
      n_states_improved = sum(is_improved)
    )

  p <- ggplot(summarize_scenarios) +
    geom_bar(
      aes(
        x = factor(forecast_date),
        y = n_states_improved,
        fill = scenario
      ),
      alpha = 0.5,
      stat = "identity",
      position = "dodge"
    ) +
    xlab("") +
    ylab("Number of states with improved forecasts") +
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
    )

  if (isTRUE(save_files)) {
    full_file_path <- file.path(figure_file_path)
    wwinference::create_dir(full_file_path)
    ggsave(
      file.path(
        full_file_path,
        glue::glue("n_states_improved_over_time.png")
      ),
      plot = p,
      width = 9,
      height = 9,
      units = "in",
      bg = "white"
    )
  }

  return(p)
}


#' Get plot of WIS over time
#'
#' @param all_scores Scores from entire time period of interest, including
#' the retrospective cfa model
#' @param cfa_real_time_scores Real-time scores from Feb - Mar for the cfa ww
#' model submitted to the hub
#' @param figure_file_path directory to save figures in
#' @param horizon_time_in_weeks horizon time in weeks to summarize over, default
#' is `NULL` which means that the scores are summarized over 4 weeks
#' @param save_files bolean indicating whether or not to save the files, default
#' is `TRUE`
#'
#' @return a ggplot object of WIS scores over time colored by model, for the
#' real-time cfa model from Feb - Mar and the retrospective CFA model over
#' all time points
#' @export
#'
get_plot_wis_over_time <- function(all_scores,
                                   cfa_real_time_scores,
                                   figure_file_path,
                                   horizon_time_in_weeks = NULL,
                                   save_files = TRUE) {
  scores <- dplyr::bind_rows(all_scores, cfa_real_time_scores)

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
    title <- glue::glue("Average weighted interval scores by model")
  }

  p <- ggplot(scores_by_forecast_date) +
    geom_line(aes(
      x = forecast_date, y = interval_score,
      color = model
    )) +
    geom_point(aes(
      x = forecast_date, y = interval_score,
      color = model
    )) +
    xlab("Forecast date") +
    ylab("Average WIS score across locations") +
    theme_bw() +
    theme(
      legend.position = "bottom",
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
    scale_x_date(
      date_breaks = "2 weeks",
      labels = scales::date_format("%Y-%m-%d")
    ) +
    ggtitle(title)

  if (isTRUE(save_files)) {
    full_file_path <- file.path(figure_file_path, "hub_comparison")
    wwinference::create_dir(full_file_path)
    ggsave(
      file.path(
        full_file_path,
        glue::glue("average_WIS_over_time_by_model.png")
      ),
      plot = p,
      width = 8,
      height = 4,
      units = "in",
      bg = "white"
    )
  }

  return(p)
}


#' Get plot of overall hub performance
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
#' @param baseline_model which model to compute relative WIS compared to, default
#' is `COVIDhub-baseline`
#' @param save_files boolean indicating whether or not to save figures, default
#' is `TRUE`
#'
#' @return a ggplot object containing distributions of WIS scores grouped by
#' model and the comaprison time period, with the mean plotted alongside the
#' full distribution
#' @export
#'
get_plot_hub_performance <- function(all_scores,
                                     cfa_real_time_scores,
                                     figure_file_path,
                                     all_time_period,
                                     real_time_period,
                                     baseline_model = "COVIDhub-baseline",
                                     save_files = TRUE) {
  scores_by_model_all_time <- all_scores |>
    data.table::as.data.table() |>
    scoringutils::summarise_scores(
      by = c("model", "forecast_date", "location", "horizon")
    ) |>
    dplyr::mutate(
      period = {{ all_time_period }}
    )

  scores_by_model_real_time <- all_scores |>
    dplyr::filter(forecast_date >= lubridate::ymd("2024-02-05")) |>
    dplyr::bind_rows(cfa_real_time_scores) |>
    data.table::as.data.table() |>
    scoringutils::summarise_scores(
      by = c("model", "forecast_date", "location")
    ) |>
    dplyr::mutate(
      period = {{ real_time_period }}
    )

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
    theme_bw() +
    coord_trans(ylim = c(0, 2)) +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 8),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 10),
      plot.title = element_text(
        size = 10,
        vjust = 0.5, hjust = 0.5
      )
    ) +
    xlab("Time period") +
    ylab(glue::glue("Relative WIS compared to {baseline_model}")) +
    ggtitle("Distribution of relative WIS across locations, horizons, and forecast dates")


  if (isTRUE(save_files)) {
    full_file_path <- file.path(figure_file_path, "hub_comparison")
    wwinference::create_dir(full_file_path)
    ggsave(
      file.path(
        full_file_path,
        glue::glue("overall_performance_WIS_distros.png")
      ),
      plot = p,
      width = 8,
      height = 4,
      units = "in",
      bg = "white"
    )
  }


  return(p)
}


#' Get heatmap of relative WIS
#'
#' @param scores df of granular (daily) score across models, locations, forecast
#' dates and horizons
#' @param figure_file_path path to save figure
#' @param time_period time period that scores are summarized over
#' @param baseline_model which model to compute relative WIS compared to, default
#' is `COVIDhub-baseline`
#' @param save_files  save_files boolean indicating whether or not to save figures, default
#' is `TRUE`
#'
#' @return a ggplot with a heatmap with model on the x-axis, location on the y-axis
#' and fill by relative WIS score across forecast dates and horizons
#' @export
#'
get_heatmap_relative_wis <- function(scores,
                                     figure_file_path,
                                     time_period,
                                     baseline_model = "COVIDhub-baseline",
                                     save_files = TRUE) {
  summarized_scores <- scores |>
    data.table::as.data.table() |>
    scoringutils::summarise_scores(
      by = c("model", "location")
    ) |>
    dplyr::left_join(cfaforecastrenewalww::flusight_location_table,
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
    dplyr::filter(model != {{ baseline_model }})


  p <- ggplot(relative_scores) +
    geom_tile(aes(x = model, y = short_name, fill = relative_interval_score)) +
    scale_fill_gradient2(
      high = "red", mid = "white", low = "blue", transform = "log2",
      midpoint = 1, guide = "colourbar", aesthetics = "fill"
    ) +
    geom_text(aes(
      x = model, y = short_name,
      label = round(relative_interval_score, 2)
    ), size = 2.5) +
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
    ylab("") +
    labs(fill = "Relative WIS") +
    ggtitle(glue::glue("Relative WIS compared to {baseline_model} from {time_period}"))


  if (isTRUE(save_files)) {
    full_file_path <- file.path(figure_file_path, "hub_comparison")
    wwinference::create_dir(full_file_path)
    ggsave(
      file.path(
        full_file_path,
        glue::glue("heatmap_{baseline_model}_{time_period}.png")
      ),
      plot = p,
      width = 6,
      height = 10,
      units = "in",
      bg = "white"
    )
  }



  return(p)
}

#' Get quantile-quantile plot
#'
#' @param scores df of granular (daily) score across models, locations, forecast
#' dates and horizons
#' @param figure_file_path path to save figure
#' @param time_period time period that scores are summarized over
#' @param save_files  save_files boolean indicating whether or not to save figures, default
#' is `TRUE`
#'
#' @return a ggplot object containing a plot of the proportion of data within
#' each interval for each model.
#' @export
#'
get_qq_plot <- function(scores,
                        figure_file_path,
                        time_period,
                        save_files = TRUE) {
  p <- scores |>
    data.table::as.data.table() |>
    scoringutils::summarise_scores(by = c("model", "range")) |>
    scoringutils::plot_interval_coverage() +
    ggtitle(glue::glue("QQ plot for {time_period}"))


  if (isTRUE(save_files)) {
    full_file_path <- file.path(figure_file_path, "hub_comparison")
    wwinference::create_dir(full_file_path)
    ggsave(
      file.path(
        full_file_path,
        glue::glue("qq_plot_{time_period}.png")
      ),
      plot = p,
      width = 6,
      height = 6,
      units = "in",
      bg = "white"
    )
  }

  return(p)
}

get_plot_ww_data <- function(eval_data) {
  eval_data <- eval_data |>
    dplyr::mutate(
      lab_site_name = glue::glue("Site: {site}, lab: {lab}")
    )
  loc <- eval_data |>
    dplyr::distinct(location) |>
    dplyr::pull()

  p <- ggplot(eval_data) +
    geom_point(aes(x = date, y = log(ww)), size = 0.5) +
    geom_line(aes(x = date, y = log(ww)), size = 0.5) +
    geom_point(
      data = eval_data |> dplyr::filter(flag_as_ww_outlier == 1),
      aes(x = date, y = log(ww)),
      fill = "red", color = "red", size = 0.5
    ) +
    geom_point(
      data = eval_data |> dplyr::filter(below_LOD == 1),
      aes(x = date, y = log(ww)),
      fill = "darkblue", color = "darkblue", size = 0.5
    ) +
    facet_wrap(~lab_site_name, scales = "free_y") +
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
    ylab("Log(genome copies per mL)") +
    ggtitle(glue::glue("Wastewater concentration data in {loc}"))

  return(p)
}

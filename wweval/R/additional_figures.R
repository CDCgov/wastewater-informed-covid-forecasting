#' Got a plot of forecasts and scores for a single
#' location and date
#'
#' @param scores_single_loc_date Scores for a single location
#' and data, as the output of [scoringutils::score()].
#' @param eval_output_subdir Subdirectory in which to save the plot.
#'
#' @export
get_plot_scores_and_forecasts <- function(
  scores_single_loc_date,
  eval_output_subdir
) {
  this_location <- unique(scores_single_loc_date$location)

  this_forecast_date <- unique(scores_single_loc_date$forecast_date)

  colors <- plot_components()

  ## Get the forecasts from file storage
  fp_ww <- get_filepath(
    eval_output_subdir,
    scenario = "status_quo",
    forecast_date = this_forecast_date,
    model_type = "ww",
    location = this_location,
    output_type = glue::glue("hosp_quantiles"),
    file_extension = "tsv"
  )

  if (file.exists(fp_ww)) {
    hosp_quantiles_ww <- readr::read_tsv(fp_ww)

    fp_hosp <- get_filepath(
      eval_output_subdir,
      scenario = "no_wastewater",
      forecast_date = this_forecast_date,
      model_type = "hosp",
      location = this_location,
      output_type = glue::glue("quantiles"),
      file_extension = "tsv"
    )
    hosp_quantiles_hosp <- readr::read_tsv(fp_hosp)

    min_scores_date <- min(scores_single_loc_date$date)

    quantiles <- hosp_quantiles_ww |>
      dplyr::bind_rows(hosp_quantiles_hosp)

    quantiles_wide <- quantiles |>
      dplyr::filter(
        quantile %in% c(0.025, 0.25, 0.5, 0.75, 0.975)
      ) |>
      tidyr::pivot_wider(
        id_cols = c(
          location,
          forecast_date,
          period,
          scenario,
          date,
          eval_data,
          calib_data,
          model_type
        ),
        names_from = quantile,
        values_from = value
      )

    quantiles_wide_forecast <- quantiles_wide |>
      dplyr::filter(date >= min_scores_date)

    scores_avg <- scores_single_loc_date |>
      dplyr::group_by(forecast_date, location, model) |>
      dplyr::summarize(avg_crps = mean(crps))

    p_scores_t <- ggplot(scores_single_loc_date) +
      geom_line(
        aes(x = date, y = crps, color = model),
        show.legend = FALSE
      ) +
      get_plot_theme(
        x_axis_dates = TRUE
      ) +
      scale_x_date(
        date_breaks = "1 week",
        date_labels = "%Y-%m-%d",
      ) +
      xlab(NULL) +
      ylab("CRPS") +
      geom_vline(
        aes(xintercept = forecast_date),
        linetype = "dashed"
      ) +
      scale_color_manual(values = colors$model_colors)

    p_scores_avg <- ggplot(scores_avg) +
      geom_bar(
        aes(x = model, y = avg_crps, fill = model),
        stat = "identity",
        position = "dodge",
        show.legend = FALSE
      ) +
      get_plot_theme(
        x_axis_dates = FALSE
      ) +
      xlab("Model") +
      ylab("Mean CRPS") +
      scale_fill_manual(values = colors$model_colors)

    p_forecasts <- ggplot(quantiles_wide_forecast) +
      geom_point(
        aes(x = date, y = eval_data),
        fill = "white",
        size = 1,
        shape = 21,
        show.legend = FALSE
      ) +
      geom_point(
        aes(x = date, y = calib_data),
        color = "black",
        show.legend = FALSE
      ) +
      geom_line(
        aes(
          x = date,
          y = `0.5`,
          color = model_type
        )
      ) +
      geom_ribbon(
        aes(
          x = date,
          ymin = `0.025`,
          ymax = `0.975`,
          fill = model_type
        ),
        alpha = 0.1
      ) +
      geom_ribbon(
        aes(
          x = date,
          ymin = `0.25`,
          ymax = `0.75`,
          fill = model_type
        ),
        alpha = 0.2,
      ) +
      geom_vline(
        aes(
          xintercept = lubridate::ymd(
            this_forecast_date
          )
        ),
        linetype = "dashed"
      ) +
      scale_x_date(
        date_breaks = "1 week",
        date_labels = "%Y-%m-%d"
      ) +
      xlab(NULL) +
      ylab("Daily hospital admissions") +
      scale_color_manual(values = colors$model_colors) +
      scale_fill_manual(values = colors$model_colors) +
      get_plot_theme(x_axis_dates = TRUE) +
      theme(
        legend.position = "top",
        legend.justification = "left"
      ) +
      labs(color = "Model", fill = "Model") +
      ggtitle(glue::glue(
        "{this_forecast_date} in {this_location}"
      ))

    fig <- patchwork::wrap_plots(
      p_forecasts,
      p_scores_t,
      p_scores_avg,
      guides = "collect",
      nrow = 3,
      ncol = 1,
      axes = "collect",
      widths = c(1, 1.5)
    ) &
      theme(
        legend.position = "top",
        legend.justification = "left"
      )

    fig_file_dir <- file.path(
      eval_output_subdir,
      "status_quo",
      this_forecast_date,
      "ww",
      this_location
    )
    alt_fig_file_dir <- file.path(
      eval_output_subdir,
      "status_quo",
      this_location
    )
    fs::dir_create(alt_fig_file_dir)

    ggsave(
      fig,
      filename = file.path(
        fig_file_dir,
        "forecast_and_score_comp_fig.png"
      ),
      width = 7,
      height = 10
    )

    ggsave(
      fig,
      filename = file.path(
        alt_fig_file_dir,
        glue::glue("comp_{this_forecast_date}.png")
      )
    )
  } else {
    fig <- NULL
  }

  return(fig)

  # Also make and save a figure with the full calibration period
  p_forecasts_all <- ggplot(quantiles_wide) +
    geom_point(
      aes(x = date, y = eval_data),
      fill = "white",
      size = 1,
      shape = 21,
      show.legend = FALSE
    ) +
    geom_point(
      aes(x = date, y = calib_data),
      color = "black",
      show.legend = FALSE
    ) +
    geom_line(
      aes(
        x = date,
        y = `0.5`,
        color = model_type
      )
    ) +
    geom_ribbon(
      aes(
        x = date,
        ymin = `0.025`,
        ymax = `0.975`,
        fill = model_type
      ),
      alpha = 0.1
    ) +
    geom_ribbon(
      aes(
        x = date,
        ymin = `0.25`,
        ymax = `0.75`,
        fill = model_type
      ),
      alpha = 0.2,
    ) +
    geom_vline(
      aes(xintercept = lubridate::ymd(this_forecast_date)),
      linetype = "dashed"
    ) +
    scale_x_date(
      date_breaks = "1 week",
      date_labels = "%Y-%m-%d"
    ) +
    xlab(NULL) +
    ylab("Daily hospital admissions") +
    scale_color_manual(values = colors$model_colors) +
    scale_fill_manual(values = colors$model_colors) +
    get_plot_theme(x_axis_dates = TRUE) +
    theme(
      legend.position = "top",
      legend.justification = "left"
    ) +
    labs(color = "Model", fill = "Model") +
    ggtitle(glue::glue("{this_forecast_date} in {this_location}"))

  ggsave(
    p_forecasts_all,
    filename = file.path(fig_file_dir, "calib_and_forecasts.png"),
    width = 7,
    height = 10
  )

  ggsave(
    p_forecasts_all,
    filename = file.path(
      alt_fig_file_dir,
      glue::glue(
        "calib_and_forecasts_{this_forecast_date}.png"
      )
    )
  )
}

get_plot_wis_t <- function(
  hosp_quantiles,
  scores,
  eval_output_subdir,
  submissions_path = "https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/master/data-processed/", # nolint
  truth_data_path = "https://media.githubusercontent.com/media/reichlab/covid19-forecast-hub/master/data-truth/truth-Incident%20Hospitalizations.csv", # nolint
  hub_comparison_model = "COVIDhub-4_week_ensemble"
) {
  truth_data <- readr::read_csv(truth_data_path)

  this_location <- hosp_quantiles |>
    dplyr::distinct(location) |>
    dplyr::pull()

  loc_code <- forecasttools::us_loc_abbr_to_code(this_location)

  this_forecast_date <- hosp_quantiles |>
    dplyr::distinct(forecast_date) |>
    dplyr::pull()
  scores <- scores |>
    dplyr::filter(
      forecast_date == !!this_forecast_date,
      location == !!loc_code
    )

  hub_quantiles <-
    readr::read_csv(glue::glue(
      "{submissions_path}{hub_comparison_model}/{this_forecast_date}-{hub_comparison_model}.csv"
    )) |>
    dplyr::filter(
      type == "quantile",
      location == loc_code
    )
  hub_quantiles_wide <- hub_quantiles |>
    dplyr::filter(
      quantile %in% c(0.025, 0.25, 0.5, 0.75, 0.975)
    ) |>
    tidyr::pivot_wider(
      id_cols = c(
        location,
        forecast_date,
        target_end_date
      ),
      names_from = quantile,
      values_from = value
    ) |>
    dplyr::left_join(
      truth_data |>
        dplyr::filter(
          location == loc_code,
          date > this_forecast_date,
          date <= this_forecast_date + 28
        ) |>
        dplyr::rename(truth = value),
      by = c("target_end_date" = "date", "location")
    )

  p_hub_forecasts <- ggplot(hub_quantiles_wide) +
    geom_point(
      aes(x = target_end_date, y = truth),
      fill = "white",
      size = 1,
      shape = 21,
      show.legend = FALSE
    ) +
    geom_line(
      aes(
        x = target_end_date,
        y = `0.5`
      ),
      color = "gray"
    ) +
    geom_ribbon(
      aes(
        x = target_end_date,
        ymin = `0.025`,
        ymax = `0.975`
      ),
      fill = "gray",
      alpha = 0.1
    ) +
    geom_ribbon(
      aes(
        x = target_end_date,
        ymin = `0.25`,
        ymax = `0.75`
      ),
      fill = "gray",
      alpha = 0.2,
    ) +
    geom_vline(
      aes(xintercept = lubridate::ymd(this_forecast_date)),
      linetype = "dashed"
    ) +
    scale_x_date(
      date_breaks = "1 week",
      date_labels = "%Y-%m-%d"
    ) +
    xlab(NULL) +
    ylab("Daily hospital admissions") +
    get_plot_theme(x_axis_dates = TRUE) +
    theme(
      legend.position = "top",
      legend.justification = "left"
    ) +
    coord_cartesian(
      ylim = c(0, 2 * max(hosp_quantiles$eval_data))
    ) +
    ggtitle(glue::glue(
      "{this_forecast_date} in {this_location} {hub_comparison_model}"
    ))

  quantiles_wide <- hosp_quantiles |>
    dplyr::filter(
      quantile %in% c(0.025, 0.25, 0.5, 0.75, 0.975),
      date >= this_forecast_date
    ) |>
    tidyr::pivot_wider(
      id_cols = c(
        location,
        forecast_date,
        period,
        scenario,
        date,
        eval_data,
        calib_data,
        model_type
      ),
      names_from = quantile,
      values_from = value
    )
  colors <- plot_components()
  ## First plot the forecasts from renewal models
  p_forecasts <- ggplot(quantiles_wide) +
    geom_point(
      aes(x = date, y = eval_data),
      fill = "white",
      size = 1,
      shape = 21,
      show.legend = FALSE
    ) +
    geom_point(
      aes(x = date, y = calib_data),
      color = "black",
      show.legend = FALSE
    ) +
    geom_line(
      aes(
        x = date,
        y = `0.5`,
        color = model_type
      ),
      show.legend = FALSE
    ) +
    geom_ribbon(
      aes(
        x = date,
        ymin = `0.025`,
        ymax = `0.975`,
        fill = model_type
      ),
      alpha = 0.1,
      show.legend = FALSE
    ) +
    geom_ribbon(
      aes(
        x = date,
        ymin = `0.25`,
        ymax = `0.75`,
        fill = model_type
      ),
      alpha = 0.2,
      show.legend = FALSE
    ) +
    geom_vline(
      aes(xintercept = lubridate::ymd(this_forecast_date)),
      linetype = "dashed"
    ) +
    scale_x_date(
      date_breaks = "1 week",
      date_labels = "%Y-%m-%d"
    ) +
    xlab(NULL) +
    ylab("Daily hospital admissions") +
    scale_color_manual(values = colors$model_colors) +
    scale_fill_manual(values = colors$model_colors) +
    get_plot_theme(x_axis_dates = TRUE) +
    theme(
      legend.position = "top",
      legend.justification = "left"
    ) +
    labs(color = "Model", fill = "Model") +
    coord_cartesian(
      ylim = c(0, 2 * max(hosp_quantiles$eval_data))
    ) +
    ggtitle(glue::glue("{this_forecast_date} in {this_location}"))

  scores_to_plot <- scores |>
    dplyr::filter(
      model %in%
        c(
          "cfa-wwrenewal",
          "cfa-hosponlyrenewal",
          {{ hub_comparison_model }}
        )
    ) |>
    dplyr::group_by(model, target_end_date) |>
    dplyr::summarize(avg_wis = mean(interval_score)) |>
    dplyr::mutate(
      model = dplyr::case_when(
        model == "cfa-wwrenewal" ~ "cfa-wwrenewal(retro)",
        model == "cfa-hosponlyrenewal" ~ "cfa-hosponlyrenewal(retro)",
        TRUE ~ model
      )
    )

  avg_scores <- scores |>
    dplyr::filter(
      model %in%
        c(
          "cfa-wwrenewal",
          "cfa-hosponlyrenewal",
          {{ hub_comparison_model }}
        )
    ) |>
    dplyr::group_by(model) |>
    dplyr::summarize(avg_wis = mean(interval_score)) |>
    dplyr::mutate(
      model = dplyr::case_when(
        model == "cfa-wwrenewal" ~ "cfa-wwrenewal(retro)",
        model == "cfa-hosponlyrenewal" ~ "cfa-hosponlyrenewal(retro)",
        TRUE ~ model
      )
    )

  scores_t <- ggplot(scores_to_plot) +
    geom_line(aes(
      x = target_end_date,
      y = avg_wis,
      color = model
    )) +
    get_plot_theme(x_axis_dates = TRUE) +
    theme(
      legend.position = "top",
      legend.justification = "left"
    ) +
    labs(color = "Model") +
    scale_color_manual(values = colors$model_colors) +
    xlab(NULL) +
    ylab("WIS")

  scores_bar <- ggplot(avg_scores) +
    geom_bar(
      aes(x = model, y = avg_wis, fill = model),
      show.legend = FALSE,
      stat = "identity",
      position = "dodge"
    ) +
    get_plot_theme() +
    scale_fill_manual(values = colors$model_colors) +
    xlab(NULL) +
    ylab("WIS")

  fig <- patchwork::wrap_plots(
    p_forecasts,
    p_hub_forecasts,
    scores_t,
    scores_bar,
    guides = "collect",
    nrow = 4,
    ncol = 1,
    axes = "collect",
    widths = c(1, 1.5)
  ) &
    theme(
      legend.position = "top",
      legend.justification = "left"
    )

  fig_file_dir <- file.path(
    eval_output_subdir,
    "status_quo",
    this_forecast_date,
    "ww",
    this_location
  )
  alt_fig_file_dir <- file.path(
    eval_output_subdir,
    "status_quo",
    this_location
  )
  fs::dir_create(alt_fig_file_dir)
  fs::dir_create(fig_file_dir)

  ggsave(
    fig,
    filename = file.path(fig_file_dir, "hub_comparison_fig.png"),
    width = 7,
    height = 11
  )

  ggsave(
    fig,
    filename = file.path(
      alt_fig_file_dir,
      glue::glue("hub_comp_{this_forecast_date}.png")
    )
  )
  return(fig)
}


#' Get a plot of bias over time
#'
#' @param scores a complete df of scores with a column for bias
#' @param fig_subscript subscript to name the figure
#' @param fig_file_dir Path to save figures
#'
#' @return a plot of bias over time averaged across locations and forecast dates,
#' separated by model
#' @export
get_plot_bias_over_time <- function(scores, fig_subscript, fig_file_dir) {
  bias_over_time <- scores |>
    dplyr::group_by(forecast_date, model) |>
    dplyr::summarize(
      avg_bias = mean(bias)
    )
  colors <- plot_components()

  p <- ggplot(bias_over_time) +
    geom_line(aes(x = forecast_date, y = avg_bias, color = model)) +
    get_plot_theme(
      x_axis_dates = TRUE,
      y_axis_title_size = 8
    ) +
    scale_x_date(
      date_breaks = "2 weeks",
      date_labels = "%Y-%m-%d"
    ) +
    geom_hline(aes(yintercept = 0), linetype = "dashed") +
    scale_color_manual(values = colors$model_colors) +
    xlab(NULL) +
    ylab("Average bias") +
    ggtitle("Average bias over time, across horizons and locations")

  fs::dir_create(fig_file_dir)

  ggsave(
    p,
    width = 10,
    height = 5,
    filename = file.path(
      fig_file_dir,
      glue::glue("fig_bias_over_time_{fig_subscript}.svg")
    )
  )
  ggsave(
    p,
    width = 10,
    height = 5,
    filename = file.path(
      fig_file_dir,
      glue::glue("fig_bias_over_time_{fig_subscript}.png")
    )
  )

  return(p)
}

#' Get plot of scores by horizon over time
#'
#' @param scores tibble of scores for every location, forecast date, and horizon
#' @param score_type string indicating the type of score to summarize over
#' @param fig_file_dir Path to save figures
#' @return plot of scores over time faceted by horizon
#' @export
get_plot_score_by_horizon_t <- function(scores, score_type, fig_file_dir) {
  scores_by_horizon_and_t <- scores |>
    # hack, will fix upstream
    dplyr::filter(
      !horizon %in% c("0 week ahead", "5 week ahead")
    ) |>
    dplyr::group_by(horizon, forecast_date, model) |>
    dplyr::filter(!is.na(horizon)) |>
    dplyr::summarize(
      mean_score = mean(!!sym(score_type))
    )

  colors <- plot_components()

  p <- ggplot(scores_by_horizon_and_t) +
    geom_line(aes(
      x = forecast_date,
      y = mean_score,
      color = model
    )) +
    get_plot_theme(
      x_axis_dates = TRUE,
      y_axis_title_size = 8
    ) +
    facet_wrap(~horizon, ncol = 1) +
    scale_x_date(
      date_breaks = "2 weeks",
      date_labels = "%Y-%m-%d"
    ) +
    scale_color_manual(values = colors$model_colors) +
    xlab(NULL) +
    ylab(glue::glue("Average {score_type}")) +
    ggtitle(glue::glue(
      "Average {score_type} over time, across locations"
    ))

  fs::dir_create(fig_file_dir)
  ggsave(
    p,
    width = 10,
    height = 10,
    filename = file.path(
      fig_file_dir,
      glue::glue("fig_{score_type}_over_time_by_horizon.svg")
    )
  )
  ggsave(
    p,
    width = 10,
    height = 10,
    filename = file.path(
      fig_file_dir,
      glue::glue("fig_{score_type}_over_time_by_horizon.png")
    )
  )
  return(p)
}


#' Get stats on number of improved forecasts
#'
#' @param scores tibble of scores for every location, forecast date, and horizon
#' @param threshold numeric indicating fold change for considering a forecast
#' improved or worse relative to baseline, e.g. 1.1
#' @param target_model Name of the target model
#' @param baseline_model Name of the baseline model
#' @param metric_to_compare Metric for which to compute
#' relative scores. One of `"wis"` or `"crps"`. Not case-sensitive.
#' Passed as the `metric_to_compare` argument
#' to [scoringutils::get_pairwise_comparisons()] via
#' [forecasttools::summarise_scores_with_baseline()].
#' @return table of the number of states with improvements, number of overall
#' forecasts with improvements, number that got worse, etc.
#' @export
get_stats_improved_forecasts <- function(
  scores,
  threshold,
  target_model,
  baseline_model,
  metric_to_compare
) {
  relative_score_by_loc <- .target_model_relative_scores(
    scores = scores,
    target_model = target_model,
    baseline_model = baseline_model,
    metric_to_compare = metric_to_compare,
    by = "location"
  ) |>
    na.omit()

  n_states <- nrow(relative_score_by_loc)

  n_states_better <- relative_score_by_loc |>
    dplyr::filter(.data$mean_scores_ratio < 1) |>
    nrow()

  n_states_worse <- relative_score_by_loc |>
    dplyr::filter(.data$mean_scores_ratio > 1) |>
    nrow()

  n_states_equal <- relative_score_by_loc |>
    dplyr::filter(.data$mean_scores_ratio == 1) |>
    nrow()

  stopifnot(n_states == n_states_better + n_states_worse + n_states_equal)

  relative_score_by_date_loc <- .target_model_relative_scores(
    scores = scores,
    target_model = target_model,
    baseline_model = baseline_model,
    metric_to_compare = metric_to_compare,
    by = c("location", "forecast_date")
  ) |>
    na.omit()

  n_forecasts <- nrow(relative_score_by_date_loc)

  n_forecasts_3x_worse <- relative_score_by_date_loc |>
    dplyr::filter(.data$mean_scores_ratio > 3) |>
    nrow()

  n_forecasts_3x_better <- relative_score_by_date_loc |>
    dplyr::filter(.data$mean_scores_ratio < 1 / 3) |>
    nrow()

  n_forecasts_better <- relative_score_by_date_loc |>
    dplyr::filter(.data$mean_scores_ratio < 1) |>
    nrow()

  n_forecasts_worse <- relative_score_by_date_loc |>
    dplyr::filter(.data$mean_scores_ratio > 1) |>
    nrow()

  n_forecasts_equal <- relative_score_by_date_loc |>
    dplyr::filter(.data$mean_scores_ratio == 1) |>
    nrow()

  n_forecasts_better_thres <- relative_score_by_date_loc |>
    dplyr::filter(
      .data$mean_scores_ratio < 1 / !!threshold
    ) |>
    nrow()

  n_forecasts_worse_thres <- relative_score_by_date_loc |>
    dplyr::filter(
      .data$mean_scores_ratio > !!threshold
    ) |>
    nrow()

  stopifnot(
    n_forecasts ==
      n_forecasts_better +
        n_forecasts_worse +
        n_forecasts_equal
  )

  stats <- tibble::tibble(
    n_states_better,
    n_states_worse,
    n_forecasts_better,
    n_forecasts_worse,
    n_forecasts_better_thres,
    n_forecasts_worse_thres,
    n_forecasts_3x_worse,
    n_forecasts_3x_better
  )

  return(stats)
}


#' Make a scatterplot comparing number of wastewater
#' sites to model performance.
#'
#' @param scores data frame of scores
#' @param ww_metadata data frame of wastewater metadata.
#' @param fig_file_dir directory in which to save the figure.
#' @return The figure, saving it to disk as a side effect.
#' @export
get_plot_sites_vs_performance <- function(scores, ww_metadata, fig_file_dir) {
  scores_summarized <- scores |>
    dplyr::group_by(location, forecast_date) |>
    dplyr::summarise(avg_crps = mean(crps))

  scores_joined <- scores_summarized |>
    dplyr::left_join(
      ww_metadata,
      by = c("forecast_date", "location")
    )

  p_n_sites <- ggplot(scores_joined) +
    geom_point(aes(x = n_sites, y = avg_crps))

  p_coverage <- ggplot(scores_joined) +
    geom_point(aes(x = pop_coverage, y = avg_crps))

  ggsave(
    p_n_sites,
    filename = file.path(
      fig_file_dir,
      glue::glue("fig_n_sites_vs_crps.png")
    )
  )

  ggsave(
    p_coverage,
    filename = file.path(
      fig_file_dir,
      glue::glue("fig_pop_coverage_vs_crps.png")
    )
  )

  return(p_n_sites)
}

#' Get a heatmap of the metadata of reasons for excluding forecasts from analysis
#'
#' @param metadata a tibble of location -forecast date metadata
#' @param type_of_analysis either "retro_comparison" or "hub_comparison"
#' @param fig_file_dir string indicating where to save figs
#'
#' @return a ggplot object with a heatmap colored by reason for excluding
#' @export
get_heatmap_metadata <- function(metadata, type_of_analysis, fig_file_dir) {
  metadata_summarized <- metadata |>
    dplyr::select(
      forecast_date,
      location,
      ww_data_present,
      ww_sufficient,
      any_flags_hosp,
      any_flags_ww
    ) |>
    dplyr::ungroup()

  if (type_of_analysis == "retro_comparison") {
    metadata_final <- metadata_summarized |>
      dplyr::mutate(
        metadata_cat = case_when(
          ww_data_present != 1 ~ "absent or insufficient wastewater",
          ww_sufficient != TRUE ~ "absent or insufficient wastewater",
          any_flags_ww == TRUE ~ "model had convergence issues",
          any_flags_hosp == TRUE ~ "model had convergence issues",
          TRUE ~ "both models produced forecasts"
        )
      )
  } else {
    metadata_final <- metadata_summarized |>
      dplyr::mutate(
        metadata_cat = case_when(
          ww_data_present != 1 ~ "absent or insufficient wastewater",
          ww_sufficient != TRUE ~ "absent or insufficient wastewater",
          any_flags_ww == TRUE ~ "model had convergence issues",
          any_flags_hosp == TRUE ~ "model had convergence issues",
          ww_exclude_manual == TRUE ~ "manual exclusion of ww model",
          TRUE ~ "both models produced forecasts"
        )
      )
  }

  p <- ggplot(metadata_final) +
    geom_tile(aes(
      x = forecast_date,
      y = location,
      fill = metadata_cat
    )) +
    scale_fill_discrete() +
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
    labs(fill = "Metadata Information") +
    ggtitle(glue::glue(
      "Summary of retrospective comparison analysis"
    ))

  ggsave(
    p,
    filename = file.path(
      fig_file_dir,
      glue::glue("fig_heatmap_metadata.png")
    )
  )

  return(p)
}

#' Get a heatmap of the metadata of Hub models submitted
#'
#' @param metadata a tibble of location -forecast date metadata
#' @param analysis_type string indicating whether this is the
#' real-time or retro analysis, which dictates how metadata is gathered
#' @param fig_file_dir string indicating where to save figs
#'
#' @return a ggplot object with a heatmap colored by reason for excluding
#' @export
get_heatmap_metadata_hub <- function(metadata, analysis_type, fig_file_dir) {
  if (analysis_type == "retro") {
    metadata_summarized <- metadata |>
      dplyr::select(
        forecast_date,
        location,
        ww_data_present,
        ww_sufficient,
        any_flags_hosp,
        any_flags_ww
      ) |>
      dplyr::ungroup() |>
      dplyr::mutate(
        model_submitted = dplyr::case_when(
          ww_data_present != 1 ~ "hosp",
          ww_sufficient != TRUE ~ "hosp",
          any_flags_ww == TRUE ~ "hosp",
          TRUE ~ "ww"
        )
      ) |>
      dplyr::mutate(
        model_name = "cfa-wwrenewal(retro)"
      )

    metadata_hosp_only <- metadata_summarized |>
      dplyr::mutate(
        model_submitted = "hosp",
        model_name = "cfa-hosponlyrenewal(retro)"
      )

    all_metadata <- dplyr::bind_rows(
      metadata_summarized,
      metadata_hosp_only
    )
  } else if (analysis_type == "real_time") {
    # Then we need to get this info on metadata from our github!
    dates <- seq(
      from = lubridate::ymd("2024-02-05"),
      to = lubridate::ymd("2024-03-11"),
      by = "week"
    )
    df_replacements <- get_date_locs_hosp_used(dates) |>
      dplyr::mutate(
        model_submitted = "hosp"
      )
    locs <- unique(metadata$location)
    metadata_grid <- expand.grid(
      location = locs,
      forecast_date = dates
    )
    metadata_ww <- metadata_grid |>
      dplyr::left_join(
        df_replacements
      ) |>
      dplyr::mutate(
        model_submitted = ifelse(
          is.na(model_submitted),
          "ww",
          "hosp"
        ),
        model_name = "cfa-wwrenewal(real-time)"
      )
    metadata_hosp <- metadata_grid |>
      dplyr::mutate(
        model_submitted = "hosp",
        model_name = "cfa-hosponlyrenewal(real-time)"
      )
    all_metadata <- dplyr::bind_rows(metadata_ww, metadata_hosp)
  }

  colors <- plot_components()
  p <- ggplot(all_metadata) +
    geom_tile(aes(
      x = forecast_date,
      y = location,
      fill = model_submitted
    )) +
    scale_fill_discrete() +
    facet_wrap(~model_name) +
    get_plot_theme(
      x_axis_dates = TRUE,
      y_axis_text_size = 4
    ) +
    scale_x_date(
      date_breaks = "1 week",
      labels = scales::date_format("%Y-%m-%d")
    ) +
    theme(legend.position = "bottom") +
    xlab("") +
    ylab("Location") +
    labs(fill = "Model submitted") +
    ggtitle(glue::glue("Summary of models used in Hub analysis"))

  ggsave(
    p,
    height = 7,
    width = 12,
    filename = file.path(
      fig_file_dir,
      glue::glue(
        "fig_heatmap_hub_metadata_{analysis_type}.png"
      )
    )
  )
}

#' Get the relative wis for the real time scores
#'
#' @param all_scores a tibble of the scores for both models in real-time
#'
#' @return A tibble of the relative wis at each forecast date, location, and
#' horizon day
#' @export
get_rel_wis_real_time <- function(all_scores) {
  full_metadata <- all_scores |>
    dplyr::filter(scale == "log") |>
    as.data.table() |>
    scoringutils::summarise_scores(
      by = c("forecast_date", "model", "location")
    ) |>
    dplyr::select(
      forecast_date,
      model,
      location,
      interval_score
    ) |>
    tidyr::pivot_wider(
      names_from = model,
      values_from = interval_score
    )
  # Find the date locations to exclude

  date_locs_to_exclude <- full_metadata |>
    dplyr::filter(is.na(ww)) |>
    dplyr::distinct(location, forecast_date)

  scores_filtered <- all_scores |>
    dplyr::anti_join(
      date_locs_to_exclude,
      by = c("location", "forecast_date")
    )

  rel_scores <- scores_filtered |>
    dplyr::filter(scale == "log") |>
    as.data.table() |>
    scoringutils::summarise_scores(
      by = c("forecast_date", "model", "date", "location")
    ) |>
    dplyr::select(
      location,
      forecast_date,
      date,
      model,
      interval_score
    ) |>
    tidyr::pivot_wider(
      names_from = model,
      values_from = interval_score
    ) |>
    dplyr::mutate(
      rel_wis = ww / hosp
    )

  return(rel_scores)
}

#' Get the relative wis from the hub formatted
#'
#' @param all_scores a tibble of the scores for both models, formatted
#' like the hub
#'
#' @return A tibble of the relative wis at each forecast date, location, and
#' horizon day
#' @export
get_rel_wis_all_time <- function(all_scores) {
  scores <- all_scores |>
    data.table::as.data.table() |>
    scoringutils::summarize_scores(
      by = c(
        "target_end_date",
        "model",
        "location",
        "forecast_date"
      )
    ) |>
    dplyr::rename(
      date = target_end_date
    ) |>
    dplyr::left_join(
      wweval::flusight_location_table,
      by = c("location" = "location_code")
    ) |>
    dplyr::select(
      short_name,
      forecast_date,
      date,
      model,
      interval_score
    ) |>
    dplyr::rename(location = short_name) |>
    dplyr::mutate(
      model = dplyr::case_when(
        model == "cfa-wwrenewal" ~ "ww",
        model == "cfa-hosponlyrenewal" ~ "hosp"
      )
    ) |>
    tidyr::pivot_wider(
      names_from = model,
      values_from = interval_score
    ) |>
    dplyr::mutate(
      rel_wis = ww / hosp
    ) |>
    dplyr::select(location, forecast_date, ww, hosp, rel_wis)

  return(scores)
}


#' Compare growth rates in wastewater and hospital admissions data
#'
#' @param input_hosp_data tibble of hospital admissions counts for a
#' particular location and forecast date
#' @param input_ww_data tibble of wastewater concentrations for a particular
#' location and forecast date
#' @param location string indicating the location of interest
#' @param forecast_date string indicating the forecast date of interest
#' @param rate string indicating whether the plot should be a daily or
#' weekly growth rates. Default is `"daily"`
#' @param align string indicating how to align the 7 day rolling average
#' of the hospital admissions and wastewater data. Default is `"center"`
#'
#' @return a ggplot object containing site-lab level exponential growth rates
#' with the state level hospital admissions growth rates overlaid
#' @export
get_growth_rate_plot <- function(
  input_hosp_data,
  input_ww_data,
  location,
  forecast_date,
  rate = "daily",
  align = "center"
) {
  hosp_data <- input_hosp_data |>
    dplyr::mutate(
      admits_7d_rolling = zoo::rollmean(
        daily_hosp_admits,
        k = 7,
        na.pad = TRUE,
        align = {{ align }}
      )
    )

  # linear interpolation of ww data
  ww_data <- input_ww_data |>
    dplyr::group_by(lab, site, lab_site_index) |>
    tidyr::complete(
      date = seq.Date(
        min(hosp_data$date),
        max(hosp_data$date),
        by = "day"
      )
    ) |>
    dplyr::mutate(
      ww_interpolated = zoo::na.approx(ww, na.rm = FALSE)
    ) |>
    dplyr::ungroup() |>
    # Get 7 day rolling average of interpolated data
    dplyr::mutate(
      ww_7d_rolling = zoo::rollmean(
        ww_interpolated,
        k = 7,
        na.pad = TRUE,
        align = {{ align }}
      )
    )
  # Put the two datasets together:
  comb_data <- ww_data |>
    dplyr::left_join(
      hosp_data,
      by = "date"
    ) |>
    dplyr::mutate(
      log_ww = log(ww_7d_rolling),
      log_hosp = log(admits_7d_rolling),
      prev_log_ww = dplyr::lag(log_ww, 1),
      prev_week_log_ww = dplyr::lag(log_ww, 7),
      prev_log_hosp = dplyr::lag(log_hosp, 1),
      prev_week_log_hosp = dplyr::lag(log_hosp, 7),
      daily_r_ww = log_ww - prev_log_ww,
      daily_r_hosp = log_hosp - prev_log_hosp,
      weekly_r_ww = (log_ww - prev_week_log_ww) / 7,
      weekly_r_hosp = (log_hosp - prev_week_log_hosp) / 7
    ) |>
    dplyr::mutate(
      lab_site_name = glue::glue("Site: {site}, Lab: {lab}")
    )

  daily_p <- ggplot(comb_data) +
    geom_line(aes(
      x = date,
      y = daily_r_ww,
      color = lab_site_name
    )) +
    geom_line(aes(x = date, y = daily_r_hosp)) +
    facet_wrap(~lab_site_name) +
    theme(
      legend.position = "bottom",
      panel.background = element_rect(fill = "white"),
      panel.grid.major = element_line(color = "gray"),
      axis.text.x = element_text(
        size = 8,
        vjust = 1,
        hjust = 1,
        angle = 45
      )
    ) +
    # coord_cartesian(ylim = c(-1, 1)) +
    ylab("Daily growth rate") +
    xlab("")
  ggtitle(glue::glue(
    "{location} {forecast_date} daily growth rate comparison"
  ))

  weekly_p <- ggplot(comb_data) +
    geom_line(aes(
      x = date,
      y = weekly_r_ww,
      color = lab_site_name
    )) +
    geom_line(aes(x = date, y = weekly_r_hosp)) +
    facet_wrap(~lab_site_name) +
    theme(
      legend.position = "bottom",
      panel.background = element_rect(fill = "white"),
      panel.grid.major = element_line(color = "gray"),
      axis.text.x = element_text(
        size = 8,
        vjust = 1,
        hjust = 1,
        angle = 45
      )
    ) +
    xlab("") +
    # coord_cartesian(ylim = c(-0.5, 0.5)) +
    ylab("Weekly growth rate") +
    ggtitle(glue::glue(
      "{location} {forecast_date} weekly growth rate comparison"
    ))

  if (rate == "weekly") {
    p <- weekly_p
  } else {
    p <- daily_p
  }
  return(p)
}


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
get_plot_ww_data_comparison <- function(
  draws_w_data,
  location,
  model_type = "ww",
  n_draws = 100
) {
  sampled_draws <- sample(1:max(draws_w_data$draw), n_draws)
  draws_w_data_subsetted <- draws_w_data |>
    dplyr::filter(
      draw %in% !!sampled_draws,
      name == "pred_ww"
    )

  p <- ggplot(draws_w_data_subsetted) +
    geom_line(
      aes(
        x = .data$date,
        y = .data$value,
        group = .data$draw,
        color = .data$site_lab_name
      ),
      linewidth = 0.1,
      alpha = 0.1,
      show.legend = FALSE
    ) +
    geom_point(
      aes(x = .data$date, y = .data$eval_data),
      fill = "white",
      size = 1,
      shape = 21,
      show.legend = FALSE
    ) +
    geom_point(
      aes(x = .data$date, y = .data$calib_data),
      color = "black",
      show.legend = FALSE
    ) +
    geom_vline(
      aes(xintercept = lubridate::ymd(.data$forecast_date)),
      linetype = "dashed"
    ) +
    scale_y_continuous(trans = "log10") +
    facet_wrap(~site_lab_name, scales = "free_y") +
    geom_point(
      data = draws_w_data_subsetted |> filter(.data$below_LOD == 1),
      aes(x = .data$date, y = .data$calib_data),
      color = "red",
      size = 1.1
    ) +
    geom_point(
      data = draws_w_data_subsetted |>
        filter(.data$flag_as_ww_outlier == 1),
      aes(x = .data$date, y = .data$calib_data),
      color = "blue",
      size = 1.1
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
        size = 8,
        vjust = 1,
        hjust = 1,
        angle = 45
      ),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      plot.title = element_text(
        size = 10,
        vjust = 0.5,
        hjust = 0.5
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
get_plot_hosp_data_comparison <- function(
  draws_w_data,
  location,
  model_type = c("ww", "hosp"),
  n_draws = 100
) {
  model_type <- arg_match(model_type)
  sampled_draws <- sample(1:max(draws_w_data$draw), n_draws)
  draws_w_data_subsetted <- draws_w_data |>
    dplyr::filter(
      draw %in% !!sampled_draws,
      name == "pred_hosp"
    )

  plot_color <- ifelse(model_type == "ww", "cornflowerblue", "purple4")

  p <- ggplot(draws_w_data_subsetted) +
    geom_line(
      aes(x = date, y = value, group = draw),
      color = plot_color,
      linewidth = 0.2,
      alpha = 0.4,
      show.legend = FALSE
    ) +
    geom_point(
      aes(x = date, y = eval_data),
      fill = "white",
      size = 1,
      shape = 21,
      show.legend = FALSE
    ) +
    geom_point(
      aes(x = date, y = calib_data),
      color = "black",
      show.legend = FALSE
    ) +
    geom_vline(
      aes(xintercept = lubridate::ymd(forecast_date)),
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
        size = 8,
        vjust = 1,
        hjust = 1,
        angle = 45
      ),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      plot.title = element_text(
        size = 10,
        vjust = 0.5,
        hjust = 0.5
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
get_plot_quantile_comparison <- function(
  hosp_quantiles,
  eval_data,
  figure_file_path,
  days_to_show_forecast = 28,
  save_files = TRUE
) {
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
      date >=
        min(hosp_quantiles$date[
          hosp_quantiles$period == "nowcast"
        ])
    )

  quantiles_wide <- hosp_quantiles |>
    dplyr::filter(period != "calibration") |>
    dplyr::filter(quantile %in% c(0.025, 0.25, 0.5, 0.75, 0.975)) |>
    tidyr::pivot_wider(
      id_cols = c(
        forecast_date,
        period,
        scenario,
        date,
        eval_data
      ),
      names_from = quantile,
      values_from = value
    )

  p <- ggplot(quantiles_wide) +
    geom_point(
      data = eval_data_subsetted,
      aes(x = date, y = daily_hosp_admits),
      color = "black",
      alpha = 0.3
    ) +
    geom_line(
      data = eval_data_subsetted,
      aes(x = date, y = daily_hosp_admits),
      color = "black",
      alpha = 0.3
    ) +
    geom_line(
      data = quantiles_wide |>
        filter(
          date >= forecast_date,
          date <=
            forecast_date +
              days_to_show_forecast
        ),
      aes(
        x = date,
        y = `0.5`,
        group = forecast_date,
        color = scenario
      )
    ) +
    geom_ribbon(
      data = quantiles_wide |>
        filter(
          date >= forecast_date,
          date <=
            forecast_date +
              days_to_show_forecast
        ),
      aes(
        x = date,
        ymin = `0.025`,
        ymax = `0.975`,
        fill = scenario,
        group = forecast_date
      ),
      alpha = 0.1,
      show.legend = FALSE
    ) +
    geom_ribbon(
      data = quantiles_wide |>
        filter(
          date >= forecast_date,
          date <=
            forecast_date +
              days_to_show_forecast
        ),
      aes(
        x = date,
        ymin = `0.25`,
        ymax = `0.75`,
        fill = scenario,
        group = forecast_date
      ),
      alpha = 0.1,
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
        size = 8,
        vjust = 1,
        hjust = 1,
        angle = 45
      ),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 10),
      plot.title = element_text(
        size = 10,
        vjust = 0.5,
        hjust = 0.5
      )
    )

  if (isTRUE(save_files)) {
    full_file_path <- file.path(
      figure_file_path,
      "quantile_comparison"
    )
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


get_plot_ww_comparison <- function(ww_quantiles, days_to_show_forecast = 28) {
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
        forecast_date,
        period,
        scenario,
        date,
        t,
        eval_data,
        site_lab_name
      ),
      names_from = quantile,
      values_from = value
    )

  p <- ggplot(quantiles_wide) +
    geom_point(
      data = quantiles_wide |>
        filter(
          date >= forecast_date,
          date <=
            forecast_date +
              days_to_show_forecast
        ),
      aes(x = date, y = eval_data),
      color = "black"
    ) +
    geom_line(
      data = quantiles_wide |>
        filter(
          date >= forecast_date,
          date <=
            forecast_date +
              days_to_show_forecast
        ),
      aes(
        x = date,
        y = `0.5`,
        group = forecast_date,
        color = site_lab_name
      ),
      show.legend = FALSE
    ) +
    geom_ribbon(
      data = quantiles_wide |>
        filter(
          date >= forecast_date,
          date <=
            forecast_date +
              days_to_show_forecast
        ),
      aes(
        x = date,
        ymin = `0.025`,
        ymax = `0.975`,
        fill = site_lab_name,
        group = forecast_date
      ),
      alpha = 0.1,
      show.legend = FALSE
    ) +
    geom_ribbon(
      data = quantiles_wide |>
        filter(
          date >= forecast_date,
          date <=
            forecast_date +
              days_to_show_forecast
        ),
      aes(
        x = date,
        ymin = `0.25`,
        ymax = `0.75`,
        fill = site_lab_name,
        group = forecast_date
      ),
      alpha = 0.1,
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
        size = 8,
        vjust = 1,
        hjust = 1,
        angle = 45
      ),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 10),
      plot.title = element_text(
        size = 8,
        vjust = 0.5,
        hjust = 0.5
      )
    )

  return(p)
}

#' Barplot of scores by scenario
#'
#' @param scenario_scores data frame of scenario scores to plot.
#' @param score_metric Metric to plot. Must be a column name in
#' `scenario_scores`. Default `"crps"`.
#' @return The plot, as a ggplot object.
#' @export
plot_scores_by_scenario <- function(scenario_scores, score_metric = "crps") {
  p <- ggplot(scenario_scores) +
    geom_bar(
      aes(
        x = .data$scenario,
        y = .data[[score_metric]],
        fill = .data$scenario
      ),
      position = "dodge",
      stat = "identity"
    ) +
    theme_bw() +
    ylab(glue::glue(
      "{score_metric} across forecast dates and locations"
    ))
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
get_plot_raw_scores <- function(all_scores, score_metric = "crps") {
  p <- ggplot(all_scores) +
    geom_point(aes(
      x = date,
      y = .data[[score_metric]],
      color = scenario,
      group = c(forecast_date)
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
get_plot_scores_w_data <- function(
  all_scores,
  eval_data,
  figure_file_path,
  score_metric = "crps",
  save_files = TRUE
) {
  location <- all_scores |>
    pull(location) |>
    unique()

  eval_data_subsetted <- eval_data |>
    dplyr::filter(
      location == !!location,
      date <= max(all_scores$date),
      date >=
        min(all_scores$forecast_date) -
          lubridate::days(10)
    )
  coeff <- (median(eval_data_subsetted$daily_hosp_admits, na.rm = TRUE) /
    median(
      all_scores |>
        dplyr::select({{ score_metric }}) |> # nolint
        dplyr::pull(),
      na.rm = TRUE
    ))

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

  coeff <- (median(eval_data_subsetted$daily_hosp_admits, na.rm = TRUE) /
    median(
      summarized_scores |>
        dplyr::select({{ score_metric }}) |> # nolint
        dplyr::pull(),
      na.rm = TRUE
    ))

  p <- ggplot(summarized_scores) +
    geom_bar(
      aes(
        x = forecast_date,
        y = .data[[score_metric]] * coeff,
        fill = scenario
      ),
      stat = "identity",
      position = "dodge",
      alpha = 0.5
    ) +
    geom_point(
      data = eval_data_subsetted,
      aes(x = date, y = daily_hosp_admits),
      color = "black",
      alpha = 0.3
    ) +
    geom_line(
      data = eval_data_subsetted,
      aes(x = date, y = daily_hosp_admits),
      color = "black",
      alpha = 0.3
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
        name = glue::glue(
          "{score_metric} by forecast date and scenario"
        )
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
        size = 8,
        vjust = 1,
        hjust = 1,
        angle = 45
      ),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 10),
      plot.title = element_text(
        size = 10,
        vjust = 0.5,
        hjust = 0.5
      )
    )

  if (isTRUE(save_files)) {
    full_file_path <- file.path(
      figure_file_path,
      "scores_w_data_overlaid"
    )
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
get_plot_summarized_scores <- function(all_scores, score_metric = "crps") {
  scores <- all_scores |>
    dplyr::select(-tar_group) |>
    data.table::as.data.table()

  summarized_scores <- all_scores |>
    mutate(forecast_date = lubridate::ymd(forecast_date)) |>
    data.table::as.data.table() |>
    scoringutils::summarize_scores(
      by = c(
        "scenario",
        "period",
        "forecast_date",
        "location"
      )
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
    geom_bar(
      aes(
        x = forecast_date,
        y = .data[[score_metric]],
        fill = scenario
      ),
      stat = "identity",
      position = "dodge",
      alpha = 0.5
    ) +
    geom_hline(
      data = summary_across_dates,
      aes(
        yintercept = .data[[score_metric]],
        color = scenario
      )
    ) +
    facet_wrap(~period, nrow = n_periods) +
    theme_bw() +
    xlab("") +
    ylab(glue::glue(
      "{score_metric} by forecast date and scenario"
    )) +
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
        size = 8,
        vjust = 1,
        hjust = 1,
        angle = 45
      ),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      plot.title = element_text(
        size = 10,
        vjust = 0.5,
        hjust = 0.5
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
get_heatmap_scores <- function(mock_submission_scores, score_metric = "crps") {
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
        size = 8,
        vjust = 1,
        hjust = 1,
        angle = 45
      ),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      plot.title = element_text(
        size = 10,
        vjust = 0.5,
        hjust = 0.5
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
get_box_plot <- function(
  all_scores,
  figure_file_path,
  baseline_scenario = "no_wastewater",
  save_files = TRUE
) {
  scores_by_date_scen_loc <- scoringutils::summarize_scores(
    all_scores,
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
    dplyr::left_join(
      baseline_only,
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
      x = factor(forecast_date),
      y = diff_in_score,
      fill = scenario,
      color = scenario
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
        size = 8,
        vjust = 1,
        hjust = 1,
        angle = 45
      ),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 10),
      plot.title = element_text(
        size = 10,
        vjust = 0.5,
        hjust = 0.5
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
get_n_states_improved_plot <- function(
  all_scores,
  figure_file_path,
  baseline_scenario = "no_wastewater",
  threshold_for_improvement = 1,
  save_files = TRUE
) {
  scores_by_date_scen_loc <- scoringutils::summarize_scores(
    all_scores,
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
    dplyr::left_join(
      baseline_only,
      by = c("location", "forecast_date")
    ) |>
    group_by(location, forecast_date, scenario) |>
    dplyr::summarize(
      relative_score = crps / baseline_score,
      diff_in_score = baseline_score - crps,
      mean_crps = mean(crps)
    ) |>
    dplyr::mutate(
      is_improved = ifelse(
        relative_score < threshold_for_improvement,
        1,
        0
      )
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
        size = 8,
        vjust = 1,
        hjust = 1,
        angle = 45
      ),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 10),
      plot.title = element_text(
        size = 10,
        vjust = 0.5,
        hjust = 0.5
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


#' Plot wastewater evaluation data
#'
#' @param eval_data Data frame of evaluation data
#' @return The plot, as a ggplot object.
#'
#' @export
get_plot_ww_data <- function(eval_data) {
  eval_data <- eval_data |>
    dplyr::mutate(
      lab_site_name = glue::glue(
        "Site: {.data$site}, lab: {.data$lab}"
      ),
      ww = exp(.data$log_genome_copies_per_ml)
    )

  loc <- eval_data |>
    dplyr::distinct(.data$location) |>
    dplyr::pull()

  p <- ggplot(eval_data) +
    geom_point(aes(x = .data$date, y = .data$ww), size = 0.5) +
    geom_line(aes(x = .data$date, y = .data$ww), size = 0.5) +
    geom_point(
      data = eval_data |>
        dplyr::filter(.data$flag_as_ww_outlier == 1),
      aes(x = .data$date, y = .data$ww),
      fill = "red",
      color = "red",
      size = 0.5
    ) +
    geom_point(
      data = eval_data |> dplyr::filter(.data$below_lod == 1),
      aes(x = .data$date, y = .data$ww),
      fill = "darkblue",
      color = "darkblue",
      size = 0.5
    ) +
    facet_wrap(~lab_site_name, scales = "free_y") +
    theme_bw() +
    theme(
      axis.text.x = element_text(
        size = 8,
        vjust = 1,
        hjust = 1,
        angle = 45
      ),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 10),
      plot.title = element_text(
        size = 10,
        vjust = 0.5,
        hjust = 0.5
      )
    ) +
    scale_x_date() +
    scale_y_continuous(transform = "log10") +
    xlab("") +
    ylab("Log(genome copies per mL)") +
    ggtitle(glue::glue("Wastewater concentration data in {loc}"))

  return(p)
}


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
plot_total_admissions <- function(
  eval_hosp_data,
  first_forecast_date,
  last_forecast_date
) {
  hosp_data <- eval_hosp_data |>
    dplyr::distinct(
      .data$location,
      .data$daily_hosp_admits,
      .data$date
    ) |>
    dplyr::group_by(.data$date) |>
    dplyr::summarise(total_hosp = sum(daily_hosp_admits))

  max_total_hosp <- max(hosp_data$total_hosp)

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

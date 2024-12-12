get_plot_scores_and_forecasts <- function(scores_single_loc_date,
                                          eval_output_subdir,
                                          n_calib_days = 10) {
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
          location, forecast_date, period, scenario,
          date, eval_data, calib_data, model_type
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
      geom_line(aes(x = date, y = crps, color = model),
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
      geom_vline(aes(xintercept = forecast_date), linetype = "dashed") +
      scale_color_manual(values = colors$model_colors)

    p_scores_avg <- ggplot(scores_avg) +
      geom_bar(aes(x = model, y = avg_crps, fill = model),
        stat = "identity", position = "dodge",
        show.legend = FALSE
      ) +
      get_plot_theme(
        x_axis_dates = FALSE
      ) +
      xlab("Model") +
      ylab("Mean CRPS") +
      scale_fill_manual(values = colors$model_colors)

    p_forecasts <- ggplot(quantiles_wide_forecast) +
      geom_point(aes(x = date, y = eval_data),
        fill = "white", size = 1, shape = 21,
        show.legend = FALSE
      ) +
      geom_point(
        aes(x = date, y = calib_data),
        color = "black", show.legend = FALSE
      ) +
      geom_line(
        aes(
          x = date, y = `0.5`,
          color = model_type
        )
      ) +
      geom_ribbon(
        aes(
          x = date, ymin = `0.025`, ymax = `0.975`,
          fill = model_type
        ),
        alpha = 0.1
      ) +
      geom_ribbon(
        aes(
          x = date, ymin = `0.25`, ymax = `0.75`,
          fill = model_type
        ),
        alpha = 0.2,
      ) +
      geom_vline(aes(xintercept = lubridate::ymd(this_forecast_date)),
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

    fig <- p_forecasts + p_scores_t + p_scores_avg +
      patchwork::plot_layout(
        guides = "collect",
        nrow = 3, ncol = 1,
        axes = "collect",
        widths = c(1, 1.5)
      ) & theme(
      legend.position = "top",
      legend.justification = "left"
    )

    fig_file_dir <- file.path(
      eval_output_subdir, "status_quo",
      this_forecast_date, "ww", this_location
    )
    alt_fig_file_dir <- file.path(
      eval_output_subdir, "status_quo",
      this_location
    )
    fs::dir_create(alt_fig_file_dir)

    ggsave(fig,
      filename = file.path(fig_file_dir, "forecast_and_score_comp_fig.png"),
      width = 7, height = 10
    )

    ggsave(fig,
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
    geom_point(aes(x = date, y = eval_data),
      fill = "white", size = 1, shape = 21,
      show.legend = FALSE
    ) +
    geom_point(
      aes(x = date, y = calib_data),
      color = "black", show.legend = FALSE
    ) +
    geom_line(
      aes(
        x = date, y = `0.5`,
        color = model_type
      )
    ) +
    geom_ribbon(
      aes(
        x = date, ymin = `0.025`, ymax = `0.975`,
        fill = model_type
      ),
      alpha = 0.1
    ) +
    geom_ribbon(
      aes(
        x = date, ymin = `0.25`, ymax = `0.75`,
        fill = model_type
      ),
      alpha = 0.2,
    ) +
    geom_vline(aes(xintercept = lubridate::ymd(this_forecast_date)),
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

  ggsave(p_forecasts_all,
    filename = file.path(fig_file_dir, "calib_and_forecasts.png"),
    width = 7, height = 10
  )

  ggsave(p_forecasts_all,
    filename = file.path(
      alt_fig_file_dir,
      glue::glue("calib_and_forecasts_{this_forecast_date}.png")
    )
  )
}

get_plot_wis_t <- function(hosp_quantiles,
                           scores,
                           eval_output_subdir,
                           submissions_path = "https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/master/data-processed/", # nolint
                           truth_data_path = "https://media.githubusercontent.com/media/reichlab/covid19-forecast-hub/master/data-truth/truth-Incident%20Hospitalizations.csv", # nolint
                           hub_comparison_model = "COVIDhub-4_week_ensemble") {
  truth_data <- readr::read_csv(truth_data_path)

  this_location <- hosp_quantiles |>
    dplyr::distinct(location) |>
    dplyr::pull()

  loc_code <- wweval::loc_abbr_to_flusight_code(this_location)

  this_forecast_date <- hosp_quantiles |>
    dplyr::distinct(forecast_date) |>
    dplyr::pull()
  scores <- scores |> dplyr::filter(
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
        location, forecast_date, target_end_date
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
    geom_point(aes(x = target_end_date, y = truth),
      fill = "white", size = 1, shape = 21,
      show.legend = FALSE
    ) +
    geom_line(
      aes(
        x = target_end_date, y = `0.5`
      ),
      color = "gray"
    ) +
    geom_ribbon(
      aes(
        x = target_end_date, ymin = `0.025`, ymax = `0.975`
      ),
      fill = "gray",
      alpha = 0.1
    ) +
    geom_ribbon(
      aes(
        x = target_end_date, ymin = `0.25`, ymax = `0.75`
      ),
      fill = "gray",
      alpha = 0.2,
    ) +
    geom_vline(aes(xintercept = lubridate::ymd(this_forecast_date)),
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
    coord_cartesian(ylim = c(0, 2 * max(hosp_quantiles$eval_data))) +
    ggtitle(glue::glue("{this_forecast_date} in {this_location} {hub_comparison_model}"))


  quantiles_wide <- hosp_quantiles |>
    dplyr::filter(
      quantile %in% c(0.025, 0.25, 0.5, 0.75, 0.975),
      date >= this_forecast_date
    ) |>
    tidyr::pivot_wider(
      id_cols = c(
        location, forecast_date, period, scenario,
        date, eval_data, calib_data, model_type
      ),
      names_from = quantile,
      values_from = value
    )
  colors <- plot_components()
  ## First plot the forecasts from renewal models
  p_forecasts <- ggplot(quantiles_wide) +
    geom_point(aes(x = date, y = eval_data),
      fill = "white", size = 1, shape = 21,
      show.legend = FALSE
    ) +
    geom_point(
      aes(x = date, y = calib_data),
      color = "black", show.legend = FALSE
    ) +
    geom_line(
      aes(
        x = date, y = `0.5`,
        color = model_type
      ),
      show.legend = FALSE
    ) +
    geom_ribbon(
      aes(
        x = date, ymin = `0.025`, ymax = `0.975`,
        fill = model_type
      ),
      alpha = 0.1,
      show.legend = FALSE
    ) +
    geom_ribbon(
      aes(
        x = date, ymin = `0.25`, ymax = `0.75`,
        fill = model_type
      ),
      alpha = 0.2,
      show.legend = FALSE
    ) +
    geom_vline(aes(xintercept = lubridate::ymd(this_forecast_date)),
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
    coord_cartesian(ylim = c(0, 2 * max(hosp_quantiles$eval_data))) +
    ggtitle(glue::glue("{this_forecast_date} in {this_location}"))

  scores_to_plot <- scores |>
    dplyr::filter(model %in% c(
      "cfa-wwrenewal", "cfa-hosponlyrenewal", {{ hub_comparison_model }}
    )) |>
    dplyr::group_by(model, target_end_date) |>
    dplyr::summarize(avg_wis = mean(interval_score)) |>
    dplyr::mutate(
      model =
        dplyr::case_when(
          model == "cfa-wwrenewal" ~ "cfa-wwrenewal(retro)",
          model == "cfa-hosponlyrenewal" ~ "cfa-hosponlyrenewal(retro)",
          TRUE ~ model
        )
    )

  avg_scores <- scores |>
    dplyr::filter(model %in% c(
      "cfa-wwrenewal", "cfa-hosponlyrenewal", {{ hub_comparison_model }}
    )) |>
    dplyr::group_by(model) |>
    dplyr::summarize(avg_wis = mean(interval_score)) |>
    dplyr::mutate(model = dplyr::case_when(
      model == "cfa-wwrenewal" ~ "cfa-wwrenewal(retro)",
      model == "cfa-hosponlyrenewal" ~ "cfa-hosponlyrenewal(retro)",
      TRUE ~ model
    ))

  scores_t <- ggplot(scores_to_plot) +
    geom_line(aes(x = target_end_date, y = avg_wis, color = model)) +
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
    geom_bar(aes(x = model, y = avg_wis, fill = model),
      show.legend = FALSE,
      stat = "identity", position = "dodge"
    ) +
    get_plot_theme() +
    scale_fill_manual(values = colors$model_colors) +
    xlab(NULL) +
    ylab("WIS")

  fig <- p_forecasts + p_hub_forecasts + scores_t + scores_bar +
    patchwork::plot_layout(
      guides = "collect",
      nrow = 4, ncol = 1,
      axes = "collect",
      widths = c(1, 1.5)
    ) & theme(
    legend.position = "top",
    legend.justification = "left"
  )

  fig_file_dir <- file.path(
    eval_output_subdir, "status_quo",
    this_forecast_date, "ww", this_location
  )
  alt_fig_file_dir <- file.path(
    eval_output_subdir, "status_quo",
    this_location
  )
  fs::dir_create(alt_fig_file_dir)
  fs::dir_create(fig_file_dir)

  ggsave(fig,
    filename = file.path(fig_file_dir, "hub_comparison_fig.png"),
    width = 7, height = 11
  )

  ggsave(fig,
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
get_plot_bias_over_time <- function(scores,
                                    fig_subscript,
                                    fig_file_dir) {
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

  ggsave(p,
    width = 10, height = 5,
    filename = file.path(
      fig_file_dir,
      glue::glue("sfig_bias_over_time_{fig_subscript}.svg")
    )
  )
  ggsave(p,
    width = 10, height = 5,
    filename = file.path(
      fig_file_dir,
      glue::glue("sfig_bias_over_time_{fig_subscript}.png")
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
get_plot_score_by_horizon_t <- function(scores,
                                        score_type,
                                        fig_file_dir) {
  scores_by_horizon_and_t <- scores |>
    # hack, will fix upstream
    dplyr::filter(!horizon %in% c("0 week ahead", "5 week ahead")) |>
    dplyr::group_by(horizon, forecast_date, model) |>
    dplyr::filter(!is.na(horizon)) |>
    dplyr::summarize(
      mean_score = mean(!!sym(score_type))
    )

  colors <- plot_components()

  p <- ggplot(scores_by_horizon_and_t) +
    geom_line(aes(x = forecast_date, y = mean_score, color = model)) +
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
    ggtitle(glue::glue("Average {score_type} over time, across locations"))

  fs::dir_create(fig_file_dir)
  ggsave(p,
    width = 10, height = 10,
    filename = file.path(
      fig_file_dir,
      glue::glue("sfig_{score_type}_over_time_by_horizon.svg")
    )
  )
  ggsave(p,
    width = 10, height = 10,
    filename = file.path(
      fig_file_dir,
      glue::glue("sfig_{score_type}_over_time_by_horizon.png")
    )
  )
  return(p)
}


#' Get avg of scores by horizon
#'
#' @param scores tibble of scores for every location, forecast date, and horizon
#' @param score_type string indicating the type of score to summarize over
#'
#' @return table of avg scores by horizon and overall by model
#' @export
get_avg_scores_model_horizon <- function(scores,
                                         score_type) {
  avg_scores <- scores |>
    dplyr::filter(
      !is.na(horizon)
    ) |>
    # hack, will fix upstream
    bind_rows(
      scores |>
        dplyr::mutate(horizon = "overall")
    ) |>
    dplyr::group_by(model, horizon) |>
    dplyr::summarize(
      avg_score = mean(!!sym({{ score_type }}))
    ) |>
    tidyr::pivot_wider(names_from = model, values_from = avg_score)

  return(avg_scores)
}


#' Get stats on number of improved forecasts
#'
#' @param scores tibble of scores for every location, forecast date, and horizon
#' @param threshold numeric between 0 and 1 indicating the threshold to call
#' something better or worse
#'
#' @return table of the number of states with improvements, number of overall
#' forecasts with improvements, number that got worse, etc.
#' @export
get_stats_improved_forecasts <- function(scores,
                                         threshold) {
  relative_crps_by_loc <- scores |>
    dplyr::group_by(location, model) |>
    dplyr::summarize(crps = mean(crps)) |>
    compute_relative_crps(id_cols = c(
      "location"
    )) |>
    ungroup() |>
    dplyr::mutate(
      pct_change_crps = (ww - hosp) / hosp
    )

  n_states_better <- relative_crps_by_loc |>
    dplyr::filter(pct_change_crps < threshold) |>
    nrow()

  n_states_worse <- relative_crps_by_loc |>
    dplyr::filter(pct_change_crps > threshold) |>
    nrow()

  relative_crps_by_forecast <- scores |>
    dplyr::group_by(location, model, forecast_date) |>
    dplyr::summarize(crps = mean(crps)) |>
    compute_relative_crps(id_cols = c(
      "location", "forecast_date"
    )) |>
    dplyr::mutate(
      pct_change_crps = (ww - hosp) / hosp
    )

  relative_crps_raw <- scores |>
    compute_relative_crps(id_cols = c(
      "location", "forecast_date", "date"
    )) |>
    dplyr::mutate(
      pct_change_crps = (ww - hosp) / hosp
    )

  ggplot(relative_crps_by_forecast) +
    geom_histogram(aes(x = pct_change_crps))

  ggplot(relative_crps_raw) +
    geom_histogram(aes(x = pct_change_crps))
  ggplot(relative_crps_raw) +
    geom_histogram(aes(x = rel_crps)) +
    scale_x_continuous(trans = "log10")




  forecasts_way_worse <- relative_crps_by_forecast |>
    dplyr::filter(pct_change_crps > 3)
  n_forecasts_3x_worse <- forecasts_way_worse |> nrow()

  forecasts_way_better <- relative_crps_by_forecast |>
    dplyr::filter(pct_change_crps < -3)
  n_forecasts_3x_better <- forecasts_way_better |> nrow()

  n_forecasts_better <- relative_crps_by_forecast |>
    dplyr::filter(pct_change_crps < 0) |>
    nrow()

  n_forecasts_worse <- relative_crps_by_forecast |>
    dplyr::filter(pct_change_crps > 0) |>
    nrow()

  n_forecasts_better_thres <- relative_crps_by_forecast |>
    dplyr::filter(
      pct_change_crps < 0,
      abs(pct_change_crps) > threshold
    ) |>
    nrow()

  n_forecasts_worse <- relative_crps_by_forecast |>
    dplyr::filter(pct_change_crps > 0) |>
    nrow()

  n_forecasts_worse_thres <- relative_crps_by_forecast |>
    dplyr::filter(
      pct_change_crps > 0,
      abs(pct_change_crps) > threshold
    ) |>
    nrow()




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



get_plot_sites_vs_performance <- function(scores,
                                          ww_metadata,
                                          fig_file_dir) {
  scores_summarized <- scores |>
    dplyr::group_by(location, forecast_date) |>
    dplyr::summarise(avg_crps = mean(crps))

  scores_joined <- scores_summarized |>
    dplyr::left_join(ww_metadata,
      by = c("forecast_date", "location")
    )

  p_n_sites <- ggplot(scores_joined) +
    geom_point(aes(x = n_sites, y = avg_crps))

  p_coverage <- ggplot(scores_joined) +
    geom_point(aes(x = pop_coverage, y = avg_crps))

  ggsave(p_n_sites,
    filename = file.path(
      fig_file_dir,
      glue::glue("sfig_n_sites_vs_crps.png")
    )
  )

  ggsave(p_coverage,
    filename = file.path(
      fig_file_dir,
      glue::glue("sfig_pop_coverage_vs_crps.png")
    )
  )

  return(p_n_sites)
}
#' Plot a heatmap of the avg forecast performance by locations and forecast date
#' for the Hub submissions
#'
#' @param scores A tibble of daily scores by forecast date, location, and model
#' @param fig_file_dir A string indicating the directory to save the figures in
#'
#' @return a ggplot object
#' @export
get_plot_hub_perf_heatmap <- function(scores,
                                      fig_file_dir) {
  scores_summary <- scores |>
    dplyr::filter(
      model %in% c("cfa-wwrenewal(retro)", "cfa-hosponlyrenewal(retro)")
    ) |>
    dplyr::group_by(forecast_date, location_name, model) |>
    dplyr::summarise(avg_wis = mean(interval_score))

  p <- ggplot(scores_summary) +
    geom_tile(aes(x = forecast_date, y = location_name, fill = avg_wis)) +
    scale_fill_gradient2(
      high = "red", mid = "white", low = "blue",
      midpoint = mean(scores_summary$avg_wis),
      guide = "colourbar", aesthetics = "fill"
    ) +
    geom_text(aes(
      x = forecast_date, y = location_name,
      label = round(avg_wis, 2)
    ), size = 1.5) +
    facet_wrap(~model) +
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
    labs(fill = "Avg WIS") +
    ggtitle(glue::glue("Average WIS by forecast date and location"))

  ggsave(p,
    width = 10, height = 6,
    filename = file.path(
      fig_file_dir,
      glue::glue("sfig_heatmap_wis.png")
    )
  )
  return(p)
}

#' Plot a heatmap of the avg forecast performance by locations and forecast date
#' for the head-to-head comparison
#'
#' @param scores A tibble of daily scores by forecast date, location, and model
#' @param fig_file_dir A string indicating the directory to save the figures in
#'
#' @return a ggplot object
#' @export
get_plot_comb_perf_heatmap <- function(scores,
                                       fig_file_dir) {
  scores_summary <- scores |>
    dplyr::group_by(forecast_date, location, model) |>
    dplyr::summarise(avg_crps = mean(crps))

  p <- ggplot(scores_summary) +
    geom_tile(aes(x = forecast_date, y = location, fill = avg_crps)) +
    scale_fill_gradient2(
      high = "red", mid = "white", low = "blue",
      midpoint = mean(scores_summary$avg_crps),
      guide = "colourbar", aesthetics = "fill"
    ) +
    geom_text(aes(
      x = forecast_date, y = location,
      label = round(avg_crps, 2)
    ), size = 1.5) +
    facet_wrap(~model) +
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
    labs(fill = "Avg CRPS") +
    ggtitle(glue::glue("Average CRPS by forecast date and location"))

  ggsave(p,
    width = 10, height = 6,
    filename = file.path(
      fig_file_dir,
      glue::glue("sfig_heatmap_crps.png")
    )
  )
  return(p)
}


#' Get a summary table of the number of forecasts excluded for each reason
#'
#' @param metadata a tibble containing metadata for each forecast date location
#'
#' @return a 1 row tibble with the number of forecasts for each category
#' @export
get_summary_metadata <- function(metadata) {
  metadata_summarized <- metadata |>
    dplyr::select(
      forecast_date, location, ww_data_present,
      ww_sufficient,
      any_flags_hosp, any_flags_ww
    )

  metadata_remove_insuff_ww <- metadata_summarized |>
    dplyr::filter(ww_data_present == 1, ww_sufficient == TRUE)

  n_insuff_ww <- nrow(metadata_summarized) - nrow(metadata_remove_insuff_ww)

  metadata_remove_conv_issues <- metadata_remove_insuff_ww |>
    dplyr::filter(any_flags_hosp == FALSE, any_flags_ww == FALSE)

  n_conv_issues <- nrow(metadata_remove_insuff_ww) - nrow(metadata_remove_conv_issues)

  summary_table <- tibble::tibble(n_insuff_ww, n_conv_issues,
    n_forecasts = nrow(metadata_remove_conv_issues)
  )

  return(summary_table)
}

#' Get a heatmap of the metadata of reasons for excluding forecasts from analysis
#'
#' @param metadata a tibble of location -forecast date metadata
#' @param type_of_analysis either "retro_comparison" or "hub_comparison"
#' @param fig_file_dir string indicating where to save figs
#'
#' @return a ggplot object with a heatmap colored by reason for excluding
#' @export
get_heatmap_metadata <- function(metadata,
                                 type_of_analysis,
                                 fig_file_dir) {
  metadata_summarized <- metadata |>
    dplyr::select(
      forecast_date, location, ww_data_present,
      ww_sufficient,
      any_flags_hosp, any_flags_ww
    ) |>
    dplyr::ungroup()

  if (type_of_analysis == "retro_comparison") {
    metadata_final <- metadata_summarized |>
      dplyr::mutate(
        metadata_cat =
          case_when(
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
        metadata_cat =
          case_when(
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
    geom_tile(aes(x = forecast_date, y = location, fill = metadata_cat)) +
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
    ggtitle(glue::glue("Summary of retrospective comparison analysis"))

  ggsave(p,
    filename = file.path(
      fig_file_dir,
      glue::glue("sfig_heatmap_metadata.png")
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
get_heatmap_metadata_hub <- function(metadata,
                                     analysis_type,
                                     fig_file_dir) {
  if (analysis_type == "retro") {
    metadata_summarized <- metadata |>
      dplyr::select(
        forecast_date, location, ww_data_present,
        ww_sufficient,
        any_flags_hosp, any_flags_ww
      ) |>
      dplyr::ungroup() |>
      dplyr::mutate(
        model_submitted =
          dplyr::case_when(
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
      metadata_summarized, metadata_hosp_only
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
    metadata_grid <- expand.grid(location = locs, forecast_date = dates)
    metadata_ww <- metadata_grid |>
      dplyr::left_join(
        df_replacements
      ) |>
      dplyr::mutate(
        model_submitted = ifelse(is.na(model_submitted), "ww", "hosp"),
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
    geom_tile(aes(x = forecast_date, y = location, fill = model_submitted)) +
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

  ggsave(p,
    height = 7, width = 12,
    filename = file.path(
      fig_file_dir,
      glue::glue("sfig_heatmap_hub_metadata_{analysis_type}.png")
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
      forecast_date, model, location,
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
    dplyr::anti_join(date_locs_to_exclude,
      by = c("location", "forecast_date")
    )

  rel_scores <- scores_filtered |>
    dplyr::filter(scale == "log") |>
    as.data.table() |>
    scoringutils::summarise_scores(
      by = c("forecast_date", "model", "date", "location")
    ) |>
    dplyr::select(location, forecast_date, date, model, interval_score) |>
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
      by =
        c(
          "target_end_date",
          "model",
          "location",
          "forecast_date"
        )
    ) |>
    dplyr::rename(
      date = target_end_date
    ) |>
    dplyr::left_join(wweval::flusight_location_table,
      by = c("location" = "location_code")
    ) |>
    dplyr::select(
      short_name, forecast_date, date,
      model, interval_score
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

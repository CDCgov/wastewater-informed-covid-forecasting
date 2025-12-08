#' Get a plot of forecasts and scores for a single
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
        rotate_x_ticks = TRUE
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
      get_plot_theme() +
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
      get_plot_theme(rotate_x_ticks = TRUE) +
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
    get_plot_theme(rotate_x_ticks = TRUE) +
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

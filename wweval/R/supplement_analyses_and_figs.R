get_plot_scores_and_forecasts <- function(scores_single_loc_date,
                                          eval_output_subdir,
                                          n_calib_days = 10) {
  this_location <- scores_single_loc_date |>
    dplyr::distinct(location) |>
    dplyr::pull()

  this_forecast_date <- scores_single_loc_date |>
    dplyr::distinct(forecast_date) |>
    dplyr::pull()
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
        quantile %in% c(0.025, 0.25, 0.5, 0.75, 0.975),
        date >= min_scores_date
      ) |>
      tidyr::pivot_wider(
        id_cols = c(
          location, forecast_date, period, scenario,
          date, t, eval_data, calib_data, model_type
        ),
        names_from = quantile,
        values_from = value
      )


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
        labels = scales::date_format("%Y-%m-%d"),
      ) +
      xlab("") +
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
        labels = scales::date_format("%Y-%m-%d")
      ) +
      xlab("") +
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

    ggsave(fig,
      filename = file.path(fig_file_dir, "forecast_and_score_comp_fig.png"),
      width = 7, height = 10
    )
  } else {
    fig <- NULL
  }

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
      labels = scales::date_format("%Y-%m-%d"),
    ) +
    geom_hline(aes(yintercept = 0), linetype = "dashed") +
    scale_color_manual(values = colors$model_colors) +
    xlab("") +
    ylab("Average bias") +
    ggtitle("Average bias over time, across horizons and locations")

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
    # dplyr::filter(!horizon %in% c("0 week ahead", "5 week ahead")) |>
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
    facet_wrap(~horizon, nrow = length(unique(scores$horizon))) +
    scale_x_date(
      date_breaks = "2 weeks",
      labels = scales::date_format("%Y-%m-%d"),
    ) +
    scale_color_manual(values = colors$model_colors) +
    xlab("") +
    ylab(glue::glue("Average {score_type}")) +
    ggtitle(glue::glue("Average {score_type} over time, across locations"))

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

  n_forecasts_better <- relative_crps_by_forecast |>
    dplyr::filter(pct_change_crps < threshold) |>
    nrow()

  n_forecasts_worse <- relative_crps_by_forecast |>
    dplyr::filter(pct_change_crps > threshold) |>
    nrow()




  stats <- tibble::tibble(
    n_states_better,
    n_states_worse,
    n_forecasts_better,
    n_forecasts_worse
  )

  return(stats)
}

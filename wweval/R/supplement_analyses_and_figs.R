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
    dplyr::filter(!horizon %in% c("0 week ahead", "5 week ahead")) |>
    # hack, will fix upstream
    bind_rows(
      scores |>
        dplyr::mutate(horizon = "overall")
    ) |>
    dplyr::group_by(model, horizon) |>
    dplyr::summarize(
      avg_score = mean(!!sym({{ score_type }}))
    )

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
    ))

  n_states_better <- relative_crps_by_loc |>
    dplyr::filter(rel_crps < 1 - threshold) |>
    nrow()

  n_states_worse <- relative_crps_by_loc |>
    dplyr::filter(rel_crps > 1 + threshold) |>
    nrow()

  relative_crps_by_forecast <- scores |>
    dplyr::group_by(location, model, forecast_date) |>
    dplyr::summarize(crps = mean(crps)) |>
    compute_relative_crps(id_cols = c(
      "location", "forecast_date"
    ))

  n_forecasts_better <- relative_crps_by_forecast |>
    dplyr::filter(rel_crps < 1 - threshold) |>
    nrow()

  n_forecasts_worse <- relative_crps_by_forecast |>
    dplyr::filter(rel_crps > 1 + threshold) |>
    nrow()




  stats <- tibble::tibble(
    n_states_better,
    n_states_worse,
    n_forecasts_better,
    n_forecasts_worse
  )

  return(stats)
}

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
make_fig5_average_wis <- function(all_scores,
                                  cfa_real_time_scores,
                                  figure_file_path,
                                  horizon_time_in_weeks = NULL) {
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

  return(p)
}

#' Get plot of overall hub performance, grouped by period
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
#'
#' @return a ggplot object containing distributions of WIS scores grouped by
#' model and the comaprison time period, with the mean plotted alongside the
#' full distribution
#' @export
#'
make_fig5_hub_performance <- function(all_scores,
                                      cfa_real_time_scores,
                                      figure_file_path,
                                      all_time_period,
                                      real_time_period,
                                      baseline_model = "COVIDhub-baseline") {
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
    dplyr::mutate(
      fig_order =
        dplyr::case_when(
          period == "Feb 2024-Mar 2024" ~ 1,
          period == "Oct 2023-Mar 2024" ~ 0
        ),
      horizon = forcats::fct_reorder(horizon, fig_order)
    )


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


  return(p)
}

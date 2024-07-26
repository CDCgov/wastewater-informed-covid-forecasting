#' Get plot of WIS over time
#'
#' @param all_scores Scores from entire time period of interest, including
#' the retrospective cfa model, scored with [scoringutils::score()] and
#' summarized across quantiles only with [scoringutils::summarise_scores()]
#' @param cfa_real_time_scores Real-time scores from Feb - Mar for the cfa ww
#' model submitted to the hub, scored with [scoringutils::score()] and
#' summarized across quantiles only with [scoringutils::summarise_scores()]
#' @param horizon_time_in_weeks horizon time in weeks to summarize over, default
#' is `NULL` which means that the scores are summarized over the nowcast period
#' and the 4 week forecast period
#'
#' @return a ggplot object of WIS scores over time colored by model, for the
#' real-time cfa model from Feb - Mar and the retrospective CFA model over
#' all time points
#' @export
#'
make_fig5_average_wis <- function(all_scores,
                                  cfa_real_time_scores,
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

  colors <- plot_components()
  p <- ggplot(scores_by_forecast_date) +
    geom_line(
      aes(
        x = forecast_date, y = interval_score,
        color = model
      ),
      size = 1
    ) +
    geom_point(aes(
      x = forecast_date, y = interval_score,
      color = model
    )) +
    xlab("") +
    ylab("Average WIS across locations") +
    get_plot_theme(
      x_axis_dates = TRUE,
      y_axis_title_size = 8
    ) +
    scale_x_date(
      date_breaks = "2 weeks",
      labels = scales::date_format("%Y-%m-%d")
    ) +
    ggtitle(title) +
    scale_color_manual(values = colors$model_colors)

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
#' @param summarize_across_horizon Boolean indicating whether or not to
#' average the scores across the horizon, default is `FALSE` meaning
#' each day-forecast-date-location score is in the distribution
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
                                      summarize_across_horizon = FALSE,
                                      baseline_model = "COVIDhub-baseline") {
  if (isTRUE(summarize_across_horizon)) {
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
  } else {
    scores_by_model_all_time <- all_scores |>
      data.table::as.data.table() |>
      dplyr::mutate(
        period = {{ all_time_period }}
      )
    scores_by_model_real_time <- all_scores |>
      data.table::as.data.table() |>
      dplyr::filter(forecast_date >= lubridate::ymd("2024-02-05")) |>
      dplyr::bind_rows(cfa_real_time_scores) |>
      dplyr::mutate(
        period = {{ real_time_period }}
      )
  }

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
      horizon = forcats::fct_reorder(period, fig_order)
    )

  colors <- plot_components()

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
    coord_trans(ylim = c(0, 2)) +
    get_plot_theme(
      x_axis_dates = TRUE,
      y_axis_title_size = 8
    ) +
    scale_fill_manual(values = colors$model_colors) +
    scale_color_manual(values = colors$model_colors) +
    xlab("") +
    ylab(glue::glue("Relative WIS compared to {baseline_model}"))



  return(p)
}

#' Make a heatmap of relative WIS across locations
#'
#' @param scores df of granular (daily) score across models, locations, forecast
#' dates and horizons
#' @param time_period time period that scores are summarized over
#' @param baseline_model which model to compute relative WIS compared to, default
#' is `COVIDhub-baseline`
#'
#' @return a ggplot with a heatmap with model on the x-axis, location on the y-axis
#' and fill by relative WIS score across forecast dates and horizons
#' @export
#'
make_fig5_heatmap_relative_wis <- function(scores,
                                           time_period,
                                           baseline_model = "COVIDhub-baseline") {
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
    dplyr::filter(
      model != {{ baseline_model }},
      location != "US"
    ) # exclued the US bc not available for
  # retro model


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
    get_plot_theme(x_axis_dates = TRUE) +
    xlab("") +
    ylab("") +
    labs(fill = "Relative WIS") +
    ggtitle(glue::glue("Relative WIS compared to {baseline_model} /n from {time_period}"))


  return(p)
}

#' Make a quantile quantile plot for the Hub
#'
#' @param scores df of granular (daily) score across models, locations, forecast
#' dates and horizons
#' @param time_period time period that scores are summarized over
#'
#' @return a ggplot object containing a plot of the proportion of data within
#' each interval for each model.
#' @export

make_fig5_qq_plot <- function(scores,
                              time_period) {
  colors <- plot_components()
  p <- scores |>
    data.table::as.data.table() |>
    scoringutils::summarise_scores(by = c("model", "quantile")) |>
    scoringutils::plot_quantile_coverage() +
    ggtitle(glue::glue("QQ plot for {time_period}")) +
    get_plot_theme() +
    scale_color_manual(values = colors$model_colors)


  return(p)
}

#' Make a figure of the distribution of standardized WIS rank
#'
#' @description
#' Adapted from https://www.pnas.org/doi/10.1073/pnas.2113561119
#' and https://github.com/reichlab/covid19-forecast-evals/blob/b741b6a24e40c7f2a8ddc41da40c95b23db6df4e/code/figure-model-ranks.R#L11 #nolint
#'
#'
#' @param scores df of granular (daily) score across models, locations, forecast
#' dates and horizons
#' @param time_period time period that scores are summarized over
#'
#' @return A ggplot object containing geomridges plots colored by density,
#' indicating the standardized rank for each location-date combo
#' @export
make_fig5_density_rank <- function(scores,
                                   time_period) {
  summarized_scores <- scores |>
    data.table::as.data.table() |>
    scoringutils::summarise_scores(
      by = c("model", "location", "forecast_date")
    )

  scores_ranked <- summarized_scores |>
    tibble() |>
    dplyr::group_by(forecast_date, location) |>
    dplyr::mutate(
      rank = dplyr::dense_rank(dplyr::desc(interval_score)),
      std_rank = rank / max(rank)
    ) |>
    dplyr::mutate(model = stats::reorder(model, rank,
      FUN = function(x) {
        quantile(x, probs = 0.25, na.rm = TRUE)
      }
    ))

  fq <- scores_ranked |>
    dplyr::group_by(model) |>
    dplyr::summarize(first_quantile = quantile(std_rank,
      probs = 0.25,
      na.rm = TRUE
    )) |>
    dplyr::arrange(first_quantile) |>
    dplyr::mutate(
      fig_order = dplyr::row_number()
    )

  scores_ranked_ordered <- scores_ranked |>
    dplyr::left_join(fq, by = "model") |>
    dplyr::mutate(
      model = forcats::fct_reorder(model, fig_order)
    )


  p <- ggplot(
    scores_ranked_ordered,
    aes(
      x = std_rank, y = model,
      fill = factor(stat(quantile)),
      height = after_stat(density)
    )
  ) +
    ggridges::stat_density_ridges(
      geom = "density_ridges_gradient", calc_ecdf = TRUE,
      quantiles = 4, quantile_lines = TRUE,
      jittered_points = TRUE,
      position = ggridges::position_points_jitter(width = 0.05, height = 0),
      point_shape = "|", point_size = 3, point_alpha = 1, alpha = 0.7,
    ) +
    scale_fill_viridis_d(name = "Quartiles") +
    get_plot_theme() +
    scale_x_continuous(name = "Standardized rank", limits = c(0, 1)) +
    ylab("") +
    ggtitle(glue::glue("Standardized rank {time_period}"))

  return(p)
}

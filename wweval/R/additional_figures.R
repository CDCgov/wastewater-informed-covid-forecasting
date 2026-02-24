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

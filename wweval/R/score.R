#' Get the scores for ever day for a particular location and forecast date
#'
#' @param draws a dataframe of the model estimated  quantity you are evaluating
#' alongside the evaluation data
#' @param scenario a string indicating the wastewater data scenario we're
#' running
#' @param metrics Vector of scoring metrics to output, passed as the
#' `metrics` argument to [scoringutils::score()]. Default
#' `c("crps", "dss", "bias", "mad", "ae_median", "se_mean")`.
#' @param ... Additional named arguments passed to [scoringutils::score()].
#'
#' @return a dataframe containing a score for each day in the nowcast
#' and forecast period
#' @export
get_full_scores <- function(draws,
                            scenario,
                            metrics = c(
                              "crps", "dss", "bias",
                              "mad", "ae_median", "se_mean"
                            ),
                            ...) {
  # Filter to after the last date
  last_calib_date <- max(draws$date[!is.na(draws$calib_data)])

  forecasted_draws <- draws |>
    filter(date > last_calib_date) |>
    ungroup() |>
    # Rename for scoring utils
    rename(
      true_value = eval_data,
      prediction = value,
      sample = draw,
      model = model_type
    ) |>
    select(
      location,
      forecast_date,
      date,
      true_value,
      prediction,
      sample,
      model
    ) |>
    mutate(
      period = ifelse(date <= forecast_date, "nowcast", "forecast"),
      scenario = !!scenario
    )


  scores <- forecasted_draws |>
    scoringutils::score(metrics = metrics, ...)

  return(scores)
}

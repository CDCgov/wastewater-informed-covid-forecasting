#' Get the scores for ever day for a particular location and forecast date
#'
#' @description
#' Logs the truth data and forecasts and scores using scoringutils
#'
#'
#' @param draws a dataframe of the model estimated  quantity you are evaluating
#' alongside the evaluation data
#' @param scenario a string indicating the wastewater data scenario we're
#' running
#' @param metrics Vector of scoring metrics to output, passed as the
#' `metrics` argument to [scoringutils::score()]. Default
#' `c("crps", "dss", "bias", "mad", "ae_median", "se_mean")`.
#'
#' @return a dataframe containing a score for each day in the nowcast
#' and forecast period
#' @export
get_full_scores <- function(draws,
                            scenario,
                            metrics = c(
                              "crps", "dss", "bias",
                              "mad", "ae_median", "se_mean"
                            )) {
  if (is.null(draws)) {
    scores <- NULL
  } else {
    # Filter to after the last date
    last_calib_date <- max(draws$date[!is.na(draws$calib_data)])

    forecasted_draws <- draws |>
      filter(date > !!last_calib_date) |>
      ungroup() |>
      # Rename for scoring utils
      mutate(
        true_value = log(eval_data + 1e-8),
        prediction = log(value + 1e-8),
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
      scoringutils::score(metrics = metrics)
  }


  return(scores)
}

#' Get the scores for every day for a location, forecast date, and scenario
#' from the quantiles during the forecast period
#' @description
#' Logs the truth data and forecasts and scores using scoringutils
#'
#' @param quantiles a dataframe of the model estimated quantiles alongside
#' the data you are evaluating against, during the nowcast and forecast period
#' only
#' @param scenario a string indicating the wastewater data scenario we're
#' running
#' @param metrics Vector of scoring metrics to output, passed as the
#' `metrics` argument to [scoringutils::score()]. Default
#' `c("coverage", "dispersion", "bias", "range")`.
#'
#' @return a dataframe containing a score for each day in the nowcast
#' and forecast period
#' @export
get_scores_from_quantiles <- function(quantiles,
                                      scenario,
                                      metrics = c(
                                        "coverage", "dispersion", "bias",
                                        "range"
                                      )) {
  if (is.null(quantiles)) {
    scores <- NULL
  } else {
    forecasted_quantiles <- quantiles |>
      ungroup() |>
      # Rename for scoring utils
      mutate(
        true_value = log(eval_data + 1e-8),
        prediction = log(value + 1e-8),
        model = model_type
      ) |>
      select(
        location,
        forecast_date,
        date,
        true_value,
        prediction,
        quantile,
        model
      ) |>
      mutate(
        period = ifelse(date <= forecast_date, "nowcast", "forecast"),
        scenario = !!scenario
      )


    scores <- forecasted_quantiles |>
      scoringutils::score(metrics = metrics)
  }


  return(scores)
}

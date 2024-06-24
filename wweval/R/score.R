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
#' `metrics` argument to [scoringutils::score()]. Default is NULL,
#' which returns all options for samples including:
#' `c("crps", "dss", "bias", "mad", "ae_median", "se_mean")`.
#'
#' @return a dataframe containing a score for each day in the nowcast
#' and forecast period
#' @export
get_full_scores <- function(draws,
                            scenario,
                            metrics = NULL) {
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
#' `metrics` argument to [scoringutils::score()]. Default is NULL which will
#' include all scoring metrics for quantiles by default, including
#' `c("interval_score", "coverage", "dispersion", "bias")`.
#'
#' @return a dataframe containing a score for each day in the nowcast
#' and forecast period
#' @export
get_scores_from_quantiles <- function(quantiles,
                                      scenario,
                                      metrics = NULL) {
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

#' Make baseline score table
#'
#' @description
#' This function makes a wide table with the average score of a pipeline run
#' summarized across all locations. The point of this is to get an approximate
#' estimate of the performance of the pipeline/model across locations when we
#' are iterating on model development. This way, we can use this score as a
#' baseline and aim to add new features to the model only if the overall score
#' of the forecast performance is improved
#'
#'
#' @param all_ww_scores the table of scores for all dates from all locations
#' from the wastewater informed model
#' @param baseline_score_table_dir character string indicating the directory
#'  to save the baseline score tables
#' @param overwrite_table boolean indicating whether or not to overwrite the
#' current baseline table, default is FALSE.
#'
#' @return a table containing the summarized outputs from scoring utils for the
#' wastewater model across dates and locations
#' @export
#'
make_baseline_score_table <- function(all_ww_scores,
                                      baseline_score_table_dir,
                                      overwrite_table = FALSE) {
  # Get metadata
  locations <- all_ww_scores |>
    dplyr::pull(location) |>
    unique()
  forecast_dates <- all_ww_scores |>
    dplyr::pull(forecast_date) |>
    unique()
  scenario <- all_ww_scores |>
    dplyr::pull(scenario) |>
    unique()


  # Score forecasts
  scores <- scoringutils::summarize_scores(all_ww_scores,
    by = c(
      "scenario",
      "forecast_date"
    )
  ) |>
    dplyr::mutate(
      locations = paste(locations, collapse = ",")
    )

  if (isTRUE(overwrite_table)) {
    model_type <- if (scenario == "status_quo") "ww" else "hosp"
    # Check that is only one scenario
    stopifnot("more than one scenario" = length(unique(scores$scenario)) == 1)
    # Check that is only one forecast_date
    stopifnot("more than one forecast_date" = length(unique(scores$forecast_date)) == 1)

    cfaforecastrenewalww::create_dir(baseline_score_table_dir)

    write.table(scores, file.path(
      baseline_score_table_dir,
      glue::glue("baseline_scores_{model_type}.tsv")
    ))
  }

  return(scores)
}

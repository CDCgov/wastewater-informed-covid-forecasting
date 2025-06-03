#' Metrics to use for quantile scores
#' @export
quantile_metrics <- scoringutils::get_metrics(
  scoringutils::example_quantile
)

#' Metrics to use for quantile scores
#' @export
sample_metrics <- scoringutils::get_metrics(
  scoringutils::example_sample_discrete
)

#' Get the scores for every day for a particular location and forecast date
#'
#' @description
#' Uses scoringutils to transform data and predictions using a log transform
#' with a log shift of 1, and return the default scoring metrics
#'
#'
#' @param draws a dataframe of the model estimated  quantity you are evaluating
#' alongside the evaluation data
#' @param scenario a string indicating the wastewater data scenario we're
#' running
#' @param metrics Vector of scoring metrics to output, passed as the
#' `metrics` argument to [scoringutils::score()]. Default
#' [sample_metrics]
#'
#' @return a dataframe containing a score for each day in the nowcast
#' and forecast period
#' @export
score_samples <- function(draws, scenario, metrics = sample_metrics) {
  if (is.null(draws)) {
    scores <- NULL
  } else {
    # Filter to after the last date
    last_calib_date <- max(draws$date[!is.na(draws$calib_data)])

    forecasted_draws <- draws |>
      dplyr::filter(.data$date > !!last_calib_date) |>
      dplyr::select(
        model = "model_type",
        "location",
        "forecast_date",
        "date",
        "value",
        "eval_data",
        "draw"
      )
    to_score <- forecasted_draws |>
      scoringutils::as_forecast_sample(
        predicted = "value",
        observed = "eval_data",
        sample_id = "draw"
      ) |>
      scoringutils::transform_forecasts(
        fun = scoringutils::log_shift,
        offset = 1
      )

    if (is.null(metrics)) {
      metrics <- scoringutils::get_metrics(to_score)
    }

    scores <- scoringutils::score(to_score, metrics = metrics) |>
      dplyr::mutate(
        period = ifelse(
          .data$date <= .data$forecast_date,
          "nowcast",
          "forecast"
        ),
        scenario = !!scenario
      )
  }

  return(scores)
}

#' Get the scores for every day for a location, forecast date, and scenario
#' from the quantiles during the forecast period
#' @description
#' Uses scoringutils to transform data and predictions using a log transform
#' with a log shift of 1, and return the default scoring metrics
#'
#' @param quantiles a dataframe of the model estimated quantiles alongside
#' the data you are evaluating against, during the nowcast and forecast period
#' only
#' @param scenario a string indicating the wastewater data scenario we're
#' running
#' @param metrics Vector of scoring metrics to output, passed as the
#' `metrics` argument to [scoringutils::score()]. Default
#' [quantile_metrics]
#'
#' @return a dataframe containing a score for each day in the nowcast
#' and forecast period
#' @export
score_quantiles <- function(quantiles, scenario, metrics = quantile_metrics) {
  if (is.null(quantiles)) {
    scores <- NULL
  } else {
    forecasted_quantiles <- quantiles |>
      dplyr::select(
        model = "model_type",
        "location",
        "forecast_date",
        "date",
        "value",
        "eval_data",
        "quantile"
      )

    to_score <- forecasted_quantiles |>
      scoringutils::as_forecast_quantile(
        predicted = "value",
        observed = "eval_data",
        quantile_level = "quantile"
      ) |>
      scoringutils::transform_forecasts(
        fun = scoringutils::log_shift,
        offset = 1
      )

    if (is.null(metrics)) {
      metrics <- scoringutils::get_metrics(to_score)
    }

    scores <- scoringutils::score(to_score, metrics = metrics) |>
      dplyr::mutate(
        period = ifelse(
          .data$date <= .data$forecast_date,
          "nowcast",
          "forecast"
        ),
        scenario = !!scenario
      )
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
make_baseline_score_table <- function(
  all_ww_scores,
  baseline_score_table_dir,
  overwrite_table = FALSE
) {
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
  scores <- scoringutils::summarize_scores(
    all_ww_scores,
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
    stopifnot(
      "more than one scenario" = length(unique(
        scores$scenario
      )) ==
        1
    )
    # Check that is only one forecast_date
    stopifnot(
      "more than one forecast_date" = length(unique(
        scores$forecast_date
      )) ==
        1
    )

    wwinference::create_dir(baseline_score_table_dir)

    write.table(
      scores,
      file.path(
        baseline_score_table_dir,
        glue::glue("baseline_scores_{model_type}.tsv")
      )
    )
  }

  return(scores)
}


#' Clean flags from a real-time forecast
#'
#' Fixes typos and computes needed quantities if absent
#'
#' @param df Dataframe of flags
#' @return A cleaned version of the data frame,
#' with flag names corrected.
clean_flag_df <- function(df) {
  return(dplyr::mutate(
    df,
    diagnostic = dplyr::case_match(
      .data$diagnostic,
      "flag_low_embfi" ~ "flag_low_ebfmi",
      .default = .data$diagnostic
    )
  ))
}

#' Compute diagnostic flags from raw stanfit objects
#'
#' @param forecast_dir Path to a top-level forecast directory.
#' @param forecast_date Forecast date, as a string in YYYY-MM-DD format.
#' @param run_on_date Date the model was run, as a string in
#' YYYY-MM-DD format.
#' @param run_id run UUID, as a string.
#' @return The flags, as the output of [get_diagnostic_flags()].
compute_flags <- function(forecast_dir, forecast_date, run_on_date, run_id) {
  cli::cli_inform(c(
    "Recomputing flags from raw stanfit objects. ",
    "This may take a moment."
  ))
  ## diagnostic flags were not saved prior to 2024-02-26;
  ## need to compute manually. Subsequent flag computation
  ## used the same flag thresholds as get_diagnostic_flags()
  ## (see https://github.com/CDCgov/wastewater-informed-covid-forecasting/blob/06d13e0b4f4cd4fbd0334ea22341b800c504abc9/cfaforecastrenewalww/R/process_model_outputs.R#L468-L473)  # nolint
  ## so we can just use that function.
  run_dir <- glue::glue("run-on-{run_on_date}-{run_id}")
  date_dir <- run_on_date
  old_file_structure <- as.Date(forecast_date) < as.Date("2024-02-19")
  stan_csvs <- fs::path(
    forecast_dir,
    "stan_objects",
    ifelse(
      old_file_structure,
      fs::path(
        forecast_date,
        run_on_date,
        run_dir
      ),
      fs::path("")
    )
  ) |>
    fs::dir_ls(
      type = "file",
      glob = "*.csv"
    )

  stanfit <- cmdstanr::as_cmdstan_fit(stan_csvs)
  flags <- get_diagnostic_flags(stanfit)

  return(flags)
}

#' Check whether a model run had any flags.
#'
#' @param forecast_dir Path to a top-level forecast directory.
#' @param forecast_date Forecast date, as a string in YYYY-MM-DD format.
#' @param run_on_date Date the model was run, as a string in
#' YYYY-MM-DD format.
#' @param run_id run UUID, as a string.
#' @return `FALSE` if there were no flags, `TRUE` if there were
#' any flags.
check_any_flags <- function(forecast_dir, forecast_date, run_on_date, run_id) {
  flags_to_check <- c(
    "flag_low_ebfmi",
    "flag_too_many_divergences",
    "flag_high_rhat",
    "flag_high_max_treedepth"
  )

  real_time_flag_path <- fs::path(
    forecast_dir,
    "diagnostics",
    ext = "csv"
  )
  post_hoc_flag_path <- fs::path(
    forecast_dir,
    "post_hoc_diagnostics",
    ext = "csv"
  )

  flags_found <- FALSE
  if (fs::file_exists(real_time_flag_path)) {
    flag_tab <- readr::read_csv(real_time_flag_path) |>
      clean_flag_df()
    checkmate::assert_names(
      names(flag_tab),
      must.include = "diagnostic"
    )
    flags_found <- checkmate::test_names(
      na.omit(flag_tab$diagnostic),
      must.include = flags_to_check
    )
    if (flags_found) {
      cli::cli_inform(
        "Using archived flags computed in real time..."
      )
    }
  }
  if (!flags_found && fs::file_exists(post_hoc_flag_path)) {
    flag_tab <- readr::read_csv(post_hoc_flag_path) |>
      clean_flag_df()
    checkmate::assert_names(
      names(flag_tab),
      must.include = "diagnostic"
    )
    flags_found <- checkmate::test_names(
      na.omit(flag_tab$diagnostic),
      must.include = flags_to_check
    )

    if (flags_found) {
      cli::cli_inform(paste0(
        "Using flags computed post-hoc in a ",
        "previous analysis run..."
      ))
    }
  }

  if (!flags_found) {
    cli::cli_inform(paste0(
      "Could not find flags computed in real ",
      "time or subsequently computed post-hoc. Will ",
      "recompute now..."
    ))

    flag_tab <- compute_flags(
      forecast_dir,
      forecast_date,
      run_on_date,
      run_id
    ) |>
      tidyr::pivot_longer(
        tidyselect::everything(),
        names_to = "diagnostic",
        values_to = "value"
      )

    cli::cli_inform(paste0(
      "Saving flags ",
      "to {post_hoc_flag_path}..."
    ))
    readr::write_csv(flag_tab, post_hoc_flag_path)
  }

  checkmate::assert_names(
    na.omit(flag_tab$diagnostic),
    must.include = flags_to_check
  )
  flags <- flag_tab |>
    dplyr::filter(.data$diagnostic %in% !!flags_to_check) |>
    dplyr::pull(.data$value)

  return(any(flags == TRUE))
}


#' Load a real-time forecast
#'
#' Loads a forecast for a single location, date,
#' model type, and run.
#'
#' @param output_dir Output directory containing real-time
#' forecasts.
#' @param forecast_date Forecast date, as a string.
#' @param location Forecast location, as a string.
#' @param model_type Model type, as a string. One of
#' `"hosp"` or `"ww"`.
#' @param forecast_output_type Type of forecast output to pull.
#' One of `"quantiles"` o `"draws"`.
#' @param table_of_run_ids Table mapping forecast dates
#' to chosen run unique ids and run dates.
#' @return The forecast, as a tibble.
#'
#' @export
load_real_time_forecast <- function(
  output_dir,
  forecast_date,
  location,
  model_type,
  forecast_output_type,
  table_of_run_ids
) {
  checkmate::assert_scalar(model_type)
  checkmate::assert_names(model_type, subset.of = c("ww", "hosp"))
  checkmate::assert_scalar(forecast_output_type)
  checkmate::assert_names(
    forecast_output_type,
    subset.of = c("quantiles", "draws")
  )
  ## remove trailing s from output type, col name is singular
  output_id_col <- stringr::str_sub(forecast_output_type, end = -2)

  metadata <- table_of_run_ids |>
    dplyr::filter(.data$forecast_date == !!forecast_date)
  run_id <- metadata$ids
  date_run <- metadata$dates_run
  model_long <- c(
    "ww" = "site-level infection dynamics",
    "hosp" = "hospital admissions only"
  )[[model_type]]

  old_file_structure <- as.Date(forecast_date) < as.Date("2024-02-19")
  if (old_file_structure) {
    dir <- fs::path(
      output_dir,
      forecast_date,
      "raw",
      location,
      model_long
    )
    forecast_filename <- fs::path(
      forecast_output_type,
      forecast_date,
      glue::glue(
        "run-on-{date_run}",
        "-{run_id}-",
        "{forecast_output_type}"
      )
    )
  } else {
    dir <- fs::path(
      output_dir,
      forecast_date,
      glue::glue("run-on-{date_run}-{run_id}"),
      "raw",
      location,
      model_long
    )
    forecast_filename <- forecast_output_type
  }

  forecast_path <- fs::path(dir, forecast_filename, ext = "parquet")
  cli::cli_inform("Looking for forecast at {forecast_path}...")
  forecast_exists <- fs::file_exists(forecast_path)
  forecast <- NULL

  if (forecast_exists) {
    any_flags <- check_any_flags(
      dir,
      forecast_date,
      date_run,
      run_id
    )
    forecast <- arrow::read_parquet(forecast_path) |>
      dplyr::filter(
        .data$name == "pred_hosp",
        .data$period != "calibration"
      ) |>
      dplyr::select(
        "forecast_date",
        "date",
        "location",
        "value",
        !!output_id_col
      ) |>
      dplyr::mutate(
        model = !!model_type,
        failed_convergence = !!any_flags
      )
  }

  return(forecast)
}


#' Load real-time quantile outputs
#'
#' @param real_time_output_dir A string indicating the upper
#' level directory where the real-time outputs live locally
#' @param table_of_run_ids A tibble containing the forecast date, run id,
#' and date run for each of the production runs
#' @param locations A vector of character strings indicating
#' the locations to pull, this should be all jurisdictions
#' @param eval_data a tibble of hospital admissions evaluation
#' data to be used for scoring.
#' @param model_type String indicating model type to load.
#' One of `"ww"` or `"hosp"`.
#' @return The forecasts, as the output of
#' [scoringutils::as_forecast_quantile()]
#' @export
load_real_time_quantile_fcsts <- function(
  real_time_output_dir,
  table_of_run_ids,
  locations,
  eval_data,
  model_type
) {
  checkmate::assert_scalar(model_type)
  checkmate::assert_names(model_type, subset.of = c("ww", "hosp"))

  load_forecast <- function(forecast_date, location) {
    forecast <- load_real_time_forecast(
      real_time_output_dir,
      forecast_date,
      location,
      model_type,
      "quantiles",
      table_of_run_ids
    )

    if (!is.null(forecast)) {
      forecast <- forecast |>
        format_for_hub() |>
        dplyr::filter(!is.na(.data$quantile)) |>
        dplyr::mutate(
          location = forecasttools::us_loc_code_to_abbr(
            .data$location
          )
        ) |>
        dplyr::inner_join(
          eval_data |>
            dplyr::select(
              observed = "daily_hosp_admits",
              target_end_date = "date",
              location = "location"
            ),
          by = c("location", "target_end_date")
        ) |>
        scoringutils::as_forecast_quantile(
          predicted = "value",
          observed = "observed",
          quantile_level = "quantile"
        )
    }
    return(forecast)
  }

  to_load <- tidyr::crossing(
    forecast_date = table_of_run_ids$forecast_date,
    location = locations
  )

  return(purrr::pmap_df(to_load, load_forecast))
}


#' Load in and score the real-time outputs
#'
#' @param score_type A string indicating which score to generate, either
#' "crps" or "wis". Note, if using crps, will score draws from nowcast and
#' forecast. If using wis, will score only the forecasts.
#' @param real_time_output_dir A string indicating the upper
#' level directory where the real-time outputs live locally
#' @param table_of_run_ids A tibble containing the forecast date, run id,
#' and date run for each of the production runs
#' @param locations A vector of character strings indicating
#' the locations to
#' pull, this should be all jurisdictions
#' @param eval_data a tibble of hospital admissions evaluation
#' data to be used
#' for scoring.
#' @param model_types Character vector of model types to score.
#' One or both of `"ww"` and `"hosp"`. Default both: `c("ww", "hosp")`
#' @return A tibble containing scores for every location and
#' forecast date, conditioned on the presence of wastewater and model
#' convergence
#' @export
score_real_time_outputs <- function(
  score_type,
  real_time_output_dir,
  table_of_run_ids,
  locations,
  eval_data,
  model_types = c("ww", "hosp")
) {
  checkmate::assert_scalar(score_type)
  checkmate::assert_names(score_type, subset.of = c("wis", "crps"))
  model_types <- unique(model_types)
  checkmate::assert_names(model_types, subset.of = c("ww", "hosp"))

  forecast_output_type <- c(
    "wis" = "quantiles",
    "crps" = "draws"
  )[[score_type]]
  ## remove trailing s from output type, col name is singular
  output_id_col <- stringr::str_sub(forecast_output_type, end = -2)

  score_problem <- function(forecast_date, location, model_type) {
    scores <- NULL

    forecast <- load_real_time_forecast(
      real_time_output_dir,
      forecast_date,
      location,
      model_type,
      forecast_output_type,
      table_of_run_ids
    )

    if (!is.null(forecast)) {
      preds_w_eval <- forecast |>
        dplyr::inner_join(
          eval_data |>
            dplyr::select(-"pop") |>
            dplyr::rename(
              true_value = "daily_hosp_admits"
            ),
          by = c("location", "date")
        ) |>
        dplyr::select(
          "location",
          "forecast_date",
          "date",
          "value",
          "true_value",
          !!output_id_col,
          "model",
          "failed_convergence"
        )

      if (score_type == "crps") {
        for_scoring <- preds_w_eval |>
          scoringutils::as_forecast_sample(
            sample_id = "draw",
            predicted = "value",
            observed = "true_value"
          )
      } else if (score_type == "wis") {
        for_scoring <- preds_w_eval |>
          dplyr::filter(
            .data$date > .data$forecast_date
          ) |>
          scoringutils::as_forecast_quantile(
            predicted = "value",
            observed = "true_value",
            quantile_level = "quantile"
          )
      }
      if (nrow(for_scoring) > 0) {
        scores <- for_scoring |>
          scoringutils::transform_forecasts(
            fun = scoringutils::log_shift,
            offset = 1,
            append = FALSE
          ) |>
          scoringutils::score()
      }
    }

    return(scores)
  }

  to_score <- tidyr::crossing(
    forecast_date = table_of_run_ids$forecast_date,
    location = locations,
    model_type = model_types
  )

  return(purrr::pmap_df(to_score, score_problem))
}

#' Format the hosp only real time scores for comparison to the other real
#' time models
#'
#' @param real_time_scores the set of real time scores gathered from local
#' pull
#' @return a tibble formatted as the other real time scores for the real
#' time hosp only model
#' @export
format_scores_for_comparison <- function(real_time_scores) {
  formatted_scores <- real_time_scores |>
    dplyr::filter(
      model == "hosp",
      scale == "log"
    ) |>
    dplyr::mutate(
      location = loc_abbr_to_flusight_code(location),
      model = "cfa-hosponlyrenewal(real-time*)",
      type = "quantile",
      days_ahead = as.numeric(date - forecast_date),
      target = glue::glue("{days_ahead} day ahead inc hosp"),
      horizon_days = as.integer(
        lubridate::ymd(date) -
          lubridate::ymd(forecast_date)
      ),
      horizon_weeks = ceiling(horizon_days / 7),
      horizon = glue::glue("{horizon_weeks} week ahead")
    ) |>
    dplyr::rename(
      target_end_date = date
    )

  return(formatted_scores)
}

#' Combine the hosp only real-time scores and cfa real-
#' time scores from github
#'
#' @param cfa_real_time_scores Hub formatted scores for
#' only the ww model, `cfa-wwrenewal(real-time)`
#' @param real_time_wis_hosp_only hosp only
#' wis scores calculated from local data
#'
#' @return a df formatted the same way as the real_time_wis_hosp_only scores,
#' but for both models
#' @export
combine_hub_and_local_wis <- function(
  cfa_real_time_scores,
  real_time_wis_hosp_only
) {
  real_time_wis_ho <- real_time_wis_hosp_only |>
    dplyr::select(-failed_convergence)

  loc_map_table <- cfa_real_time_scores |>
    dplyr::distinct(location) |>
    dplyr::left_join(
      wweval::flusight_location_table,
      by = c("location" = "location_code")
    )

  rt_reformatted <- cfa_real_time_scores |>
    dplyr::rename(location_code = location) |>
    dplyr::left_join(
      loc_map_table,
      by = c("location_code" = "location")
    ) |>
    dplyr::rename(
      location = short_name,
      date = target_end_date,
    ) |>
    dplyr::mutate(
      model = "ww"
    ) |>
    dplyr::select(colnames(real_time_wis_ho))

  real_time_wis_both_models <- dplyr::bind_rows(
    rt_reformatted,
    real_time_wis_ho
  )
  return(real_time_wis_both_models)
}


#' Make a summary of the with and without wastewater comparison scores
#'
#' @param scores a tibble of scores for each model, horizon day, forecast date
#' and location for the subset of forecasts used in the head-to-head comparison
#'
#' @return a list of two tables with summary scores, one overall and one by
#' forecast vs nowcast
#' @export
get_score_summary_tables <- function(scores) {
  # Overall avg crps, bias, absolute error etc
  scores_overall <- scoringutils::summarise_scores(scores)
  scores_by_period <- scoringutils::summarise_scores(
    scores,
    by = "period"
  )

  scores_tables <- list(
    scores_overall = scores_overall,
    scores_by_period = scores_by_period
  )

  return(scores_tables)
}

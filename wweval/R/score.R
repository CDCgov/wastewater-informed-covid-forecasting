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
score_samples <- function(draws,
                          scenario,
                          metrics = sample_metrics) {
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

    scores <- scoringutils::score(to_score,
      metrics = metrics
    ) |>
      dplyr::mutate(
        period =
          ifelse(.data$date <= .data$forecast_date,
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
score_quantiles <- function(quantiles,
                            scenario,
                            metrics = quantile_metrics) {
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

    scores <- scoringutils::score(to_score,
      metrics = metrics
    ) |>
      dplyr::mutate(
        period = ifelse(.data$date <= .data$forecast_date,
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

    wwinference::create_dir(baseline_score_table_dir)

    write.table(scores, file.path(
      baseline_score_table_dir,
      glue::glue("baseline_scores_{model_type}.tsv")
    ))
  }

  return(scores)
}


#' Query Zoltar for models to include in the analysis
#'
#' @description
#' This function uses the `zoltr` R package to connect to the Zoltar
#' database, which contains forecasts from the COVID Hub forecast
#' project, and query it for the specified forecast dates.
#' It queries for all dates, computes the proportion of dates for
#' which the model has submitted, filters for models that have
#' submitted for greater than the specified proportion of forecast
#' dates for inclusion, and returns the vector of model names.
#'
#' @param prop_dates_for_incl_hub Numeric greater than 0 and less
#' than or equal to 1 indicating the inclusion threshold for the
#' proportion of forecast dates that a model must have submitted
#' forecasts to be included in analysis.
#' @param prop_locs_for_incl_hub Numeric less than 1 indicating
#' the inclusion threshold for the proportion of the locations
#' we expect that a model must have subbmited forecasts for to
#' be included in analysis.
#' @param forecast_dates vector of dates formatted in ISO8601 convention
#' (YYYY-MM-DD) indicating the forecast dates for the analysis
#' @param locations vector of state abbreviations that we want to
#' ensure the submitting teams have produced forecasts for.
#' @param project_name name of the Zoltar project, default is
#' `"COVID-19 Forecasts"`.
#'
#' @return a vector of character strings indicating the
#' unique model names that fit the inclusion criteria
#' @export
query_and_select_models <- function(prop_dates_for_incl_hub,
                                    prop_locs_for_incl_hub,
                                    forecast_dates,
                                    locations,
                                    project_name = "COVID-19 Forecasts") {
  assert_needed_env_vars(c("ZOLTAR_USERNAME", "ZOLTAR_PASSWORD"))
  # get state abbreviation codes
  state_codes <- loc_abbr_to_flusight_code(
    unique(locations)
  )

  if (prop_dates_for_incl_hub > 1 || prop_dates_for_incl_hub <= 0) {
    cli::cli_abort(c(
      "Proportion of forecast dates required for hub inclusion",
      "must be greater than 0 and less than or equal to 1."
    ))
  }

  if (prop_locs_for_incl_hub > 1 || prop_locs_for_incl_hub <= 0) {
    cli::cli_abort(c(
      "Proportion of locations required for hub inclusion",
      "must be greater than 0 and less than or equal to 1."
    ))
  }

  zoltar_connection <- zoltr::new_connection()
  zoltr::zoltar_authenticate(
    zoltar_connection, Sys.getenv("ZOLTAR_USERNAME"),
    Sys.getenv("ZOLTAR_PASSWORD")
  )

  # list of project on zoltar
  the_projects <- zoltr::projects(zoltar_connection)

  # Grabbing a specific project
  project_url <- the_projects[the_projects$name == project_name, "url"]
  the_project_info <- zoltr::project_info(zoltar_connection, project_url)

  # get the models
  the_models <- zoltr::models(zoltar_connection, project_url)

  # Submit query, poll job, get job data

  forecast_data <- zoltr::do_zoltar_query(
    zoltar_connection = zoltar_connection,
    project_url = project_url,
    query_type = "forecasts",
    models = NULL, # all models by default
    units = state_codes,
    # We could query all of them, but this was very slow. This ensures
    # that the forecasts submitted have at least reached 28 days.
    targets = c("28 day ahead inc hosp"),
    types = "quantile",
    timezeros = forecast_dates
  )

  n_unique_forecasts <- forecast_data |>
    dplyr::distinct(timezero) |>
    dplyr::pull() |>
    length()

  forecasts_present_per_model <- forecast_data |>
    dplyr::distinct(timezero, model, unit) |>
    dplyr::group_by(model, timezero) |>
    dplyr::summarize(
      n_locs = dplyr::n(),
      prop_locs = n_locs / length(state_codes)
    ) |>
    # Exclude any forecast dates/models with too few locations submitted
    dplyr::filter(prop_locs >= !!prop_locs_for_incl_hub) |>
    dplyr::group_by(model) |>
    dplyr::summarize(
      n_forecast_dates = dplyr::n(),
      prop_present = n_forecast_dates / !!n_unique_forecasts
    )

  models <- forecasts_present_per_model |>
    dplyr::filter(prop_present > !!prop_dates_for_incl_hub) |>
    dplyr::filter(model != "COVIDhub_CDC-ensemble") |>
    dplyr::pull(model)

  return(models)
}

#' Score hub submissions
#'
#' @param model_name a vector of character strings indicating the names of the
#' models to scores
#' @param dates a vector of dates indicating the dates of the submissions to
#'  score
#' @param locations a vector of character strings indicating the locations
#' to score
#' @param hub_subdir path where the retrospective hub submissions are saved
#' locally since these are not on COVID hub github
#' @param pull_from_github boolean indicating whether or not to pull from github
#' @param submissions_path url pointing to the "data-processed" folder on
#' the COVIDhub github, which is where team's submissions are located
#' @param truth_data_path the path to the truth data used by the hub for
#' evaluation
#'
#' @return a dataframe containing all of the scores for all models,
#' forecast dates (indicated by dates), locations, target end dates, and
#' quantiles
#' @export
#'
score_hub_submissions <- function(model_name,
                                  dates,
                                  locations = NULL,
                                  hub_subdir = NA,
                                  pull_from_github = TRUE,
                                  submissions_path = "https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/master/data-processed/", # nolint
                                  truth_data_path = "https://media.githubusercontent.com/media/reichlab/covid19-forecast-hub/master/data-truth/truth-Incident%20Hospitalizations.csv") { # nolint

  truth_data <- truth_data <- readr::read_csv(truth_data_path,
    show_col_types = FALSE
  ) |>
    dplyr::rename(
      true_value = "value",
      target_end_date = "date"
    )

  to_score <- tidyr::crossing(
    model_name = model_name,
    forecast_date = dates
  )

  score_model_date <- function(model_name, forecast_date) {
    if (isTRUE(pull_from_github)) {
      gh_path <- glue::glue(
        "{submissions_path}{model_name}/",
        "{forecast_date}-{model_name}.csv"
      )
      quantiles <- tryCatch(
        readr::read_csv(
          gh_path,
          show_col_types = FALSE
        ) |>
          dplyr::filter(type == "quantile"),
        error = function(e) {
          NULL
        }
      )
    } else {
      quantiles <- readr::read_csv(
        file.path(
          hub_subdir, model_name,
          glue::glue("{forecast_date}-{model_name}.csv")
        ),
        show_col_types = FALSE
      )
    }

    if (is.null(quantiles)) {
      quantiles_w_truth <- tibble::tibble()
    } else {
      quantiles_w_truth <- quantiles |>
        dplyr::rename(prediction = value) |>
        dplyr::mutate(model = !!model_name) |>
        dplyr::inner_join(truth_data,
          by = c(
            "target_end_date",
            "location"
          )
        )
    }

    ## Filter locations if they are specified,
    ## otherwise leave them all in
    if (!is.null(locations)) {
      quantiles_w_truth <- quantiles_w_truth |>
        dplyr::filter(
          .data$location %in%
            loc_abbr_to_flusight_code(!!locations)
        )
    }

    if (nrow(quantiles_w_truth) > 0) {
      scores <- quantiles_w_truth |>
        scoringutils::as_forecast_quantile(
          predicted = "prediction",
          observed = "true_value",
          quantile_level = "quantile"
        ) |>
        scoringutils::transform_forecasts(
          fun = scoringutils::log_shift,
          offset = 1
        ) |>
        scoringutils::score() |>
        dplyr::mutate(horizon_days = as.integer(
          lubridate::ymd(.data$target_end_date) -
            lubridate::ymd(.data$forecast_date)
        )) |>
        dplyr::mutate(
          horizon_weeks = .data$horizon_days %/% 7 + 1,
          horizon = glue::glue("{horizon_weeks} week ahead")
        ) |>
        dplyr::select(-"horizon_weeks", -"horizon_days")
    } else {
      scores <- tibble(
        model = model_name,
        forecast_date = lubridate::ymd(forecast_date),
        scale = "missing"
      )
    }

    return(scores)
  }

  all_scores <- purrr::pmap_df(to_score, score_model_date)

  scores_list <- list(
    natural_scale_scores = all_scores |>
      dplyr::filter(.data$scale == "natural"),
    log_scale_scores = all_scores |>
      dplyr::filter(.data$scale == "log"),
    missing_forecasts = all_scores |>
      dplyr::filter(.data$scale == "missing") |>
      dplyr::select("model", "forecast_date")
  )
  return(scores_list)
}


#' Clean flags from a real-time forecast
#'
#' Fixes typos and computes needed quantities if absent
#'
#' @param df Dataframe of flags
#' @return A cleaned version of the data frame,
#' with flag names corrected.
clean_flag_df <- function(df) {
  df <- df |>
    dplyr::mutate(
      diagnostic = dplyr::recode(.data$diagnostic, "flag_low_embfi" = "flag_low_ebfmi")
    )

  return(df)
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
load_real_time_forecast <- function(output_dir,
                                    forecast_date,
                                    location,
                                    model_type,
                                    forecast_output_type,
                                    table_of_run_ids) {
  checkmate::assert_scalar(model_type)
  checkmate::assert_names(model_type, subset.of = c("ww", "hosp"))
  checkmate::assert_scalar(forecast_output_type)
  checkmate::assert_names(forecast_output_type,
    subset.of = c("quantiles", "draws")
  )
  ## remove trailing s from output type, col name is singular
  output_id_col <- stringr::str_sub(forecast_output_type,
    end = -2
  )

  flags_to_check <- c(
    "flag_low_ebfmi",
    "flag_too_many_divergences",
    "flag_high_rhat",
    "flag_high_max_treedepth"
  )

  metadata <- table_of_run_ids |>
    dplyr::filter(.data$forecast_date == !!forecast_date)
  run_id <- metadata$ids
  date_run <- metadata$dates_run
  model_long <- c(
    "ww" = "site-level infection dynamics",
    "hosp" = "hospital admissions only"
  )[[model_type]]

  if (forecast_date %in% c("2024-02-05", "2024-02-12")) {
    ## older file structure
    dir <- fs::path(
      output_dir,
      glue::glue("output_{forecast_date}"),
      "raw",
      location,
      forecast_output_type,
      forecast_date
    )
    filename <- glue::glue(
      "run-on-{date_run}-{run_id}-",
      "{forecast_output_type}"
    )
    ## older structure did not have flags
    any_flags <- FALSE
  } else {
    ## main newer file structure
    dir <- fs::path(
      output_dir,
      forecast_date,
      glue::glue("run-on-{date_run}-{run_id}"),
      "raw",
      location,
      model_long
    )
    filename <- forecast_output_type

    diagnostic_file <- fs::path(dir, "diagnostics", ext = "csv")
    stan_csvs <- fs::dir_ls(fs::path(dir, "stan_objects"),
      type = "file",
      glob = "*.csv"
    )

    if (forecast_date == "2024-02-19") {
      ## diagnostic flags were not computed on 2024-02-19,
      ## need to compute manually. Subsequent flag computation
      ## used the same flag thresholds as get_diagnostic_flags()
      ## (see https://github.com/CDCgov/wastewater-informed-covid-forecasting/blob/06d13e0b4f4cd4fbd0334ea22341b800c504abc9/cfaforecastrenewalww/R/process_model_outputs.R#L468-L473)  # nolint
      ## so we can just use that function.

      stanfit <- cmdstanr::as_cmdstan_fit(stan_csvs)

      flag_tab <- get_diagnostic_flags(stanfit)
    } else if (fs::file_exists(diagnostic_file)) {
      flag_tab <- readr::read_csv(diagnostic_file)
    } else {
      cli::cli_abort("Missing diagnostics file.")
    }

    flag_tab <- clean_flag_df(flag_tab)
    checkmate::assert_names(flag_tabs$diagnostic,
      must.include = flags_to_check
    )

    flags <- flag_tab |>
      dplyr::filter(.data$diagnostic %in% !!flags_to_check) |>
      dplyr::pull(.data$value)

    any_flags <- any(flags == TRUE)
  }

  forecast_path <- fs::path(dir,
    filename,
    ext = "parquet"
  )

  forecast <- NULL

  if (fs::file_exists(forecast_path)) {
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
#' @param dates A vector of forecast dates to pull
#' @param eval_data a tibble of hospital admissions evaluation
#' data to be used
#' for scoring.
#' @param model_types Character vector of model types to score.
#' One or both of `"ww"` and `"hosp"`. Default both: `c("ww", "hosp")`
#'
#' @return A large tibble containing crps scores for every location and
#' forecast date, conditioned on the presence of wastewater and model
#' convergence
#' @export
score_real_time_outputs <- function(score_type,
                                    real_time_output_dir,
                                    table_of_run_ids,
                                    locations,
                                    eval_data,
                                    model_types = c("ww", "hosp")) {
  checkmate::assert_scalar(score_type)
  checkmate::assert_names(score_type, subset.of = c("wis", "crps"))
  model_types <- unique(model_types)
  checkmate::assert_names(model_types, subset.of = c("ww", "hosp"))

  forecast_output_type <- c(
    "wis" = "quantiles",
    "crps" = "draws"
  )[[score_type]]
  ## remove trailing s from output type, col name is singular
  output_id_col <- stringr::str_sub(forecast_output_type,
    end = -2
  )

  score_problem <- function(forecast_date,
                            location,
                            model_type) {
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
            dplyr::rename(true_value = "daily_hosp_admits"),
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
          dplyr::filter(.data$date > .data$forecast_date) |>
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
#' @param other_real_time_scores the set we want them to be formatted like
#' @param truth_data_path a link to the truth data to create loc name
#'
#' @return a tibble formatted as the other real time scores for the real
#' time hosp only model
#' @export
format_scores_for_comparison <- function(real_time_scores,
                                         other_real_time_scores,
                                         truth_data_path = "https://media.githubusercontent.com/media/reichlab/covid19-forecast-hub/master/data-truth/truth-Incident%20Hospitalizations.csv") { # nolint

  loc_to_loc_name_table <- readr::read_csv(truth_data_path) |>
    dplyr::distinct(location, location_name)


  formatted_scores <- real_time_scores |>
    dplyr::filter(
      model == "hosp",
      scale == "log"
    ) |>
    dplyr::mutate(
      location = loc_abbr_to_flusight_code(location),
      model = "cfa-hosponlyrenewal(real-time)*",
      type = "quantile",
      days_ahead = as.numeric(date - forecast_date),
      target = glue::glue("{days_ahead} day ahead inc hosp"),
      horizon_days = as.integer(
        lubridate::ymd(date) - lubridate::ymd(forecast_date)
      ),
      horizon_weeks = ceiling(horizon_days / 7),
      horizon = glue::glue("{horizon_weeks} week ahead")
    ) |>
    dplyr::rename(
      target_end_date = date
    ) |>
    dplyr::left_join(
      loc_to_loc_name_table,
      by = "location"
    ) |>
    dplyr::select(
      colnames(other_real_time_scores)
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
    real_time_wis_hosp_only) {
  real_time_wis_ho <- real_time_wis_hosp_only |>
    dplyr::select(-failed_convergence)

  loc_map_table <- cfa_real_time_scores |>
    dplyr::distinct(location) |>
    dplyr::left_join(wweval::flusight_location_table,
      by = c("location" = "location_code")
    )

  rt_reformatted <- cfa_real_time_scores |>
    dplyr::rename(location_code = location) |>
    dplyr::left_join(loc_map_table,
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

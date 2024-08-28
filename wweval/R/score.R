#' Get the scores for ever day for a particular location and forecast date
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
      rename(
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
      data.table::as.data.table() |>
      scoringutils::transform_forecasts(
        fun = scoringutils::log_shift,
        offset = 1
      ) |>
      scoringutils::check_forecasts() |>
      scoringutils::score(metrics = metrics)
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
      rename(
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
      data.table::as.data.table() |>
      scoringutils::transform_forecasts(
        fun = scoringutils::log_shift,
        offset = 1
      ) |>
      scoringutils::check_forecasts() |>
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


#' Query Zoltar for models to include in the analysis
#'
#' @description
#' This function uses the `zoltr` R package to connect to the Zoltar database
#' which contains forecasts from the COVID Hub forecast project, and query it
#' for the specified forecast dates. It queries for all dates, computes the
#' proportion of dates for which the model has submitted, filters for models
#' that have submitted for greater than the specified proportion of forecast
#' dates for inclusion, and returns the vector of model names.
#'
#'
#' @param prop_dates_for_incl_hub Numeric less than 1 indicating the inclusion
#' threshold for the proportion of forecast dates that a model must have
#' submitted forecasts to be included in analysis
#' @param prop_locs_for_incl_hub Numeric less than 1 indicating the inclusion
#' threshold for the proportion of the locations we expect that a model
#' must have subbmited forecasts for to be included in analysis
#' @param forecast_dates vector of dates formatted in ISO8601 convention
#' (YYYY-MM-DD) indicating the forecast dates for the analysis
#' @param locations vector of state abbreviations that we want to ensure the
#' submitting teams have produced forecasts for
#' @param project_name name of the Zoltar project, default is
#' `"COVID-19 Forecasts"`
#'
#' @return a vector of character strings indicating the unique model names
#' that fit the inclusion criteria
#' @export
query_and_select_models <- function(prop_dates_for_incl_hub,
                                    prop_locs_for_incl_hub,
                                    forecast_dates,
                                    locations,
                                    project_name = "COVID-19 Forecasts") {
  # get state abbreviation codes
  state_codes <- cfaforecastrenewalww::loc_abbr_to_flusight_code(
    unique(locations)
  )

  if (prop_dates_for_incl_hub > 1) {
    cli::cli_abort(c(
      "Proportion of forecast dates required for hub inclusion",
      "must be less than 1."
    ))
  }

  if (prop_locs_for_incl_hub > 1) {
    cli::cli_abort(c(
      "Proportion of locations required for hub inclusion",
      "must be less than 1."
    ))
  }

  zoltar_connection <- zoltr::new_connection()
  zoltr::zoltar_authenticate(
    zoltar_connection, get_secret("Z_USERNAME"),
    get_secret("Z_PASSWORD")
  )

  # list of project on zoltar
  the_projects <- zoltr::projects(zoltar_connection)

  # Grabbing a specific project
  project_url <- the_projects[the_projects$name == !!project_name, "url"]
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
    # that the forecasts submitted have at leastr eached 28 days.
    targets = c("28 day ahead inc hosp"),
    types = "quantile",
    timezeros = forecast_dates
  )

  n_unique_forecasts <- forecast_data |>
    dplyr::distinct(timezero) |>
    dplyr::pull() |>
    length()

  n_forecasts_per_model <- forecast_data |>
    dplyr::distinct(timezero, model) |>
    dplyr::group_by(model) |>
    dplyr::summarize(
      n_forecast_dates = dplyr::n(),
      prop_present = n_forecast_dates / n_unique_forecasts
    )

  models <- n_forecasts_per_model |>
    dplyr::filter(prop_present > prop_dates_for_incl_hub) |>
    dplyr::pull(model)

  return(models)
}

#' Score hub submissions
#'
#' @param model_name a vector of character strings indicating the names of the
#' models to scores
#' @param dates a vector of dates indicating the dates of the submissions to score
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
#' @return a dataframe containing all of the scores for all models, forecast dates
#' (indcated by dates), locations, target end dates, and quantiles
#' @export
#'
score_hub_submissions <- function(model_name,
                                  dates,
                                  locations = NULL,
                                  hub_subdir = NA,
                                  pull_from_github = TRUE,
                                  submissions_path = "https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/master/data-processed/", # nolint
                                  truth_data_path = "https://media.githubusercontent.com/media/reichlab/covid19-forecast-hub/master/data-truth/truth-Incident%20Hospitalizations.csv") { # nolint

  truth_data <- truth_data <- readr::read_csv(truth_data_path)

  natural_scale_scores <- tibble::tibble()
  log_scale_scores <- tibble::tibble()
  missing_forecasts <- tibble::tibble()
  for (i in seq_along(model_name)) {
    for (j in seq_along(dates)) {
      this_forecast_date <- dates[j]
      this_model_name <- model_name[i]
      if (isTRUE(pull_from_github)) {
        quantiles <- tryCatch(
          readr::read_csv(glue::glue(
            "{submissions_path}{this_model_name}/{this_forecast_date}-{this_model_name}.csv"
          )) |>
            dplyr::filter(type == "quantile"),
          error = function(e) NULL
        )
      } else {
        quantiles <- readr::read_csv(
          file.path(
            hub_subdir, this_model_name,
            glue::glue("{this_forecast_date}-{this_model_name}.csv")
          )
        )
      }

      if (!is.null(quantiles)) {
        quantiles_w_truth <- quantiles |>
          dplyr::left_join(
            truth_data |> dplyr::rename(
              true_value = value
            ),
            by = c(
              "target_end_date" = "date",
              "location"
            )
          ) |>
          dplyr::rename(
            prediction = value
          ) |>
          dplyr::mutate(
            model = this_model_name
          )

        # Filter locations if they are specified, otherwise leave them all in
        if (!is.null(locations)) {
          quantiles_w_truth <- quantiles_w_truth |>
            dplyr::filter(location %in% cfaforecastrenewalww::loc_abbr_to_flusight_code(locations))
        }

        # Pass to scoring utils, no summaries just daily, quantiled scores
        these_natural_scale_scores <- quantiles_w_truth |>
          scoringutils::score(metrics = NULL) |>
          dplyr::mutate(horizon_days = as.integer(
            lubridate::ymd(target_end_date) - lubridate::ymd(forecast_date)
          )) |>
          dplyr::mutate(
            horizon_weeks =
              ceiling(horizon_days / 7)
          ) |>
          dplyr::mutate(horizon = glue::glue("{horizon_weeks} week ahead")) |>
          dplyr::select(-horizon_weeks, -horizon_days)

        these_log_scores <- quantiles_w_truth |>
          scoringutils::transform_forecasts(
            fun = scoringutils::log_shift,
            offset = 1
          ) |>
          scoringutils::score(metrics = NULL) |>
          dplyr::mutate(horizon_days = as.integer(
            lubridate::ymd(target_end_date) - lubridate::ymd(forecast_date)
          )) |>
          dplyr::mutate(
            horizon_weeks =
              ceiling(horizon_days / 7)
          ) |>
          dplyr::mutate(horizon = glue::glue("{horizon_weeks} week ahead")) |>
          dplyr::select(-horizon_weeks, -horizon_days)


        log_scale_scores <- dplyr::bind_rows(log_scale_scores, these_log_scores)
        natural_scale_scores <- dplyr::bind_rows(natural_scale_scores, these_natural_scale_scores)
      } else { # end if statement for quantiles empty
        these_missing_forecasts <- tibble(
          model = this_model_name,
          forecast_date = this_forecast_date
        )
        missing_forecasts <- dplyr::bind_rows(missing_forecasts, these_missing_forecasts)
      }
    } # end loop forecast dates
  } # end loop model name

  scores_list <- list(
    natural_scale_scores = natural_scale_scores,
    log_scale_scores = log_scale_scores,
    missing_forecasts = missing_forecasts
  )
  return(scores_list)
}

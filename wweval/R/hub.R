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
query_and_select_models <- function(
        prop_dates_for_incl_hub,
        prop_locs_for_incl_hub,
        forecast_dates,
        locations,
        project_name = "COVID-19 Forecasts"
) {
        assert_needed_env_vars(c("ZOLTAR_USERNAME", "ZOLTAR_PASSWORD"))
        # get state abbreviation codes
        state_codes <- forecasttools::us_loc_abbr_to_code(
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
                zoltar_connection,
                Sys.getenv("ZOLTAR_USERNAME"),
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

#' Pull hub submissions and create a scorable table
#' as the output of [scoringutils::as_forecast_quantile()].
#'
#' @param model_name a vector of character strings indicating
#' the names of the models to score.
#' @param dates a vector of dates indicating the dates of the
#' submissions to score.
#' @param eval_data Table of evaluation data, as the output of
#' [get_input_hosp_data()]
#' @param locations a vector of character strings indicating the locations
#' to score
#' @param hub_subdir path where the retrospective hub submissions are saved
#' locally since these are not on COVIDHub github
#' @param pull_from_github boolean indicating whether or not to pull
#' from github
#' @param submissions_path url pointing to the "data-processed" folder on
#' the COVIDhub github, which is where team's submissions are located
#'
#' @return a dataframe containing all of the scores for all models,
#' forecast dates (indicated by dates), locations, target end dates, and
#' quantiles
#' @export
#'
pull_hub_forecasts <- function(
        model_name,
        dates,
        eval_data,
        locations = NULL,
        hub_subdir = NA,
        pull_from_github = TRUE,
        submissions_path = "https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/master/data-processed/" # nolint
) {
        to_pull <- tidyr::crossing(
                model_name = model_name,
                forecast_date = dates
        )

        pull_model_date <- function(model_name, forecast_date) {
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
                                        hub_subdir,
                                        model_name,
                                        glue::glue(
                                                "{forecast_date}-{model_name}.csv"
                                        )
                                ),
                                show_col_types = FALSE
                        )
                }

                if (is.null(quantiles)) {
                        quantiles_w_truth <- tibble::tibble()
                } else {
                        quantiles_w_truth <- quantiles |>
                                dplyr::filter(!is.na(.data$quantile)) |>
                                dplyr::rename(
                                        predicted = "value",
                                        quantile_level = "quantile"
                                ) |>
                                dplyr::mutate(
                                        model = !!model_name,
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
                                        by = c("target_end_date", "location")
                                )
                }

                ## Filter locations if they are specified,
                ## otherwise leave them all in
                if (!is.null(locations)) {
                        quantiles_w_truth <- quantiles_w_truth |>
                                dplyr::filter(.data$location %in% !!locations)
                }

                if (nrow(quantiles_w_truth) > 0) {
                        result <- quantiles_w_truth |>
                                scoringutils::as_forecast_quantile(
                                        predicted = "predicted",
                                        observed = "observed",
                                        quantile_level = "quantile_level"
                                )
                } else {
                        result <- NULL
                }

                return(result)
        }

        all_forecasts <- purrr::pmap_df(to_pull, pull_model_date)

        return(all_forecasts)
}


#' Score hub forecasts
#'
#' @param hub_forecasts hub quantile forecasts, as the output
#' of [pull_hub_forecasts()].
#'
#' @return Data frame of scores, as the output of [scoringutils::score()].
#' @export
score_hub_forecasts <- function(hub_forecasts) {
        scores <- hub_forecasts |>
                scoringutils::transform_forecasts(
                        fun = scoringutils::log_shift,
                        offset = 1,
                        append = FALSE
                ) |>
                scoringutils::score() |>
                dplyr::mutate(
                        horizon_days = as.integer(
                                lubridate::ymd(.data$target_end_date) -
                                        lubridate::ymd(.data$forecast_date)
                        )
                ) |>
                dplyr::mutate(
                        horizon_weeks = .data$horizon_days %/% 7 + 1,
                        horizon = glue::glue("{horizon_weeks} week ahead")
                ) |>
                dplyr::select(-"horizon_weeks", -"horizon_days")
        return(scores)
}

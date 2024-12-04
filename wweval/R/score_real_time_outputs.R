#' Load in and score the real-time outputs
#'
#' @param score_type A string indicating which score to generate, either
#' "crps" or "wis". Note, if using crps, will score draws from nowcast and
#' forecast. If using wis, will score only the forecasts.
#' @param real_time_output_dir A string indicating the upper level directory
#' where the real-time outputs live locally
#' @param table_of_run_ids A tibble containing the forecast date, run id,
#' and date run for each of the production runs
#' @param locations A vector of character strings indicating the locations to
#' pull, this should be all jurisdictions
#' @param dates A vector of forecast dates to pull
#' @param eval_data a tibble of hospital admissions evaluation data to be used
#' for scoring.
#' @param hosp_only boolean indicating if we should only pull the hospital
#' admissions model
#'
#' @return A large tibble containing crps scores for every location and
#' forecast date, conditioned on the presence of wastewater and model
#' convergence
#' @export
score_real_time_outputs <- function(score_type,
                                    real_time_output_dir,
                                    table_of_run_ids,
                                    locations,
                                    dates,
                                    eval_data,
                                    hosp_only = FALSE) {
  if (isTRUE(hosp_only)) {
    model_types <- c("hosp")
  } else {
    model_types <- c("ww", "hosp")
  }
  all_scores <- c()

  data_type <- ifelse(score_type == "crps", "draws", "quantiles")
  col_name <- ifelse(score_type == "crps", "draw", "quantile")
  for (i in seq_along(dates)) {
    date_to_pull <- dates[i]
    metadata <- table_of_run_ids |> dplyr::filter(
      forecast_date == date_to_pull
    )
    run_id <- metadata$ids
    date_run <- metadata$dates_run
    for (j in seq_along(locations)) {
      for (m in seq_along(model_types)) {
        model_long <- ifelse(model_types[m] == "ww",
          "site-level infection dynamics",
          "hospital admissions only"
        )
        if (date_to_pull %in% c("2024-02-05", "2024-02-12")) {
          # Assume the old  file structure
          fp <- file.path(
            real_time_output_dir,
            glue::glue("output_{date_to_pull}"),
            "raw",
            locations[j],
            model_long,
            data_type,
            date_to_pull,
            glue::glue("run-on-{date_run}-{run_id}-{data_type}.parquet")
          )
          if (file.exists(fp)) {
            # The diagnostics are not flags here, just values.
            any_flags <- FALSE

            these_preds <- arrow::read_parquet(fp) |>
              dplyr::filter(
                name == "pred_hosp",
                period != "calibration"
              ) |>
              dplyr::select(
                forecast_date,
                date,
                location,
                value,
                !!sym(col_name)
              ) |>
              dplyr::mutate(
                model = model_types[m],
                failed_convergence = any_flags
              )
          } else {
            these_preds <- c()
          }
        } else {
          # Assume the main newer file structure
          dir <- file.path(
            real_time_output_dir,
            date_to_pull,
            glue::glue("run-on-{date_run}-{run_id}"),
            "raw",
            locations[j],
            model_long
          )

          if (file.exists(file.path(dir, glue::glue("{data_type}.parquet")))) {
            this_flags <- readr::read_csv(file.path(dir, "diagnostics.csv"))
            any_flags <- any(this_flags$value[20:23] == TRUE)

            these_preds <- arrow::read_parquet(
              file.path(dir, glue::glue("{data_type}.parquet"))
            ) |>
              dplyr::filter(
                name == "pred_hosp",
                period != "calibration"
              ) |>
              dplyr::select(
                forecast_date,
                date,
                location,
                value,
                !!sym(col_name)
              ) |>
              dplyr::mutate(
                model = model_types[m],
                failed_convergence = any_flags
              )
          } else {
            these_preds <- c()
          }
        } # end ifelse for file structures

        # Score the draws
        if (!is.null(these_preds)) {
          preds_w_eval <- these_preds |>
            dplyr::left_join(
              eval_data |>
                dplyr::select(-pop) |>
                dplyr::rename(true_value = daily_hosp_admits),
              by = c("location", "date")
            )

          # Pass to scoring utils
          if (score_type == "crps") {
            forecasted_preds <- preds_w_eval |>
              dplyr::rename(
                sample = draw,
                prediction = value,
              ) |>
              dplyr::select(
                location,
                forecast_date,
                date,
                true_value,
                prediction,
                sample,
                model,
                failed_convergence
              )
          } else if (score_type == "wis") {
            forecasted_preds <- preds_w_eval |>
              dplyr::rename(
                prediction = value,
              ) |>
              dplyr::select(
                location,
                forecast_date,
                date,
                true_value,
                prediction,
                quantile,
                model,
                failed_convergence
              ) |>
              dplyr::filter(
                date > forecast_date
              )
          }
          scores <- forecasted_preds |>
            data.table::as.data.table() |>
            scoringutils::transform_forecasts(
              fun = scoringutils::log_shift,
              offset = 1
            ) |>
            scoringutils::check_forecasts() |>
            scoringutils::score() |>
            tibble::tibble() |>
            dplyr::filter(scale == "log")
        } else {
          scores <- c()
        }


        all_scores <- dplyr::bind_rows(
          all_scores,
          scores
        )
      } # end loop around model types
    } # end loop around locs
  } # end loop around forecast dates


  return(all_scores)
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
      model = "cfa-hosponlyrenewal(real-time)",
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

#' Combine thehosp only real-time scores and cfa real
#' time scores from github
#'
#' @param cfa_real_time_scores Hub formatted scores for
#' only the ww model, `cfa-wwrenewal(real-time)`
#' @param real_time_wis_hosp_only hosp only
#' wis scores calculated from local data
#'
#' @return
#' @export
#'
#' @examples
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

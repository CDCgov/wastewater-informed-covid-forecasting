#' Load in and score the real-time outputs
#'
#'
#' @param real_time_output_dir A string indicating the upper level directory
#' where the real-time outputs live locally
#' @param table_of_run_ids A tibble containing the forecast date, run id,
#' and date run for each of the production runs
#' @param locations A vector of character strings indicating the locations to
#' pull, this should be all jurisdictions
#' @param dates A vector of forecast dates to pull
#' @param eval_data a tibble of hospital admissions evaluation data to be used
#' for scoring.
#'
#' @return A large tibble containing crps scores for every location and
#' forecast date, conditioned on the presence of wastewater and model
#' convergence
#' @export
load_and_score_rt_outputs <- function(real_time_output_dir,
                                      table_of_run_ids,
                                      locations,
                                      dates,
                                      eval_data) {
  model_types <- c("ww", "hosp")
  all_scores <- c()
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
        if (date_to_pull %in% c("2024-02-05", "2024-02-11")) {
          # Assume the old  file structure
          fp <- file.path(
            real_time_output_dir,
            "old_output",
            "raw",
            locations[j],
            "site-level infection dynamics",
            "draws",
            date_to_pull,
            glue::glue("run-on-{date_run}-{run_id}-draws.parquet")
          )
          fp_flags <- file.path(
            real_time_output_dir,
            "old_output",
            "raw",
            locations[j],
            model_long,
            "diagnostics",
            date_to_pull,
            glue::glue("run-on-{date_run}-{run_id}-diagnostics.csv")
          )
          if (file.exists(fp)) {
            this_flags <- readr::read_csv(fp_flags)
            any_flags <- any(this_flags$value[1:7] == TRUE)

            this_draws <- arrow::read_parquet(fp) |>
              dplyr::filter(
                name == "pred_hosp",
                period != "calibration"
              ) |>
              dplyr::select(
                forecast_date,
                date,
                location,
                value,
                draw
              ) |>
              dplyr::mutate(
                model = model_types[m],
                failed_convergence = any_flags
              )
          } else {
            this_draws <- c()
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

          if (file.exists(file.path(dir, "draws.parquet"))) {
            this_flags <- readr::read_csv(file.path(dir, "diagnostics.csv"))
            any_flags <- any(this_flags$value[1:7] == TRUE)

            this_draws <- arrow::read_parquet(file.path(dir, "draws.parquet")) |>
              dplyr::filter(
                name == "pred_hosp",
                period != "calibration"
              ) |>
              dplyr::select(
                forecast_date,
                date,
                location,
                value,
                draw
              ) |>
              dplyr::mutate(
                model = model_types[m],
                failed_convergence = any_flags
              )
          } else {
            this_draws <- c()
          }
        } # end ifelse for file structures

        # Score the draws
        draws_w_eval <- this_draws |>
          dplyr::left_join(
            eval_data |>
              dplyr::select(-pop) |>
              dplyr::rename(true_value = daily_hosp_admits),
            by = c("location", "date")
          )

        # Pass to scoring utils

        forecasted_draws <- draws_w_eval |>
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
            model
          )
        scores <- forecasted_draws |>
          data.table::as.data.table() |>
          scoringutils::transform_forecasts(
            fun = scoringutils::log_shift,
            offset = 1
          ) |>
          scoringutils::check_forecasts() |>
          scoringutils::score()

        all_scores <- all_scores |> dplyr::bind_rows(
          all_scores,
          scores
        )
      } # end loop around model types
    } # end loop around locs
  } # end loop around forecast dates


  return(all_scores)
}

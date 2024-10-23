#' Benchmark performance of wastewater and hospital admissions model
#' foreast runs
#'
#' @param ww_scores A tibble containing the scores for all target horizon days,
#' forecast dates, and locations for the wastewater model
#' @param hosp_scores A tibble containing the scores for all target horizon
#' days,forecast dates, and locations for the hospital admissions only model
#' @param benchmark_dir A directory indicating where to save the yaml file
#' containing tables and the git hash for benchmarking
#' @param benchmark_scope A string indicating verbally what the scores
#' are summarized over. Options are "all_forecasts" or "subset_of_forecasts"
#' @param overwrite_benchmarking A boolean indicating whether or not to
#' write the yaml to the benchmarking directory
#'
#' @return a list containing the metadata and 3 tables summarizing forecast
#' performance overall, by forecast date, and by location
#' @export
benchmark_performance <- function(ww_scores,
                                  hosp_scores,
                                  benchmark_dir,
                                  benchmark_scope,
                                  overwrite_benchmarking) {
  wweval_commit_hash <- system("git log --pretty=format:'%h' -n 1",
    intern = TRUE
  )

  overall_scores <- dplyr::bind_rows(
    ww_scores,
    hosp_scores
  ) |>
    dplyr::group_by(model) |>
    dplyr::summarize(
      crps = mean(crps),
      bias = mean(bias),
      ae = mean(ae_median)
    ) |>
    tidyr::pivot_wider(
      values_from = c("crps", "bias", "ae"),
      names_from = "model"
    )

  scores_by_forecast_date <- dplyr::bind_rows(
    ww_scores,
    hosp_scores
  ) |>
    dplyr::group_by(model, forecast_date) |>
    dplyr::summarize(
      crps = mean(crps),
      bias = mean(bias),
      ae = mean(ae_median)
    ) |>
    tidyr::pivot_wider(
      id_cols = forecast_date,
      values_from = c("crps", "bias", "ae"),
      names_from = "model"
    )

  scores_by_location <- dplyr::bind_rows(
    ww_scores,
    hosp_scores
  ) |>
    dplyr::group_by(model, location) |>
    dplyr::summarize(
      crps = mean(crps),
      bias = mean(bias),
      ae = mean(ae_median)
    ) |>
    tidyr::pivot_wider(
      id_cols = location,
      values_from = c("crps", "bias", "ae"),
      names_from = "model"
    )



  benchmarks <- list(
    wweval_commit_hash = wweval_commit_hash,
    overall_scores = overall_scores,
    scores_by_forecast_date = scores_by_forecast_date,
    scores_by_location = scores_by_location
  )

  if (isTRUE(overwrite_benchmarking)) {
    yaml::write_yaml(benchmarks, file = file.path(
      benchmark_dir,
      glue::glue("{benchmark_scope}.yaml")
    ))
  }


  return(benchmarks)
}

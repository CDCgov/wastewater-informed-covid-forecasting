benchmark_performance <- function(ww_scores,
                                  hosp_scores,
                                  benchmark_dir,
                                  benchmark_scope) {
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
    )



  benchmarks <- list(
    wweval_commit_hash = wweval_commit_hash,
    overall_scores = overall_scores,
    scores_by_forecast_date = scores_by_forecast_date,
    scores_by_location = scores_by_location
  )

  yaml::write_yaml(benchmarks, file = file.path(
    benchmark_dir,
    glue::glue("{benchmark_scope}.yaml")
  ))

  return(benchmarks)
}

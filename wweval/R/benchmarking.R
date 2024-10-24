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
#' @param wwinference_version Character string indicating the version
#' of the wwinference model being run
#' @param overwrite_benchmark A boolean indicating whether or not to
#' write the yaml to the benchmarking directory
#'
#' @return a list containing the metadata and 3 tables summarizing forecast
#' performance overall, by forecast date, and by location
#' @export
benchmark_performance <- function(ww_scores,
                                  hosp_scores,
                                  benchmark_dir,
                                  benchmark_scope,
                                  wwinference_version,
                                  overwrite_benchmark) {
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
    ) |>
    dplyr::mutate(
      location = "all",
      wweval_commit_hash = as.character(wweval_commit_hash),
      wwinference_version = as.character(wwinference_version),
      time_stamp = as.character(format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
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
    ) |>
    dplyr::mutate(
      wweval_commit_hash = as.character(wweval_commit_hash),
      forecast_date = lubridate::ymd(forecast_date),
      wwinference_version = as.character(wwinference_version),
      time_stamp = as.character(format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
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
    ) |>
    dplyr::mutate(
      wweval_commit_hash = as.character(wweval_commit_hash),
      wwinference_version = as.character(wwinference_version),
      time_stamp = as.character(format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
    ) |>
    dplyr::select(colnames(overall_scores)) |>
    dplyr::bind_rows(overall_scores)

  benchmarks <- list(
    scores_by_forecast_date = scores_by_forecast_date,
    scores_by_location = scores_by_location
  )


  if (isTRUE(overwrite_benchmark)) {
    readr::write_tsv(
      scores_by_forecast_date,
      file.path(
        benchmark_dir,
        glue::glue(
          "latest_{benchmark_scope}_by_forecast_date.tsv"
        )
      )
    )
    readr::write_tsv(
      scores_by_location,
      file.path(
        benchmark_dir,
        glue::glue(
          "latest_{benchmark_scope}_by_location.tsv"
        )
      )
    )

    # Read in and append scores by forecast_date
    if (file.exists(file.path(
      benchmark_dir,
      glue::glue(
        "{benchmark_scope}_by_forecast_date.tsv"
      )
    ))) {
      df <- readr::read_tsv(
        file.path(
          benchmark_dir,
          glue::glue(
            "{benchmark_scope}_by_forecast_date.tsv"
          )
        ),
        col_types = readr::cols(
          forecast_date = readr::col_date(),
          crps_hosp = readr::col_double(),
          crps_ww = readr::col_double(),
          bias_hosp = readr::col_double(),
          bias_ww = readr::col_double(),
          ae_hosp = readr::col_double(),
          ae_ww = readr::col_double(),
          wweval_commit_hash = readr::col_character(),
          wwinference_version = readr::col_character(),
          time_stamp = readr::col_character()
        )
      )

      # check that wwinference hash is different
      if (df$wwinference_version[1] != wwinference_version || df$wweval_commit_hash[1] != wweval_commit_hash) { # nolint
        df_to_append <- df
      } else {
        df_to_append <- tibble::tibble()
      }
    } else {
      df_to_append <- tibble::tibble()
    }


    readr::write_tsv(
      dplyr::bind_rows(scores_by_forecast_date, df_to_append),
      file.path(
        benchmark_dir,
        glue::glue(
          "{benchmark_scope}_by_forecast_date.tsv"
        )
      )
    )

    # Read in and append scores by loc
    if (file.exists(file.path(
      benchmark_dir,
      glue::glue(
        "{benchmark_scope}_by_location.tsv"
      )
    ))) {
      df <- readr::read_tsv(
        file.path(
          benchmark_dir,
          glue::glue(
            "{benchmark_scope}_by_location.tsv"
          )
        ),
        col_types = readr::cols(
          crps_hosp = readr::col_double(),
          crps_ww = readr::col_double(),
          bias_hosp = readr::col_double(),
          bias_ww = readr::col_double(),
          ae_hosp = readr::col_double(),
          ae_ww = readr::col_double(),
          location = readr::col_character(),
          wweval_commit_hash = readr::col_character(),
          wwinference_version = readr::col_character(),
          time_stamp = readr::col_character()
        )
      )
      # check that wwinference hash is different
      if (df$wwinference_version[1] != wwinference_version || df$wweval_commit_hash[1] != wweval_commit_hash) { # nolint
        df_to_append <- df
      } else {
        df_to_append <- tibble::tibble()
      }
    } else {
      df_to_append <- tibble::tibble()
    }

    readr::write_tsv(
      dplyr::bind_rows(scores_by_location, df_to_append),
      file.path(
        benchmark_dir,
        glue::glue(
          "{benchmark_scope}_by_location.tsv"
        )
      )
    )
  }


  return(benchmarks)
}

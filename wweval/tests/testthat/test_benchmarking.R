ww_scores <- tibble::tibble(
  location = c("x", "y"),
  forecast_date = rep("2023-10-15", 2),
  model = rep("ww", 2),
  crps = c(0.4, 0.5),
  bias = c(3, 4),
  ae_median = c(0.2, 0.1)
)

hosp_scores <- tibble::tibble(
  location = c("x", "y"),
  forecast_date = rep("2023-10-15", 2),
  model = rep("hosp", 2),
  crps = c(0.5, 0.6),
  bias = c(4, 5),
  ae_median = c(0.3, 0.2)
)

test_that("benchmarking writes files correctly", {
  benchmark_dir <- tempdir()


  write_files <- benchmark_performance(ww_scores,
    hosp_scores,
    benchmark_dir,
    benchmark_scope = "all",
    wwinference_version = "vtest",
    overwrite_benchmark = TRUE
  )

  df <- read.csv(file.path(benchmark_dir, "all_by_location.csv"))

  # append
  write_files_again <- benchmark_performance(ww_scores,
    hosp_scores,
    benchmark_dir,
    benchmark_scope = "all",
    wwinference_version = "vtest2",
    overwrite_benchmark = TRUE
  )

  df2 <- read.csv(file.path(benchmark_dir, "all_by_location.csv"))
  expect_equal(nrow(df) * 2, nrow(df2))
})

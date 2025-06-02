test_that("fit_hosp_trend() works as expected", {
  sim_r <- 0.05
  time <- 1:14
  init <- 50
  preds <- init * (1 + sim_r)^time
  withr::with_seed(5, {
    hosp <- stats::rnbinom(
      n = length(preds),
      mu = preds,
      size = 30
    )
  })

  data <- tibble::tibble(
    time = time,
    hosp = hosp
  )

  result <- fit_hosp_trend(
    data,
    0,
    0.05,
    0,
    1,
    10
  ) |>
    quiet()

  expect_true(brms::is.brmsfit(result))
})

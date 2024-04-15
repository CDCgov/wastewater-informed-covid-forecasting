sample_draws <- data.frame(
  date = as.Date("2023-01-01") + 0:4,
  draw = rnorm(5),
  location = rep("Location1", times = 5),
  value = rnorm(5),
  name = rep("Name1", times = 5),
  calib_data = c(3, 4, NA, NA, NA),
  forecast_date = as.Date("2023-01-03"),
  scenario = rep("Status quo", 5),
  model_type = rep("ww", 5)
)


# Test case: Check if the output has expected columns after processing
test_that("get_state_level_quantiles returns expected columns", {
  result <- get_state_level_quantiles(sample_draws)

  expected_columns <- c("quantile", "value", "name", "period")

  expect_true(all(expected_columns %in% names(result)))
})

# Test case: Check if period column is calculated correctly
test_that("Period column is calculated correctly in get_state_level_quantiles", {
  result <- get_state_level_quantiles(sample_draws)

  # Check if 'period' column has correct values based on 'date' and 'forecast_date'
  expect_true(all(result$period %in% c("calibration", "nowcast", "forecast")))

  # Ensure that rows with date <= forecast_date without calibration data are labeled as "nowcast"
  expect_true(all(result$period[result$date <= sample_draws$forecast_date & # nolint
    is.na(result$calib_data)] == "nowcast")) # nolint

  # Ensure that rows with date > forecast_date are labeled as "forecast"
  expect_true(all(result$period[result$date > sample_draws$forecast_date] == "forecast"))
})

# Test case: Check if the join preserves all unique combinations of 't' and 'name'
test_that("Join operation in get_state_level_quantiles preserves uniqueness", {
  result <- get_state_level_quantiles(sample_draws)

  unique_combinations <- unique(sample_draws[c("date", "name")])

  result_combinations <- unique(result[c("date", "name")])

  expect_equal(nrow(unique_combinations), nrow(result_combinations))
})

# Test case: Check if quantile levels are correctly assigned
test_that("Quantile levels are correctly assigned in get_state_level_quantiles", {
  result <- get_state_level_quantiles(sample_draws)

  # Assuming trajectories_to_quantiles returns a fixed set of quantile levels
  expected_quantile_levels <- c(0.025, 0.25, 0.5, 0.75, 0.975)

  expect_true(all(expected_quantile_levels %in% round(result$quantile, 4)))
})

# Test case: Check that there are no NAs in the date values
test_that("There aren't NAs where there shouldn't be, all dates compelte", {
  result <- get_state_level_quantiles(sample_draws)

  expect_true(!any(is.na(result$date)))
})

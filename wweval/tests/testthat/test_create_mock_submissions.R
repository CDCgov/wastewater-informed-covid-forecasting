# Create a mock dataset to use for testing

set.seed(123) # For reproducibility
# Create a full expanded grid with all combos
mock_all_scores_full <- expand.grid(
  forecast_date = seq(as.Date("2024-01-01"), by = "month", length.out = 3),
  scenario = c("base", "no_wastewater", "optimistic", "pessimistic"),
  location = c("State1", "State2")
) |> cbind(data.frame(score = runif(24, min = 0, max = 100)))

# Remove an entire state for a scenario, and then remove a random row
# simulating a scenario without WW in that location + a location with
# missign data for a single forecast date
mock_all_scores_incomplete <- mock_all_scores_full[-c(1, 2, 3, 9), ]

# Test that the function returns a data frame
test_that("Function returns a data frame", {
  result <- create_mock_submission_scores(mock_all_scores_full)
  expect_true(is.data.frame(result))
})

# Test that all combinations of forecast dates, locations, and scenarios are present
# when we pass in an incomplete dataframe
test_that("All combinations of dates, locations, and scenarios are present", {
  result <- create_mock_submission_scores(mock_all_scores_incomplete)

  expected_combos <- expand.grid(
    forecast_date = unique(mock_all_scores_incomplete$forecast_date),
    location = unique(mock_all_scores_incomplete$location),
    scenario = unique(mock_all_scores_incomplete$scenario)
  )

  actual_combos <- result |>
    dplyr::distinct(forecast_date, location, scenario) |>
    dplyr::arrange(forecast_date, location, scenario)

  expect_equal(nrow(actual_combos), nrow(expected_combos))
})



# Test that missing locations are filled with scores from the replacement model
test_that("Missing locations are filled with scores from replacement model", {
  # Run the function on incomplete data
  result <- create_mock_submission_scores(mock_all_scores_incomplete)

  # Check if all missing locations have been filled with 'no_wastewater' scenario scores
  no_wastewater_data <- filter(result, scenario == "no_wastewater")

  for (date in unique(no_wastewater_data$forecast_date)) {
    for (loc in unique(no_wastewater_data$location)) {
      for (scen in setdiff(unique(mock_all_scores_full$scenario), "no_wastewater")) {
        if (!any(mock_all_scores_incomplete$forecast_date == date &
          mock_all_scores_incomplete$location == loc & # nolint
          mock_all_scores_incomplete$scenario == scen)) { # nolint
          # If data was missing, check that it has been replaced correctly
          replaced_score <- filter(result, forecast_date == date, location == loc, scenario == scen)
          original_score <- filter(no_wastewater_data, forecast_date == date, location == loc)

          expect_equal(nrow(replaced_score), 1) # Ensure a replacement exists
          expect_equal(replaced_score$score, original_score$score)
          # Ensure the score matches the 'no_wastewater' score
        }
      }
    }
  }
})

# Test that the function fails when there are no scores to replace from the replacement model
test_that("Function fails when replacement model scores are unavailable", {
  # Create a situation where 'no_wastewater' scenario has no data at all
  no_replacement_scores <- mock_all_scores_incomplete[
    mock_all_scores_incomplete$scenario != "no_wastewater",
  ]

  expect_error(
    create_mock_submission_scores(no_replacement_scores),
    "Replacement scores unavailable"
  )
})

# Test that the function adds correct model_type labels to each row
test_that("Correct model_type labels are added", {
  result <- create_mock_submission_scores(mock_all_scores_incomplete)


  # Check if all scenarios have their respective model types labeled correctly
  unique_scenarios <- unique(mock_all_scores_full$scenario)

  for (scen in unique_scenarios) {
    scenario_data <- filter(result, scenario == scen)

    if (scen != "no_wastewater") {
      expect_true(all(scenario_data$model_type != "no_wastewater"))
    } else {
      expect_true(all(scenario_data$model_type == "no_wastewater"))
    }
  }
})

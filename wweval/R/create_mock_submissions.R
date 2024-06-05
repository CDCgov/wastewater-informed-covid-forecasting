#' Create mock submission scores
#'
#' @description For each forecast date, we want to create a mock submission to the Hub
#' so forecasts for all states + territories for each scenario. Since we
#' know the scores are independent and we just do a weighted average over them
#' to get an overall score, we can just take the scores directly from the
#' "no_wastewater" scenario whenever a wastewater score is missing,
#' add it to the dataframe, and continue to concatenate until we get a full
#' score dataframe with all combinations of forecast dates, locations, and
#' scenarios, labeled according to scenario. We will still know which ones
#' are from which model with the `model`` column
#'
#' @param all_scores This a dataframe of scores for all the successful model runs.
#' Because there are locations in each scenario where the wastewater data is
#' missing, this is not all the combinations of scenarios, forecast dates, and
#' locations
#' @param name_of_replacement_model the the string identifier in the `scenario`
#' of `all_scores` that should be used to replace the missing locations for
#' each scenario and forecast date. Default is `no_wastewater` as we will
#' typically use the hospital admissions only model for this.
#'
#' @return `all_submission_scores` which will be an expanded dataframe with
#' scores for each day with evaluation data for all combinations of
#' forecast dates, locations, and scenarios, with the name of replacement
#' model scores filling in when scores are missing.
#' @export

create_mock_submission_scores <- function(all_scores,
                                          name_of_replacement_model = "no_wastewater") {
  # For each forecast date, we want to create a mock submission to the Hub
  # (so forecasts for all states + territories) for each scenario. Since we
  # know the scores are independent and we just do a weighted average over them
  # to get an overall score, we can just take the scores directly from the
  # "no_wastewater" scenario whenever a wastewater score is missing,
  # add it to the dataframe, and continue to concatenate until we get a full
  # score dataframe with all combinations of forecast dates, locations, and
  # scenarios, labeled according to scenario. We will still know which ones
  # are from which model with the model_type

  forecast_dates <- unique(all_scores$forecast_date)
  scenarios <- unique(all_scores$scenario)
  locations <- unique(all_scores$location)

  all_submission_scores <- data.frame()
  for (i in seq_along(forecast_dates)) {
    for (j in seq_along(scenarios)) {
      scores_from_model <- all_scores |>
        dplyr::filter(
          scenario == scenarios[j],
          forecast_date == forecast_dates[i]
        )
      locs_present <- unique(scores_from_model$location)
      needed_locs <- locations[!locations %in% locs_present]

      replacement_scores <- all_scores |>
        dplyr::filter(
          scenario == {{ name_of_replacement_model }},
          forecast_date == forecast_dates[i],
          location %in% needed_locs
        ) |>
        dplyr::mutate(scenario = scenarios[j])

      submission_scores <- rbind(
        scores_from_model,
        replacement_scores
      )
      all_submission_scores <- rbind(all_submission_scores, submission_scores)
    }
  }

  n_combos <- all_submission_scores |>
    dplyr::select(forecast_date, location, scenario) |>
    unique() |>
    nrow()
  n_expected_combos <- length(forecast_dates) * length(locations) * length(scenarios)

  message("Number of expected combos:", n_expected_combos)
  message("Number of actual combos:", n_combos)

  # If there are locations/forecast dates with less than the unique number of
  # scenarios, this is because the hospital admissions only model didn't
  # converge. If this is the case, we probably want to exclude that location
  # forecast date bc we can't compare across scenarios.

  exclusions <- all_submission_scores |>
    dplyr::distinct(location, forecast_date, scenario) |>
    count(location, forecast_date) |>
    arrange(n) |>
    dplyr::filter(n < length(unique(all_submission_scores$scenario)))

  # Function that excludes rows based on one combination of exclusions
  exclude_combination <- function(df, exclusion) {
    filtered_df <- df |> dplyr::filter(
      !(location == exclusion$location & forecast_date == exclusion$forecast_date)
    )
    return(filtered_df)
  }

  # Apply the exclusion function for each row in the exclusions dataframe
  filtered_scores <- all_submission_scores
  for (i in seq_len(nrow(exclusions))) {
    filtered_scores <- exclude_combination(filtered_scores, exclusions[i, ])
  }

  # check that all ns are n_unique combos
  test <- filtered_scores |>
    dplyr::distinct(location, forecast_date, scenario) |>
    count(location, forecast_date) |>
    arrange(n)

  stopifnot(
    "Check that all locations forecast dates have full set of scenarios" =
      min(test$n) == length(unique(all_submission_scores$scenario))
  )


  return(filtered_scores)
}

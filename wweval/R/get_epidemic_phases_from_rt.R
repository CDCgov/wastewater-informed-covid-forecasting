#' Get epidemic phases from R(t)
#' @description
#' This function loads in the posterior estimate of the retrospective R(t)
#' from NNH. Then it categorizes each week into an epidemic phase based on the
#' algorithm used in https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1011200 #nolint
#' in the S5 appendix. Code available here: https://github.com/cdcepi/Evaluation-of-case-forecasts-submitted-to-COVID19-Forecast-Hub/blob/b3c379dfd48e8c673f67996014f151ce44cbd8fa/Code/Supplement%205_Rt_Epi%20Phases.R #nolint
#'
#'
#' @param locations A vector of the state abbreviations
#' @param retro_rt_path  A path to the parquet file of retrospective R(t) estimates
#' @param location_col_name A string indicating the name of the column
#' indicating location, default is `state_abb`
#' @param prob_threshold A numeric between 0 and 1 that defines the threshold
#' probability R(t) >= 1 or R(t) <= 1. Default is `0.9` from the above paper.
#'
#' @return df_epi_phase A dataframe containing the epidemic phase expanded
#' to be daily, for each location, based on the retrospective R(t) estimate.
#' @export
#'
get_epidemic_phases_from_rt <- function(locations,
                                        retro_rt_path,
                                        location_col_name = "state_abb",
                                        prob_threshold = 0.9) {
  retro_rt <- arrow::read_parquet(retro_rt_path) |>
    dplyr::rename(state_abbr = !!sym(location_col_name)) |>
    dplyr::filter(state_abbr %in% locations) |>
    dplyr::mutate(
      week_start_date = cut(reference_date, "week")
    )

  rt_initial_cat <- retro_rt |>
    dplyr::group_by(state_abbr, week_start_date) |>
    dplyr::summarise(
      prob_rt_greater_than_1 = mean(R > 1)
    ) |>
    dplyr::arrange(state_abbr, week_start_date) |>
    dplyr::mutate(
      trend = dplyr::case_when(
        prob_rt_greater_than_1 > prob_threshold ~ "increasing",
        prob_rt_greater_than_1 < 1 - prob_threshold ~ "decreasing",
        TRUE ~ "uncertain"
      )
    ) |>
    dplyr::group_by(state_abbr) |>
    dplyr::mutate(
      groups_phase = data.table::rleid(trend)
    ) |>
    dplyr::ungroup()

  summarized_by_trend <- rt_initial_cat |>
    dplyr::distinct(groups_phase, state_abbr, trend) |>
    dplyr::arrange(state_abbr, groups_phase) |>
    dplyr::group_by(state_abbr) |>
    dplyr::mutate(
      lag_phase = dplyr::lag(trend),
      lead_phase = dplyr::lead(trend),
      phase = dplyr::case_when(
        trend == "uncertain" & lag_phase == lead_phase ~ lead_phase, # nolint
        trend == "uncertain" & lag_phase == "decreasing" & lead_phase == "increasing" ~ "nadir", # nolint
        trend == "uncertain" & lag_phase == "increasing" & lead_phase == "decreasing" ~ "peak", # nolint
        TRUE ~ trend
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::select(state_abbr, groups_phase, phase)

  rt_cat <- rt_initial_cat |>
    dplyr::left_join(
      summarized_by_trend,
      by = c("state_abbr", "groups_phase")
    ) |>
    dplyr::select(state_abbr, phase, week_start_date)



  # Expand to daily and save only the necessary columns
  df_epi_phase <- retro_rt |>
    dplyr::distinct(reference_date, state_abbr, week_start_date) |>
    dplyr::left_join(
      rt_cat,
      by = c("state_abbr", "week_start_date")
    ) |>
    dplyr::select(state_abbr, reference_date, phase) |>
    dplyr::rename(
      location = state_abbr,
      date = reference_date
    )



  return(df_epi_phase)
}

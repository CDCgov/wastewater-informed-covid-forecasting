#' Process a dataframe of bootstrapped CRPS values.
#'
#' @param df Output of [rsample::bootstraps()] applied to a table of
#' relative scores for the wastewater model, with columns `crps` and
#' `mean_scores_ratio`.
#'
#' @return A tibble of replicate values for `crps_hosp` and `crps_ww`,
#' organized by bootstrap replicate id.
#' @keywords internal
.process_bstrap_samples <- function(df) {
  df |>
    dplyr::mutate(splits = purrr::map(.data$splits, as.data.frame)) |>
    tidyr::unnest(splits)
}


#' Summarize bootstrapped CRPS
#'
#' @param df Data frame of replicate CRPS values for the two models,
#' as the output of [.process_bstrap_samples()].
#' @param by Additional summarization variables besides `"id"`.
#' `c("id", by)` will be passed as the `.by` argument to
#' [dplyr::summarize()]. Default `NULL`.
#'
#' @return A summary of the data frame by replicate dataset `id` and optionally
#' other grouping variables, including the mean CRPS for the wastewater model
#' for each replicate dataset and group, the mean CRPS for the admissions-only model
#' for each replicate dataset and group, and the ratio of those two means for each
#' replicate dataset and group.
#' @keywords internal
.summarize_bstrap_crps <- function(df, by = NULL) {
  by <- c("id", by)
  return(dplyr::summarize(
    df,
    bstrap_crps_ww = mean(.data$crps_ww),
    bstrap_crps_hosp = mean(.data$crps_hosp),
    bstrap_rel_crps = .data$bstrap_crps_ww / .data$bstrap_crps_hosp,
    .by = !!by
  ))
}

#' Prepare bootstrap input from a table of paired absolute and relative retro
#' CPRS for the wastewater-informed and hospital-admissions only model.
#'
#' @param df table of paired CRPS values, with columns `model`, `crps`,
#' and `rel_crps`. Model column must include the retro wastewater-informed
#' model (`cfa-wwrenewal(retro)`).
#'
#' @return A table that can be passed to [bootstrap_crps_values()].
#'
#' @export
convert_to_boostrap_input <- function(df) {
  checkmate::assert_names(df$model, must.include = "cfa-wwrenewal(retro)")
  return(
    df |>
      dplyr::filter(.data$model == "cfa-wwrenewal(retro)") |>
      dplyr::rename(crps_ww = "crps") |>
      dplyr::mutate(crps_hosp = .data$crps_ww / .data$rel_crps)
  )
}

#' Create bootstrapped replicates to estimate uncertainty in
#' the mean CRPS for the admissions-only and wastewater-informed
#' models.
#'
#' Produces bootstrapped mean CRPS values for each value of the provided
#' grouping variable.
#'
#' @param scores data frame of relative scores for the wastewater
#' model with the admissions-only model as a baseline.
#' @param n_replicates Number of bootstrap replicate datasets to produce.
#' @param by Optional grouping column for bootstrap replicate datasets. Default
#' `NULL` (no grouping, produce replicates of the entire dataset).
#'
#' @return Tibble with the grouping column specified in `by` (if any)
#' and four other columns:
#'    - `id`: unique (within-group) identifier of the replicate dataset.
#'    - `bstrap_crps_ww`: mean CRPS for the wastewater model in that replicate dataset.
#'    - `bstrap_crps_hosp`: mean CRPS for the admissions-only model in that replicate dataset.
#'       sampled for that replicate.
#'    - `bstrap_rel_crps`: ratio (ww / hosp) of the mean CRPS values for the two models
#'       in that replicate dataset.
#'
#' @export
bootstrap_crps_values <- function(scores, n_replicates, by = NULL) {
  .do_bootstrap <- function(scores, grp) {
    rsample::bootstraps(scores, times = n_replicates) |>
      .process_bstrap_samples() |>
      .summarize_bstrap_crps()
  }

  samples <- scores |>
    dplyr::group_by(dplyr::pick(!!by)) |>
    dplyr::group_modify(.do_bootstrap)

  return(samples)
}

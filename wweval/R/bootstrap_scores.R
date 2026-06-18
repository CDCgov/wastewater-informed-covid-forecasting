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
    tidyr::unnest(splits) |>
    dplyr::rename(crps_ww = "crps") |>
    dplyr::mutate(crps_hosp = .data$crps_ww / .data$mean_scores_ratio)
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
  by = c("id", by)
  return(dplyr::summarize(
    df,
    bstrap_crps_ww = mean(.data$crps_ww),
    bstrap_crps_hosp = mean(.data$crps_hosp),
    bstrap_rel_crps = .data$bstrap_crps_ww / .data$bstrap_crps_hosp,
    .by = !!by
  ))
}

#' Compute the ratio of bootstrapped mean CRPS values for the two
#' models.
#'
#' @param df Data frame of bootstrapped CRPS values, as the output of
#' [.summarize_bstrap_crps()]
#' @param by Variables to summarize by when computing ratio of means.
#' Passed as the `.by` argument to [dplyr::summarize()]. Default `NULL`.
#'
#' @return table of the ratios
#'
#' @keywords internal
.get_ratio_of_bstrap_means <- function(df, by = NULL) {
  return(
    dplyr::summarize(
      df,
      ratio_of_bstrap_means = mean(.data$bstrap_crps_ww) /
        mean(.data$bstrap_crps_hosp),
      .by = !!by
    ) |>
      dplyr::arrange(.data$ratio_of_bstrap_means)
  )
}


#' Create bootstrapped replicates to estimate uncertainty in
#' the mean CRPS for the admissions-only and wastewater-informed
#' models.
#'
#' Produces bootstrapped values  mean CRPS
#' for each value of the provided grouping variable. Each
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

#' Plot bootstrapped CRPS ratios as pointintervals
#'
#' @param replicates Data frame of bootstrapped replicates,
#' as the output of [bootstrap_crps_values()].
#' @param by Stratification variable. Will become the x-axis
#' of the plot. Default `NULL` (plot a single point-interval.
#' @param connect_points Connect the points in the point intervals with lines?
#' Boolean, default `FALSE`.
#' @param order_by_point_estimate Order x-axis values by the value of the point estimate
#' (ascending)? Boolean, default `FALSE`.
#'
#' @return The plot, as a ggplot object.
#' @export
plot_bootstrapped_score_ratios <- function(
  replicates,
  by = NULL,
  connect_points = FALSE,
  order_by_point_estimate = FALSE
) {
  replicates <- dplyr::ungroup(replicates)
  if (is.null(by)) {
    by <- ".x_value_placeholder"
    replicates <- replicates |> dplyr::mutate(!!by := by)
  }

  point_estimates <- .get_ratio_of_bstrap_means(replicates, by = by)

  if (order_by_point_estimate) {
    replicates <- replicates |>
      dplyr::mutate(
        !!by := factor(
          .data[[by]],
          ordered = TRUE,
          levels = point_estimates[[by]]
        )
      )
  }

  dat_plot <- replicates |>
    dplyr::select(tidyselect::all_of(c("id", !!by, "bstrap_rel_crps"))) |>
    tidyr::pivot_longer("bstrap_rel_crps")

  point_estimate_geom <- if (connect_points) {
    forecasttools::geom_line_point
  } else {
    ggplot2::geom_point
  }

  plot <- dat_plot |>
    ggplot2::ggplot(ggplot2::aes(x = .data[[by]], y = .data$value)) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed", linewidth = 2) +
    ggdist::stat_pointinterval(show_point = FALSE) +
    point_estimate_geom(
      data = point_estimates,
      mapping = ggplot2::aes(y = .data$ratio_of_bstrap_means),
      shape = 21,
      size = 5,
      fill = "darkblue"
    ) +
    ggplot2::scale_y_continuous(transform = "log10") +
    ggplot2::coord_cartesian(
      ylim = forecasttools::sym_limits(dat_plot$value, transform = "log10")
    ) +
    get_plot_theme()

  if (by == ".x_value_placeholder") {
    plot <- plot +
      ggplot2::theme(
        axis.ticks.x = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_blank()
      )
  }

  return(plot)
}

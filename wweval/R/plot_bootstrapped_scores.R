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


#' Plot boostrapped absolute CRPS values as pointintervals.
#'
#' @param replicates Data frame of bootstrapped replicates,
#' as the output of [bootstrap_crps_values()].
#' @return The plot, as a ggplot object.
#' @export
plot_bootstrapped_score_values <- function(replicates) {
  replicates <- dplyr::ungroup(replicates)
  dat_plot <- replicates |>
      dplyr::select(tidyselect::all_of(c("id",
                                         "bstrap_crps_hosp",
                                         "bstrap_crps_ww"))) |>
      tidyr::pivot_longer(-"id") |>
      dplyr::mutate(name = dplyr::recode_values(.data$name,
                                                "bstrap_crps_hosp" ~ "cfa-hosponlyrenewal(retro)",
                                                "bstrap_crps_ww" ~ "cfa-wwrenewal(retro)",
                                                unmatched = "error") |>
                        factor(ordered = TRUE,
                               levels = c("cfa-wwrenewal(retro)",
                                          "cfa-hosponlyrenewal(retro)")))
  
  
  plot <- dat_plot |>
      ggplot2::ggplot(ggplot2::aes(x = .data$name,
                                   y = .data$value,
                                   fill = .data$name)) +
      ggdist::stat_pointinterval(point_interval = "mean_qi",
                                 shape = 21) +
      ggplot2::scale_y_continuous(transform = "log10") +
      scale_fill_model() +
      get_plot_theme()

  return(plot)
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

.plot_ww_trend_fit <- function(
  ww_fit,
  save_dir,
  figure_ext,
  n_lab_sites_plot
) {
  conditions <- dplyr::distinct(
    ww_fit$data,
    .data$lab_site_index
  ) |>
    head(n_lab_sites_plot)
  ww_plot <- brms::conditional_effects(
    ww_fit,
    "time",
    re_formula = NULL,
    conditions = conditions
  ) |>
    plot(points = TRUE)

  ww_plot <- ww_plot$time + theme_minimal()
  ggsave(
    filename = fs::path(
      save_dir,
      "ww_recent_trend_plot",
      ext = figure_ext
    ),
    plot = ww_plot
  )

  return(ww_plot)
}


.plot_hosp_trend_fit <- function(hosp_fit, save_dir, figure_ext) {
  hosp_plot <- brms::conditional_effects(
    hosp_fit,
    "time",
  ) |>
    plot(points = TRUE)
  hosp_plot <- hosp_plot$time +
    scale_y_continuous(transform = "log10") +
    theme_minimal()

  ggsave(
    filename = fs::path(
      save_dir,
      "hosp_recent_trend_plot",
      ext = figure_ext
    ),
    plot = hosp_plot
  )
  return(hosp_plot)
}

.spread_ww_trend_fit_draws <- function(ww_fit) {
  df <- ww_fit |>
    tidybayes::spread_draws(
      !!str2lang("b_time"),
      !!str2lang("sd_lab_site_index__time")
    ) |>
    dplyr::rename(
      global_slope_ww = "b_time",
      sd_slope_ww = "sd_lab_site_index__time"
    )
  return(df)
}

.spread_hosp_trend_fit_draws <- function(hosp_fit) {
  df <- hosp_fit |>
    tidybayes::spread_draws(
      !!str2lang("b_time"),
    ) |>
    dplyr::rename(
      global_slope_hosp = "b_time"
    )

  return(df)
}

#' Process BRMS log-linear fits of recent trends
#'
#' @param hosp_fit [brms::brmsfit] object for the hospital admissions trend.
#' @param ww_fit [brms::brmsfit] object for the wastewater trend.
#' @param save_dir Directory in which to save output
#' (figures and tidy posterior draws).
#' @param figure_ext File extension for figures, without the `.`,
#' e.g. `"pdf"` or `"png"`. Default `"pdf"`.
#' @param n_lab_sites_plot Maximum number of lab-sites for which to
#' plot wastewater trends. Default `10`.
#' @return NULL, invisibly saving postprocessing results to
#' disk as a side effect.
#' @export
process_recent_trend_fits <- function(
  hosp_fit,
  ww_fit,
  save_dir,
  figure_ext = "pdf",
  n_lab_sites_plot = 10
) {
  if (!is.null(ww_fit)) {
    .plot_ww_trend_fit(ww_fit, save_dir, figure_ext, n_lab_sites_plot)
  }

  if (!is.null(hosp_fit)) {
    .plot_hosp_trend_fit(hosp_fit, save_dir, figure_ext)
  }

  if (!is.null(ww_fit) && !is.null(hosp_fit)) {
    ww_trend_draws <- .spread_ww_trendfit_draws(ww_fit)
    hosp_trend_draws <- .spread_hosp_trendfit_draws(hosp_fit)
    trend_draws <- dplyr::inner_join(
      ww_trend_draws,
      hosp_trend_draws,
      by = c(".draw", ".iteration", ".chain")
    )
    readr::write_tsv(
      trend_draws,
      fs::path(save_dir, "trend_draws", ext = "tsv")
    )
  }

  invisible()
}

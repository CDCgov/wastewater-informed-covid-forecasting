#' Process BRMS log-linear fits of recent trends
#'
#' @param hosp_fit [brms::brmsfit] object for the hospital admissions trend.
#' @param ww_fit [brms::brmsfit] object for the wastewater trend.
#' @param figure_save_dir Directory in which to save output figures
#' @param figure_ext File extension for figures, without the `.`,
#' e.g. `"pdf"` or `"png"`.
#' @return NULL, invisibly saving postprocessing results to
#' disk as a side effect.
#' @export
process_recent_trend_fits <- function(
  hosp_fit,
  ww_fit,
  figure_save_dir,
  figure_ext = "pdf"
) {
  conditions <- dplyr::distinct(
    ww_fit$data,
    .data$lab_site_index
  ) |>
    head(10)
  ww_plot <- brms::conditional_effects(
    ww_fit,
    "time",
    re_formula = NULL,
    conditions = conditions
  ) |>
    plot(points = TRUE)

  ww_plot <- ww_plot$time + theme_minimal()

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
      figure_save_dir,
      "ww_recent_trend_plot",
      ext = figure_ext
    ),
    plot = ww_plot
  )
  ggsave(
    filename = fs::path(
      figure_save_dir,
      "hosp_recent_trend_plot",
      ext = ext
    ),
    plot = hosp_plot
  )

  invisible()
}

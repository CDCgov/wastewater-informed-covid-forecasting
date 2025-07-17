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
#' @param forecast_date forecast_date for the trend fits.
#' @param location Location for the trend fits.
#' @param scenario Wastewater data availability scenario for the
#' trend fits.
#' @param processed_output_dir Base directory for processed
#' output (processed output will be saved in a subdirectory specific
#' to the `forecast_date`, `location` and `scenario`. See
#' [forecast_output_path()].
#' @param figure_ext File extension for figures, without the `.`,
#' e.g. `"pdf"` or `"png"`. Default `"pdf"`.
#' @param n_lab_sites_plot Maximum number of lab-sites for which to
#' plot wastewater trends. Default `10`.
#' @param max_rhat maximum allowable r-hat value. Default 1.01
#' @param min_neff_ratio maximum allowable ratio of effective sample size
#' to nominal sample size. Default 0.1.
#' @param max_frac_divergent maximum allowable fraction of divergent
#' transitions. Default 0.01 (1%).
#' @return NULL, invisibly saving postprocessing results to
#' disk as a side effect.
#' @export
process_recent_trend_fits <- function(
  hosp_fit,
  ww_fit,
  forecast_date,
  location,
  scenario,
  processed_output_dir,
  figure_ext = "pdf",
  n_lab_sites_plot = 10,
  max_rhat = 1.01,
  min_neff_ratio = 0.1,
  max_frac_divergent = 0.01
) {
  save_dir <- forecast_output_path(
    processed_output_dir,
    scenario,
    forecast_date,
    "ww",
    location
  )
  .check_convergence <- function(fit) {
    samp_params <- rstan::get_sampler_params(fit$fit)
    fracs_divergent <- sapply(samp_params, \(x) mean(x[, "divergent__"]))
    print(fracs_divergent)
    rhat_ok <- all(brms::rhat(fit) < max_rhat)
    neff_ok <- all(brms::neff_ratio(fit) > min_neff_ratio)
    divergent_ok <- all(fracs_divergent < max_frac_divergent)

    return(rhat_ok && neff_ok && divergent_ok)
  }

  fs::dir_create(save_dir)

  valid_ww <- FALSE
  valid_hosp <- FALSE
  if (!is.null(ww_fit)) {
    .plot_ww_trend_fit(ww_fit, save_dir, figure_ext, n_lab_sites_plot)
    valid_ww <- .check_convergence(ww_fit)
  }

  if (!is.null(hosp_fit)) {
    .plot_hosp_trend_fit(hosp_fit, save_dir, figure_ext)
    valid_hosp <- .check_convergence(
      hosp_fit
    )
  }

  if (valid_ww && valid_hosp) {
    message("Extracting and joining draws...")
    ww_trend_draws <- .spread_ww_trend_fit_draws(ww_fit)
    hosp_trend_draws <- .spread_hosp_trend_fit_draws(hosp_fit)
    trend_draws <- dplyr::inner_join(
      ww_trend_draws,
      hosp_trend_draws,
      by = c(".draw", ".iteration", ".chain")
    ) |>
      dplyr::mutate(
        location = !!location,
        forecast_date = as.Date(!!forecast_date),
        scenario = !!scenario
      )
  } else {
    message("Fitting or convergence failure!")
    trend_draws <- tibble::tibble()
  }
  readr::write_tsv(
    trend_draws,
    fs::path(save_dir, "trend_draws", ext = "tsv")
  )

  invisible()
}

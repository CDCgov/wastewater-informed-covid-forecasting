#' Make a plot comparing the fit and forecasted hospital admissions
#' from the wastewater and hospital admissions model for a few
#' example states
#'
#' @param hosp_quantiles A tibble containing the calibrated hospital admissions
#' data, the evaluation hospital admissions data, and the quantiles of the
#' calibrated and forecasted admissions
#' @param loc_to_plot A  string indicating the state abbreviations of the state
#' to plot
#' @param date_to_plot A character string indicating what forecast date to plot,
#' in IS08601 format YYYY-MM-DD
#' @param n_forecast_days An integer indicating the number of days to show the
#' forecast for, default is `28`
#' @param n_calib_days An integer indicating the number of days to show the
#' calibration data for, default is `90`
#'
#' @return a ggplot object containing a faceted vertical plot of the
#' forecasts produced for each state, comparing the wastewater and hospital
#' admissions models
#' @export
make_fig2_hosp_t <- function(hosp_quantiles,
                             loc_to_plot,
                             date_to_plot,
                             n_forecast_days = 28,
                             n_calib_days = 90) {
  hosp <- hosp_quantiles |>
    dplyr::filter(location %in% c(!!loc_to_plot)) |>
    dplyr::filter(forecast_date == !!date_to_plot) |>
    dplyr::filter(
      date <= forecast_date + lubridate::days(n_forecast_days),
      date >= forecast_date - lubridate::days(n_calib_days)
    )

  quantiles_wide <- hosp |>
    dplyr::filter(quantile %in% c(0.025, 0.25, 0.5, 0.75, 0.975)) |>
    tidyr::pivot_wider(
      id_cols = c(
        location, forecast_date, period, scenario,
        date, t, eval_data, calib_data, model_type
      ),
      names_from = quantile,
      values_from = value
    )


  p <- ggplot(quantiles_wide) +
    geom_point(aes(x = date, y = eval_data),
      fill = "white", size = 1, shape = 21,
      show.legend = FALSE
    ) +
    geom_point(
      aes(x = date, y = calib_data),
      color = "black", show.legend = FALSE
    ) +
    geom_line(
      aes(
        x = date, y = `0.5`,
        color = model_type
      )
    ) +
    geom_ribbon(
      aes(
        x = date, ymin = `0.025`, ymax = `0.975`,
        fill = model_type
      ),
      alpha = 0.1
    ) +
    geom_ribbon(
      aes(
        x = date, ymin = `0.25`, ymax = `0.75`,
        fill = model_type
      ),
      alpha = 0.1,
    ) +
    geom_vline(aes(xintercept = lubridate::ymd(forecast_date)),
      linetype = "dashed"
    ) +
    scale_x_date(
      date_breaks = "2 weeks",
      labels = scales::date_format("%Y-%m-%d")
    ) +
    xlab("") +
    ylab("Daily hospital admissions") +
    scale_color_discrete() +
    scale_fill_discrete() +
    get_plot_theme(x_axis_dates = TRUE) +
    theme(
      legend.position = "top",
      legend.justification = "left"
    ) +
    labs(color = "Model", fill = "Model")

  return(p)
}


#' Make concentration fit and forecast figure
#'
#' @param ww_quantiles  A tibble containing the calibrated wastewater
#' concentrations, the evaluation wastewater concentration data, and the
#' quantiles of the calibrated and forecasted wastewater concentrations
#' @param loc_to_plot A character string indicating the state abbreviation
#' for which state to plot, can only be one state
#' @param date_to_plot A character string indicating what forecast date to plot,
#' in IS08601 format YYYY-MM-DD
#' @param n_forecast_days An integer indicating the number of days to show the
#' forecast for, default is `28`
#' @param n_calib_days An integer indicating the number of days to show the
#' calibration data for, default is `90`
#' @param max_n_site_labs_to_show An integer indicating the maximum number
#' of site-labs to show in the figure, default is `3`
#'
#' @return A ggplot object containing a faceted horizontal plot of the
#' calibrated and forecasted wastewater concentrations for 3 or fewer
#' site-lab combinations for a single state
#' @export
make_fig2_ct <- function(ww_quantiles,
                         loc_to_plot,
                         date_to_plot,
                         n_forecast_days = 28,
                         n_calib_days = 90,
                         max_n_site_labs_to_show = 3) {
  ww <- ww_quantiles |>
    dplyr::filter(location == !!loc_to_plot) |>
    dplyr::filter(forecast_date == !!date_to_plot) |>
    dplyr::filter(
      date <= forecast_date + lubridate::days(!!n_forecast_days),
      date >= forecast_date - lubridate::days(!!n_calib_days)
    ) |>
    dplyr::filter(lab_site_index <= !!max_n_site_labs_to_show)

  stopifnot(
    "This function is meant for one location" =
      length(unique(ww$location)) <= 1
  )

  quantiles_wide <- ww |>
    dplyr::mutate(log_conc = log(value)) |>
    dplyr::filter(quantile %in% c(0.025, 0.25, 0.5, 0.75, 0.975)) |>
    tidyr::pivot_wider(
      id_cols = c(
        location, site_lab_name, forecast_date, period, scenario,
        date, t, eval_data, calib_data
      ),
      names_from = quantile,
      values_from = log_conc
    )


  p <- ggplot(quantiles_wide) +
    geom_point(aes(x = date, y = log(eval_data)),
      fill = "white", size = 1, shape = 21,
      show.legend = FALSE
    ) +
    geom_point(
      aes(x = date, y = log(calib_data)),
      color = "black", show.legend = FALSE
    ) +
    geom_line(
      aes(
        x = date, y = `0.5`
      ),
      color = "#00BFC4"
    ) +
    geom_ribbon(
      aes(
        x = date, ymin = `0.025`, ymax = `0.975`
      ),
      alpha = 0.1,
      fill = "#00BFC4",
      show.legend = FALSE
    ) +
    geom_ribbon(
      aes(
        x = date, ymin = `0.25`, ymax = `0.75`,
      ),
      fill = "#00BFC4",
      alpha = 0.1,
      show.legend = FALSE
    ) +
    geom_vline(aes(xintercept = lubridate::ymd(forecast_date)),
      linetype = "dashed"
    ) +
    facet_grid(location ~ site_lab_name,
      scales = "free_y"
    ) +
    xlab("") +
    ylab("Log(genome copies per mL)") +
    scale_color_discrete() +
    scale_fill_discrete() +
    scale_x_date(
      date_breaks = "2 weeks",
      labels = scales::date_format("%Y-%m-%d")
    ) +
    get_plot_theme(x_axis_dates = TRUE)
  return(p)
}


#' Make figure 2
#'
#' @param hosp1 first hospital admissions forecast
#' @param hosp2 second
#' @param hosp3 third
#' @param ct1 first faceted fit to wastewater data in each site
#' @param ct2 second
#' @param ct3 third
#'
#' @return a combined ggplot object
#' @export
make_fig2 <- function(hosp1, hosp2, hosp3,
                      ct1, ct2, ct3) {
  patch <- hosp1 + ct1 +
    hosp2 + ct2 +
    hosp3 + ct3 +
    patchwork::plot_layout(
      guides = "collect",
      nrow = 3, ncol = 2,
      axes = "collect",
      widths = c(1, 1.5)
    ) & theme(
    legend.position = "top",
    legend.justification = "left"
  )
  return(patch)
}

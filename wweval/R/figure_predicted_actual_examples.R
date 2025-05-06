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
plot_pred_actual_hosp <- function(hosp_quantiles,
                                  loc_to_plot,
                                  date_to_plot,
                                  n_forecast_days = 28,
                                  n_calib_days = 90) {
  hosp <- hosp_quantiles |>
    dplyr::filter(.data$location %in% c(!!loc_to_plot)) |>
    dplyr::filter(.data$forecast_date == !!date_to_plot) |>
    dplyr::filter(
      .data$date <= .data$forecast_date +
        lubridate::days(!!n_forecast_days),
      .data$date >= .data$forecast_date -
        lubridate::days(!!n_calib_days)
    ) |>
    dplyr::select(
      "model_type",
      "forecast_date",
      "date",
      "calib_data",
      "observed",
      "quantile_level",
      "predicted"
    )

  quantiles_wide <- hosp |>
    dplyr::filter(.data$quantile_level %in% c(0.025, 0.25, 0.5, 0.75, 0.975)) |>
    tidyr::pivot_wider(
      names_from = "quantile_level",
      values_from = "predicted"
    )

  colors <- plot_components()

  p <- ggplot(quantiles_wide) +
    geom_point(
      aes(
        x = .data$date,
        y = .data$observed
      ),
      fill = "white", size = 1, shape = 21,
      show.legend = FALSE
    ) +
    geom_point(
      aes(
        x = .data$date,
        y = .data$calib_data
      ),
      color = "black",
      show.legend = FALSE
    ) +
    geom_line(
      aes(
        x = .data$date,
        y = .data$`0.5`,
        color = .data$model_type
      )
    ) +
    geom_ribbon(
      aes(
        x = .data$date,
        ymin = .data$`0.025`,
        ymax = .data$`0.975`,
        fill = .data$model_type
      ),
      alpha = 0.1
    ) +
    geom_ribbon(
      aes(
        x = .data$date,
        ymin = .data$`0.25`,
        ymax = .data$`0.75`,
        fill = .data$model_type
      ),
      alpha = 0.2,
    ) +
    geom_vline(aes(xintercept = lubridate::ymd(.data$forecast_date)),
      linetype = "dashed"
    ) +
    scale_x_date(
      date_breaks = "2 weeks",
      labels = scales::date_format("%Y-%m-%d")
    ) +
    xlab("") +
    ylab("Daily hospital admissions") +
    scale_color_manual(values = colors$model_colors) +
    scale_fill_manual(values = colors$model_colors) +
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
#' @param site_lab_names_to_show a vector of character strings indicating
#' the site lab names to be displayed in the plot. If NULL, the first
#' `max_n_site_labs_to_show` or all are displayed. Default is `NULL`.
#'
#' @return A ggplot object containing a faceted horizontal plot of the
#' calibrated and forecasted wastewater concentrations for 3 or fewer
#' site-lab combinations for a single state
#' @export
plot_pred_actual_ww <- function(ww_quantiles,
                                loc_to_plot,
                                date_to_plot,
                                n_forecast_days = 28,
                                n_calib_days = 90,
                                max_n_site_labs_to_show = 3,
                                site_lab_names_to_show = NULL) {
  if (!is.null(site_lab_names_to_show)) {
    ww_quantiles <- ww_quantiles |>
      dplyr::filter(site_lab_name %in% c(site_lab_names_to_show))
  } else {
    ww_quantiles <- ww_quantiles |>
      dplyr::filter(lab_site_index <= !!max_n_site_labs_to_show)
  }

  ww <- ww_quantiles |>
    dplyr::filter(location == !!loc_to_plot) |>
    dplyr::filter(forecast_date == !!date_to_plot) |>
    dplyr::filter(
      date <= forecast_date + lubridate::days(!!n_forecast_days),
      date >= forecast_date - lubridate::days(!!n_calib_days)
    )



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
        date, eval_data, calib_data, below_LOD, flag_as_ww_outlier
      ),
      names_from = "quantile",
      values_from = "log_conc"
    ) |>
    dplyr::mutate(
      model = "ww",
      observation_status =
        dplyr::case_when(
          flag_as_ww_outlier == 1 ~ "outlier",
          below_LOD == 1 ~ "below LOD",
          TRUE ~ "standard"
        )
    )



  colors <- plot_components()
  # Set ribbon and line color for model fit, in this case is always ww model
  model_color <- as.character(colors$model_colors["ww"])

  p <- ggplot(quantiles_wide) +
    geom_point(aes(x = date, y = log(eval_data)),
      fill = "white", size = 1, shape = 21,
      show.legend = FALSE
    ) +
    geom_point(
      aes(
        x = date, y = log(calib_data),
        color = observation_status,
        shape = observation_status
      ),
      show.legend = FALSE
    ) +
    geom_line(
      aes(
        x = date, y = `0.5`
      ),
      color = model_color,
      show.legend = FALSE
    ) +
    geom_ribbon(
      aes(
        x = date, ymin = `0.025`, ymax = `0.975`,
      ),
      fill = model_color,
      alpha = 0.2,
      show.legend = FALSE
    ) +
    geom_ribbon(
      aes(
        x = date, ymin = `0.25`, ymax = `0.75`
      ),
      fill = model_color,
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
    scale_x_date(
      date_breaks = "2 weeks",
      labels = scales::date_format("%Y-%m-%d")
    ) +
    get_plot_theme(x_axis_dates = TRUE) +
    scale_fill_manual(values = colors$observation_status_colors) +
    scale_color_manual(values = colors$observation_status_colors) +
    scale_shape_manual(values = colors$observation_status_shapes)
  return(p)
}
#' Make concentration fit and forecast figure for supplement
#'
#' @param ww_quantiles  A tibble containing the calibrated wastewater
#' concentrations, the evaluation wastewater concentration data, and the
#' quantiles of the calibrated and forecasted wastewater concentrations
#' @param loc_to_plot A character string indicating the state abbreviation
#' for which state to plot, can only be one state
#' @param date_to_plot A character string indicating what forecast date to plot,
#' in IS08601 format YYYY-MM-DD
#' @param ms_fig_dir A string indicating where to save the figure
#' @param n_forecast_days An integer indicating the number of days to show the
#' forecast for, default is `28`
#' @param n_calib_days An integer indicating the number of days to show the
#' calibration data for, default is `90`
#' @param max_n_site_labs_to_show An integer indicating the maximum number
#' of site-labs to show in the figure, default is `3`
#' @param site_lab_names_to_show a vector of character strings indicating
#' the site lab names to be displayed in the plot. If NULL, the first
#' `max_n_site_labs_to_show` or all are displayed. Default is `NULL`.
#'
#' @return A ggplot object containing a faceted horizontal plot of the
#' calibrated and forecasted wastewater concentrations for 3 or fewer
#' site-lab combinations for a single state
#' @export
make_fig2_ct_supp <- function(ww_quantiles,
                              loc_to_plot,
                              date_to_plot,
                              ms_fig_dir,
                              n_forecast_days = 28,
                              n_calib_days = 90,
                              max_n_site_labs_to_show = 3,
                              site_lab_names_to_show = NULL) {
  if (!is.null(site_lab_names_to_show)) {
    ww_quantiles <- ww_quantiles |>
      dplyr::filter(site_lab_name %in% c(site_lab_names_to_show))
  } else {
    ww_quantiles <- ww_quantiles |>
      dplyr::filter(lab_site_index <= !!max_n_site_labs_to_show)
  }

  ww <- ww_quantiles |>
    dplyr::filter(location == !!loc_to_plot) |>
    dplyr::filter(forecast_date == !!date_to_plot) |>
    dplyr::filter(
      date <= forecast_date + lubridate::days(!!n_forecast_days),
      date >= forecast_date - lubridate::days(!!n_calib_days)
    )



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
        date, eval_data, calib_data, below_LOD, flag_as_ww_outlier
      ),
      names_from = quantile,
      values_from = log_conc
    ) |>
    dplyr::mutate(
      model = "ww",
      observation_status =
        dplyr::case_when(
          flag_as_ww_outlier == 1 ~ "outlier",
          below_LOD == 1 ~ "below LOD",
          TRUE ~ "standard"
        )
    )



  colors <- plot_components()
  # Set ribbon and line color for model fit, in this case is always ww model
  model_color <- as.character(colors$model_colors["ww"])

  p <- ggplot(quantiles_wide) +
    geom_point(aes(x = date, y = log(eval_data)),
      fill = "white", size = 1, shape = 21,
      show.legend = FALSE
    ) +
    geom_point(
      aes(
        x = date, y = log(calib_data),
        color = observation_status,
        shape = observation_status
      ),
      show.legend = FALSE
    ) +
    geom_line(
      aes(
        x = date, y = `0.5`
      ),
      color = model_color,
      show.legend = FALSE
    ) +
    geom_ribbon(
      aes(
        x = date, ymin = `0.025`, ymax = `0.975`,
      ),
      fill = model_color,
      alpha = 0.2,
      show.legend = FALSE
    ) +
    geom_ribbon(
      aes(
        x = date, ymin = `0.25`, ymax = `0.75`
      ),
      fill = model_color,
      alpha = 0.1,
      show.legend = FALSE
    ) +
    geom_vline(aes(xintercept = lubridate::ymd(forecast_date)),
      linetype = "dashed"
    ) +
    facet_wrap(~site_lab_name,
      scales = "free_y"
    ) +
    xlab("") +
    ylab("Log(genome copies per mL)") +
    scale_x_date(
      date_breaks = "2 weeks",
      labels = scales::date_format("%Y-%m-%d")
    ) +
    get_plot_theme(
      x_axis_dates = TRUE,
      x_axis_text_size = 6
    ) +
    scale_fill_manual(values = colors$observation_status_colors) +
    scale_color_manual(values = colors$observation_status_colors) +
    scale_shape_manual(values = colors$observation_status_shapes)

  ggsave(p,
    height = 5,
    width = 7,
    filename = file.path(
      ms_fig_dir,
      glue::glue("sfig_ww_conc_ex_{loc_to_plot}.png")
    )
  )
  return(p)
}


#' Make figure 2
#'
#' @param hosp1 first hospital admissions predicted-actual figure
#' @param hosp2 second hospital admissions predicted-actual figure
#' @param hosp3 third hospital admissions predicted-actual figure
#' @param ww1 first wastewater predicted-actual figure
#' @param ww2 second wastewater predicted-actual figure
#' @param ww3 third wastewater predicted-actual figure
#' @param fig_file_dir Path to save figures
#'
#' @return a combined ggplot object
#' @export
multi_location_pred_actual_fig <- function(hosp1, hosp2, hosp3,
                                           ww1, ww2, ww3,
                                           fig_file_dir) {
  fig <- patchwork::wrap_plots(
    hosp1,
    ww1,
    hosp2,
    ww2,
    hosp3,
    ww3,
    guides = "collect",
    nrow = 3,
    ncol = 2,
    axes = "collect",
    widths = c(1, 1.5)
  ) & theme(
    legend.position = "top",
    legend.justification = "left"
  )

  fs::dir_create(fig_file_dir)
  ggsave(fig,
    filename = file.path(fig_file_dir, "fig_pred_actual_examples.png"),
    width = 10, height = 7,
    create.dir = TRUE
  )
  ggsave(fig2,
    filename = file.path(fig_file_dir, "fig_pred_actual_examples.svg"),
    width = 10, height = 7,
    create.dir = TRUE
  )
  return(fig)
}

#' Plots for visualizing postperior predictive draws or quantiles
#' compared to observed data.

#' Spaghetti plot of hospital admissions data compared to model draws
#'
#' @param draws_w_data A long tidy dataframe containing draws from the
#' model of the estimated hospital admissions joined with both the data
#' the model was calibrated to and the later observed data.
#' @param location the jursidiction the data is from
#' @param model_type type of model the output is from, options are
#' "ww" or "hosp"
#' @param n_draws number of draws to plot, default = 100
#'
#' @return a ggplot object showing posterior predictive draws for
#' hospital admissions alongside the calibration and evaluation data
#' @export
plot_spaghetti_hosp_draws <- function(
  draws_w_data,
  location,
  model_type = c("ww", "hosp"),
  n_draws = 100
) {
  model_type <- arg_match(model_type)
  sampled_draws <- sample(1:max(draws_w_data$draw), n_draws)
  draws_w_data_subsetted <- draws_w_data |>
    dplyr::filter(
      draw %in% !!sampled_draws,
      name == "pred_hosp"
    )

  plot_color <- ifelse(model_type == "ww", "cornflowerblue", "purple4")

  p <- ggplot(draws_w_data_subsetted) +
    geom_line(
      aes(x = date, y = value, group = draw),
      color = plot_color,
      linewidth = 0.2,
      alpha = 0.4,
      show.legend = FALSE
    ) +
    geom_point(
      aes(x = date, y = eval_data),
      fill = "white",
      size = 1,
      shape = 21,
      show.legend = FALSE
    ) +
    geom_point(
      aes(x = date, y = calib_data),
      color = "black",
      show.legend = FALSE
    ) +
    geom_vline(
      aes(xintercept = lubridate::ymd(forecast_date)),
      linetype = "dashed"
    ) +
    # scale_y_continuous(trans = "log10") +
    xlab("") +
    ylab("Daily hospital admissions") +
    ggtitle(glue::glue(
      "Site-level expected observed hospital admissions in {location} from {model_type} model"
    )) +
    theme_bw() +
    scale_color_discrete() +
    scale_fill_discrete() +
    scale_x_date(
      date_breaks = "2 weeks",
      labels = scales::date_format("%Y-%m-%d")
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(
        size = 8,
        vjust = 1,
        hjust = 1,
        angle = 45
      ),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      plot.title = element_text(
        size = 10,
        vjust = 0.5,
        hjust = 0.5
      )
    )
  return(p)
}


#' Get plot of wastewater data compared to model posterior predictive
#' draws
#'
#' @param draws_w_data A long tidy dataframe containing posterior
#' predictivedraws from the for wastewater concentration joined to
#' both the calibration and evaluation data.
#' @param location the jurisdiction the data is from
#' @param model_type type of model the output is from, default is `ww`
#' @param n_draws number of draws to plot, default = 100
#' @return a ggplot object faceted by site showing the draws
#' @export
plot_spaghetti_ww_draws <- function(
  draws_w_data,
  location,
  model_type = "ww",
  n_draws = 100
) {
  sampled_draws <- sample(1:max(draws_w_data$draw), n_draws)
  draws_w_data_subsetted <- draws_w_data |>
    dplyr::filter(
      draw %in% !!sampled_draws,
      name == "pred_ww"
    )

  p <- ggplot(draws_w_data_subsetted) +
    geom_line(
      aes(
        x = .data$date,
        y = .data$value,
        group = .data$draw,
        color = .data$site_lab_name
      ),
      linewidth = 0.1,
      alpha = 0.1,
      show.legend = FALSE
    ) +
    geom_point(
      aes(x = .data$date, y = .data$eval_data),
      fill = "white",
      size = 1,
      shape = 21,
      show.legend = FALSE
    ) +
    geom_point(
      aes(x = .data$date, y = .data$calib_data),
      color = "black",
      show.legend = FALSE
    ) +
    geom_vline(
      aes(xintercept = lubridate::ymd(.data$forecast_date)),
      linetype = "dashed"
    ) +
    scale_y_continuous(trans = "log10") +
    facet_wrap(~site_lab_name, scales = "free_y") +
    geom_point(
      data = draws_w_data_subsetted |> filter(.data$below_LOD == 1),
      aes(x = .data$date, y = .data$calib_data),
      color = "red",
      size = 1.1
    ) +
    geom_point(
      data = draws_w_data_subsetted |>
        filter(.data$flag_as_ww_outlier == 1),
      aes(x = .data$date, y = .data$calib_data),
      color = "blue",
      size = 1.1
    ) +
    xlab("") +
    ylab("Genome copies per mL") +
    ggtitle(glue::glue(
      "Site-level expected observed wastewater concentration in {location} from {model_type} model"
    )) +
    theme_bw() +
    scale_color_discrete() +
    scale_fill_discrete() +
    scale_x_date(
      date_breaks = "2 weeks",
      labels = scales::date_format("%Y-%m-%d")
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(
        size = 8,
        vjust = 1,
        hjust = 1,
        angle = 45
      ),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      plot.title = element_text(
        size = 10,
        vjust = 0.5,
        hjust = 0.5
      )
    )
  return(p)
}

#' Make a plot comparing the fit and forecasted hospital admissions
#' from the wastewater-informed and hospital admissions-only models.
#'
#' @param hosp_quantiles A tibble containing the calibrated
#' hospital admissions data, the evaluation hospital
#' admissions data, and the quantiles of the calibrated
#' and forecasted admissions, for either or both the
#' wastewater model and the hospital admissions-only model.
#' @param loc_to_plot A  string indicating the state abbreviation
#' of the state to plot.
#' @param date_to_plot A character string indicating what
#' forecast date to plot in IS08601 format (YYYY-MM-DD).
#' @param n_forecast_days An integer indicating the number of days
#' to show the forecast for, default is `28`.
#' @param n_calib_days An integer indicating the number of days
#' to show the calibration data for, default is `90`.
#'
#' @return a ggplot object with all model forecasts plotted against
#' observed data and models differentiated by fill/line color.
#' @export
plot_ribbon_hosp_quantiles <- function(
  hosp_quantiles,
  loc_to_plot,
  date_to_plot,
  n_forecast_days = 28,
  n_calib_days = 90
) {
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
        "location",
        "forecast_date",
        "period",
        "scenario",
        "date",
        "eval_data",
        "calib_data",
        "model_type"
      ),
      names_from = quantile,
      values_from = value
    )

  p <- ggplot(quantiles_wide) +
    geom_point(
      aes(x = .data$date, y = .data$eval_data),
      fill = "white",
      size = 1,
      shape = 21,
      show.legend = FALSE
    ) +
    geom_point(
      aes(x = .data$date, y = .data$calib_data),
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
    geom_vline(
      xintercept = lubridate::ymd(date_to_plot),
      linetype = "dashed"
    ) +
    scale_x_date(
      date_breaks = "2 weeks",
      labels = scales::date_format("%Y-%m-%d")
    ) +
    xlab("") +
    ylab("Daily hospital admissions") +
    scale_color_model() +
    scale_fill_model() +
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
plot_ribbon_ww_quantiles <- function(
  ww_quantiles,
  loc_to_plot,
  date_to_plot,
  n_forecast_days = 28,
  n_calib_days = 90,
  max_n_site_labs_to_show = 3,
  site_lab_names_to_show = NULL
) {
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
    "This function is meant for one location" = length(unique(ww$location)) <= 1
  )

  quantiles_wide <- ww |>
    dplyr::filter(quantile %in% c(0.025, 0.25, 0.5, 0.75, 0.975)) |>
    tidyr::pivot_wider(
      id_cols = c(
        "location",
        "site_lab_name",
        "forecast_date",
        "period",
        "scenario",
        "date",
        "eval_data",
        "calib_data",
        "below_LOD",
        "flag_as_ww_outlier"
      ),
      names_from = quantile,
      values_from = value
    ) |>
    dplyr::mutate(
      model = "ww",
      observation_status = dplyr::case_when(
        .data$flag_as_ww_outlier == 1 ~ "outlier",
        .data$below_LOD == 1 ~ "below LOD",
        TRUE ~ "standard"
      )
    )

  colors <- plot_components()
  ## Set ribbon and line color for model fit,
  ## in this case is always ww model
  model_color <- as.character(colors$model_colors["ww"])

  p <- ggplot(quantiles_wide) +
    geom_point(
      aes(x = .data$date, y = .data$eval_data),
      fill = "white",
      size = 1,
      shape = 21,
      show.legend = FALSE
    ) +
    geom_point(
      aes(
        x = .data$date,
        y = .data$calib_data,
        color = .data$observation_status,
        shape = .data$observation_status
      ),
      show.legend = FALSE
    ) +
    geom_line(
      aes(x = .data$date, y = .data$`0.5`),
      color = model_color,
      show.legend = FALSE
    ) +
    geom_ribbon(
      aes(
        x = .data$date,
        ymin = .data$`0.025`,
        ymax = .data$`0.975`
      ),
      fill = model_color,
      alpha = 0.2,
      show.legend = FALSE
    ) +
    geom_ribbon(
      aes(
        x = .data$date,
        ymin = .data$`0.25`,
        ymax = .data$`0.75`
      ),
      fill = model_color,
      alpha = 0.1,
      show.legend = FALSE
    ) +
    geom_vline(
      xintercept = lubridate::ymd(date_to_plot),
      linetype = "dashed"
    ) +
    facet_wrap(~ .data$site_lab_name, scales = "free_y") +
    xlab("") +
    ylab("Genome copies per mL") +
    scale_x_date(
      date_breaks = "2 weeks",
      labels = scales::date_format("%Y-%m-%d")
    ) +
    scale_y_continuous(transform = "log10") +
    get_plot_theme(x_axis_dates = TRUE) +
    scale_fill_manual(values = colors$observation_status_colors) +
    scale_color_manual(values = colors$observation_status_colors) +
    scale_shape_manual(values = colors$observation_status_shapes)
  return(p)
}


#' Make a plot comparing the fit and forecasted hospital admissions
#' from the wastewater and hospital admissions model for a few
#' example states
#'
#' @param hosp_quantiles Tibble of posterior predictive quantiles for hospital admissions,
#' with observed (calibration and evaluation) data.
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
plot_pred_actual_hosp <- function(
  hosp_quantiles,
  loc_to_plot,
  date_to_plot,
  n_forecast_days = 28,
  n_calib_days = 90
) {
  hosp <- hosp_quantiles |>
    dplyr::filter(.data$location %in% c(!!loc_to_plot)) |>
    dplyr::filter(.data$forecast_date == !!date_to_plot) |>
    dplyr::filter(
      .data$date <=
        .data$forecast_date +
          lubridate::days(!!n_forecast_days),
      .data$date >=
        .data$forecast_date -
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

  p <- ggplot(quantiles_wide) +
    geom_point(
      aes(
        x = .data$date,
        y = .data$observed
      ),
      fill = "white",
      size = 1,
      shape = 21,
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
    geom_vline(
      aes(xintercept = lubridate::ymd(.data$forecast_date)),
      linetype = "dashed"
    ) +
    scale_x_date(
      date_breaks = "2 weeks",
      labels = scales::date_format("%Y-%m-%d")
    ) +
    xlab("") +
    ylab("Daily hospital admissions") +
    scale_color_model() +
    scale_fill_model() +
    get_plot_theme(x_axis_dates = TRUE) +
    theme(
      legend.position = "top",
      legend.justification = "left"
    ) +
    labs(color = "Model", fill = "Model")

  return(p)
}


#' Make wastewater concentration fit and forecast figure
#'
#' @param ww_quantiles Tibble of posterior predictive quantiles for
#' wastewater concentrations, along with calibration and evaluation data.
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
plot_pred_actual_ww <- function(
  ww_quantiles,
  loc_to_plot,
  date_to_plot,
  n_forecast_days = 28,
  n_calib_days = 90,
  max_n_site_labs_to_show = 3,
  site_lab_names_to_show = NULL
) {
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
    "This function is meant for one location" = length(unique(ww$location)) <= 1
  )

  quantiles_wide <- ww |>
    dplyr::mutate(log_conc = log(value)) |>
    dplyr::filter(quantile %in% c(0.025, 0.25, 0.5, 0.75, 0.975)) |>
    tidyr::pivot_wider(
      id_cols = c(
        location,
        site_lab_name,
        forecast_date,
        period,
        scenario,
        date,
        eval_data,
        calib_data,
        below_LOD,
        flag_as_ww_outlier
      ),
      names_from = "quantile",
      values_from = "log_conc"
    ) |>
    dplyr::mutate(
      model = "ww",
      observation_status = dplyr::case_when(
        flag_as_ww_outlier == 1 ~ "outlier",
        below_LOD == 1 ~ "below LOD",
        TRUE ~ "standard"
      )
    )

  colors <- plot_components()
  # Set ribbon and line color for model fit, in this case is always ww model
  model_color <- as.character(colors$model_colors["ww"])

  p <- ggplot(quantiles_wide) +
    geom_point(
      aes(x = date, y = log(eval_data)),
      fill = "white",
      size = 1,
      shape = 21,
      show.legend = FALSE
    ) +
    geom_point(
      aes(
        x = date,
        y = log(calib_data),
        color = observation_status,
        shape = observation_status
      ),
      show.legend = FALSE
    ) +
    geom_line(
      aes(
        x = date,
        y = `0.5`
      ),
      color = model_color,
      show.legend = FALSE
    ) +
    geom_ribbon(
      aes(
        x = date,
        ymin = `0.025`,
        ymax = `0.975`,
      ),
      fill = model_color,
      alpha = 0.2,
      show.legend = FALSE
    ) +
    geom_ribbon(
      aes(
        x = date,
        ymin = `0.25`,
        ymax = `0.75`
      ),
      fill = model_color,
      alpha = 0.1,
      show.legend = FALSE
    ) +
    geom_vline(
      aes(xintercept = lubridate::ymd(forecast_date)),
      linetype = "dashed"
    ) +
    facet_grid(location ~ site_lab_name, scales = "free_y") +
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

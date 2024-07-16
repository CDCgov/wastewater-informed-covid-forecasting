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
    dplyr::filter(location %in% c(loc_to_plot)) |>
    dplyr::filter(forecast_date == date_to_plot) |>
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
      alpha = 0.1,
      show.legend = FALSE
    ) +
    geom_ribbon(
      aes(
        x = date, ymin = `0.25`, ymax = `0.75`,
        fill = model_type
      ),
      alpha = 0.1,
      show.legend = FALSE
    ) +
    geom_vline(aes(xintercept = lubridate::ymd(forecast_date)),
      linetype = "dashed"
    ) +
    facet_wrap(~location,
      nrow = length(unique(quantiles_wide$location)),
      scales = "free_y"
    ) +
    theme_bw() +
    xlab("") +
    ylab("Daily hospital admissions") +
    scale_color_discrete() +
    scale_fill_discrete() +
    scale_x_date(
      date_breaks = "2 weeks",
      labels = scales::date_format("%Y-%m-%d")
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(
        size = 8, vjust = 1,
        hjust = 1, angle = 45
      ),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 10),
      plot.title = element_text(
        size = 10,
        vjust = 0.5, hjust = 0.5
      )
    )

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
    dplyr::filter(location == loc_to_plot) |>
    dplyr::filter(forecast_date == date_to_plot) |>
    dplyr::filter(
      date <= forecast_date + lubridate::days(n_forecast_days),
      date >= forecast_date - lubridate::days(n_calib_days)
    ) |>
    dplyr::filter(lab_site_index <= max_n_site_labs_to_show)

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
      )
    ) +
    geom_ribbon(
      aes(
        x = date, ymin = `0.025`, ymax = `0.975`
      ),
      alpha = 0.1,
      show.legend = FALSE
    ) +
    geom_ribbon(
      aes(
        x = date, ymin = `0.25`, ymax = `0.75`,
      ),
      alpha = 0.1,
      show.legend = FALSE
    ) +
    geom_vline(aes(xintercept = lubridate::ymd(forecast_date)),
      linetype = "dashed"
    ) +
    facet_grid(location ~ site_lab_name,
      scales = "free_y"
    ) +
    theme_bw() +
    xlab("") +
    ylab("Log(genome copies per mL)") +
    scale_color_discrete() +
    scale_fill_discrete() +
    scale_x_date(
      date_breaks = "2 weeks",
      labels = scales::date_format("%Y-%m-%d")
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(
        size = 8, vjust = 1,
        hjust = 1, angle = 45
      ),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 10),
      plot.title = element_text(
        size = 10,
        vjust = 0.5, hjust = 0.5
      )
    )
  return(p)
}

#' Make hospital forecast comparison figure
#'
#' @param hosp_quantiles A tibble containing the calibrated hospital admissions
#' data, the evaluation hospital admissions data, and the quantiles of the
#' calibrated and forecasted admissions
#' @param loc_to_plot A  string indicating the state abbreviations of the state
#' to plot
#' @param horizon_to_plot A string indicating what horizon period to plot,
#' one of `nowcast`, `1 wk`, or `4 wks`
#' @param days_to_show_prev_data An ingeger indicating how many days before the
#' last forecast date to show the data, default is `14`
#'
#' @return A ggplot object containing a plot of the retrospective hospital
#' admissions data compared to the nowcasted/forecasted quantiles and median
#' for the specified horizon to plot, colored by the model type
#' @export
make_hosp_forecast_comp_fig <- function(hosp_quantiles,
                                        loc_to_plot,
                                        horizon_to_plot,
                                        days_to_show_prev_data = 14) {
  hosp_quants_horizons <- hosp_quantiles |>
    dplyr::filter(location == loc_to_plot) |>
    dplyr::filter(date >=
      min(forecast_date) - lubridate::days(
        days_to_show_prev_data
      )) |>
    dplyr::mutate(
      horizon = dplyr::case_when(
        date <= forecast_date & period != "calibration" ~ "nowcast",
        date > forecast_date & date <= forecast_date +
          lubridate::days(7) ~ "1 wk",
        date > forecast_date + lubridate::days(21) &
          date <= forecast_date + lubridate::days(28) ~ "4 wks"
      )
    )

  hosp <- hosp_quants_horizons |>
    dplyr::filter(horizon == horizon_to_plot) |>
    dplyr::filter(quantile %in% c(0.025, 0.25, 0.5, 0.75, 0.975)) |>
    tidyr::pivot_wider(
      id_cols = c(
        forecast_date, model_type,
        date, t, eval_data
      ),
      names_from = quantile,
      values_from = value
    )

  p <- ggplot(hosp) +
    geom_point(
      data = hosp_quants_horizons,
      aes(x = date, y = eval_data),
      fill = "black", size = 1, shape = 21,
      show.legend = FALSE
    ) +
    geom_line(
      data = hosp |> dplyr::filter(model_type == "ww"),
      aes(
        x = date, y = `0.5`, group = forecast_date
      ),
      color = "cornflowerblue"
    ) +
    geom_ribbon(
      data = hosp |> dplyr::filter(model_type == "ww"),
      aes(
        x = date, ymin = `0.025`, ymax = `0.975`,
        group = as.character(forecast_date)
      ), alpha = 0.1,
      fill = "cornflowerblue",
      show.legend = FALSE
    ) +
    geom_ribbon(
      data = hosp |> dplyr::filter(model_type == "ww"),
      aes(
        x = date, ymin = `0.25`, ymax = `0.75`,
        group = as.factor(forecast_date)
      ),
      alpha = 0.1,
      fill = "cornflowerblue",
      show.legend = FALSE
    ) +
    geom_line(
      data = hosp |> dplyr::filter(model_type == "hosp"),
      aes(
        x = date, y = `0.5`, group = forecast_date
      ),
      color = "purple4"
    ) +
    geom_ribbon(
      data = hosp |> dplyr::filter(model_type == "hosp"),
      aes(
        x = date, ymin = `0.025`, ymax = `0.975`,
        group = as.character(forecast_date)
      ), alpha = 0.1,
      fill = "purple4",
      show.legend = FALSE
    ) +
    geom_ribbon(
      data = hosp |> dplyr::filter(model_type == "ww"),
      aes(
        x = date, ymin = `0.25`, ymax = `0.75`,
        group = as.factor(forecast_date)
      ),
      fill = "purple4",
      alpha = 0.1,
      show.legend = FALSE
    ) +
    xlab("") +
    ylab("Daily hospital admissions") +
    ggtitle(glue::glue(
      "{horizon_to_plot}"
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
        size = 8, vjust = 1,
        hjust = 1, angle = 45
      ),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      plot.title = element_text(
        size = 10,
        vjust = 0.5, hjust = 0.5
      )
    )

  return(p)
}

#' Make CRPS underlay figure
#'
#' @param scores A tibble of scores by location, forecast date, date and model,
#' the ouput of `scoringutils::score()` on samples.
#' @param loc_to_plot A  string indicating the state abbreviations of the state
#' to plot
#' @param horizon_to_plot A string indicating what horizon period to plot,
#' either `nowcast`, `1 wk`, or `4 wks`
#'
#' @return A ggplot object containing a bar chart of the crps score averaged
#' across the horizon for each forecast date, colored by the model type
#' @export
make_crps_underlay_fig <- function(scores,
                                   loc_to_plot,
                                   horizon_to_plot) {
  scores_by_horizon <- scores |>
    dplyr::filter(location == loc_to_plot) |>
    dplyr::mutate(
      horizon = dplyr::case_when(
        date <= forecast_date ~ "nowcast",
        date > forecast_date & date <= forecast_date +
          lubridate::days(7) ~ "1 wk",
        date > forecast_date + lubridate::days(21) &
          date <= forecast_date + lubridate::days(28) ~ "4 wks"
      )
    ) |>
    data.table::as.data.table() |>
    scoringutils::summarise_scores(by = c(
      "forecast_date", "location",
      "model", "horizon"
    )) |>
    dplyr::filter(horizon == horizon_to_plot)

  p <- ggplot(scores_by_horizon) +
    geom_bar(aes(x = forecast_date, y = crps, fill = model),
      stat = "identity", position = "dodge"
    ) +
    xlab("") +
    ylab("CRPS scores") +
    ggtitle(glue::glue(
      "{horizon_to_plot}"
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
        size = 8, vjust = 1,
        hjust = 1, angle = 45
      ),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      plot.title = element_text(
        size = 10,
        vjust = 0.5, hjust = 0.5
      )
    )

  return(p)
}

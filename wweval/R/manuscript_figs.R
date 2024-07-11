#' Make a plot comparing the fit and forecasted hospital admissions
#' from the wastewater and hospital admissions model for a few
#' example states
#'
#' @param hosp_quantiles A tibble containing the calibrated hospital admissions
#' data, the evaluation hospital admissions data, and the quantiles of the
#' calibrated and forecasted admissions
#' @param locs_to_plot A vector of strings indicating the state abbreviations
#' for which states to plot
#' @param date_to_plot A character string indicating what forecast date to plot
#'
#' @return a ggplot object containing a faceted vertical plot of the
#' forecasts produced for each state, comparing the wastewater and hospital
#' admissions models
#' @export
make_fig2_hosp_t <- function(hosp_quantiles,
                             locs_to_plot,
                             date_to_plot) {
  hosp <- hosp_quantiles |>
    dplyr::filter(location %in% c(locs_to_plot)) |>
    dplyr::filter(forecast_date == date_to_plot)

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

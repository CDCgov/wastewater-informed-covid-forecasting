#' Get standardized plot theme to add to figures
#'
#' @param x_axis_text_size integer indicating the size of x axis text, to be
#' passed to theme. Default is `8`
#' @param y_axis_text_size integer indicating the size of y axis text, to be
#' passed to theme. Default is `8`
#' @param x_axis_title_size integer indicating the size of x axis title, to be
#' passed to theme. Default is `10`
#' @param y_axis_title_size integer indicating the size of y axis title, to be
#' passed to theme. Default is `10`
#' @param legend_title_size integer indicating the size of the legend title
#' Default is `10`
#' @param legend_text_size integer indicating the size of the legend items
#' Default is `10`
#' @param facet_x_text_size integer indicating the size of facet title on the
#' x-xaxis, to be passed to theme. Default is `8`
#' @param facet_y_text_size integer indicating the size of facet title on the
#' x-xaxis, to be passed to theme. Default is `8`
#' @param plot_title_size integer indicating thesize of plot title, to be passed
#'  to theme. Default is `10`
#' @param x_axis_dates boolean indicating whether or not the x axis are dates.
#' If they are dates, we will rotate x axis tick text 45 degrees. Default is
#' `FALSE`
#'
#' @return a theme object to add to a [ggplot2::ggplot()] object to specify
#' line size and formatting
#' @export
get_plot_theme <- function(x_axis_text_size = 8,
                           y_axis_text_size = 8,
                           x_axis_title_size = 12,
                           y_axis_title_size = 12,
                           legend_title_size = 10,
                           legend_text_size = 8,
                           facet_x_text_size = 8,
                           facet_y_text_size = 8,
                           plot_title_size = 10,
                           x_axis_dates = FALSE) {
  ww_theme <-
    cowplot::theme_half_open() +
    cowplot::background_grid() +
    theme(
      axis.text.x = element_text(
        size = x_axis_text_size
      ),
      axis.text.y = element_text(
        size = y_axis_text_size
      ),
      legend.title = element_text(size = legend_title_size),
      legend.text = element_text(size = legend_text_size),
      strip.text.x = element_text(size = facet_x_text_size),
      strip.text.y = element_text(size = facet_y_text_size),
      axis.title.x = element_text(size = x_axis_title_size),
      axis.title.y = element_text(size = y_axis_title_size),
      plot.title = element_text(
        size = plot_title_size,
        vjust = 0.5, hjust = 0.5
      )
    )


  if (isTRUE(x_axis_dates)) {
    # If x-axis are dates, default to 2 week date breaks
    # and rotate 45 degrees
    ww_theme <- ww_theme +
      theme(
        axis.text.x = element_text(
          size = x_axis_text_size - 2, vjust = 1,
          hjust = 1, angle = 45
        )
      )
  }

  return(ww_theme)
}


#' Get plot components (colors for now)
#'
#' @return a list of model, phase, and horizon colors to be passed to
#' `scale_fill_manual()` and `scale_color_manual()`
#' @export
plot_components <- function() {
  pal_horizons <- RColorBrewer::brewer.pal(12, "Paired")

  # somewhat arbitrary, we can play with these later
  horizon_colors <- c(
    "calibration" = pal_horizons[1],
    "nowcast" = pal_horizons[2],
    "1 wk" = pal_horizons[3],
    "2 wks" = pal_horizons[4],
    "3 wks" = pal_horizons[5],
    "4 wks" = pal_horizons[6],
    "overall" = pal_horizons[10]
  )

  pal_model <- RColorBrewer::brewer.pal(8, "Dark2")
  pastel_model <- RColorBrewer::brewer.pal(8, "Pastel2")

  model_colors <- c(
    "ww" = pal_model[1],
    "hosp" = pal_model[2],
    "cfa-wwrenewal(retro)" = pal_model[1],
    "cfa-wwrenewal(real-time)" = pal_model[5],
    "cfa-hosponlyrenewal(retro)" = pal_model[2],
    "COVIDhub-4_week_ensemble" = pastel_model[3],
    "COVIDhub-trained_ensemble" = pal_horizons[1],
    "COVIDhub_CDC-ensemble" = pal_horizons[2],
    "COVIDhub_ensemble" = pal_horizons[3],
    "UMass-trends_ensemble" = pastel_model[4],
    "UT-Osiris" = pastel_model[5],
    "CEPH-Rtrend_covid" = pastel_model[1],
    "CMU-TimeSeries" = pastel_model[2],
    "Sgroup-RandomForest" = pastel_model[7],
    "UMass-gbq" = pastel_model[8],
    "UMass-sarix" = pal_model[3],
    "COVIDhub-baseline" = pastel_model[6]
  )

  phase_colors <- c(
    "increasing" = "peachpuff",
    "peak" = "tomato",
    "nadir" = "lightskyblue",
    "decreasing" = "lightgreen",
    "uncertain" = "lightgray"
  )

  colors_list <- list(
    horizon_colors = horizon_colors,
    model_colors = model_colors,
    phase_colors = phase_colors
  )
  return(colors_list)
}

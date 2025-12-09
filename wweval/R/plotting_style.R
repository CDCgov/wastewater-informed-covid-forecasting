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
#' @param rotate_x_ticks boolean indicating whether to rotate x axis
#' tick text 45 degrees. Default is `FALSE`
#'
#' @return a theme object to add to a [ggplot2::ggplot()] object to specify
#' line size and formatting
#' @export
get_plot_theme <- function(
  x_axis_text_size = 8,
  y_axis_text_size = 8,
  x_axis_title_size = 12,
  y_axis_title_size = 12,
  legend_title_size = 10,
  legend_text_size = 8,
  facet_x_text_size = 8,
  facet_y_text_size = 8,
  plot_title_size = 10,
  rotate_x_ticks = FALSE
) {
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
        vjust = 0.5,
        hjust = 0.5
      ),
      plot.background = element_rect(fill = "white")
    )

  if (isTRUE(rotate_x_ticks)) {
    ww_theme <- ww_theme +
      theme(
        axis.text.x = element_text(
          vjust = 1,
          hjust = 1,
          angle = 45
        )
      )
  }

  return(ww_theme)
}

pal_horizons <- RColorBrewer::brewer.pal(12, "Paired")
pal_model <- RColorBrewer::brewer.pal(8, "Dark2")
pastel_model <- RColorBrewer::brewer.pal(8, "Pastel2")


## somewhat arbitrary, we can play with these later
horizon_colors <- c(
  "calibration" = pal_horizons[1],
  "nowcast" = pal_horizons[2],
  "1 wk" = pal_horizons[3],
  "2 wks" = pal_horizons[4],
  "3 wks" = pal_horizons[5],
  "4 wks" = pal_horizons[6],
  "overall" = pal_horizons[10]
)

phase_colors <- c(
  "increasing" = "peachpuff",
  "peak" = "tomato",
  "nadir" = "lightskyblue",
  "decreasing" = "lightgreen",
  "uncertain" = "lightgray"
)

observation_status_colors <- c(
  "standard" = "black",
  "outlier" = "red",
  "below LOD" = "blue"
)
observation_status_shapes <- c(
  "standard" = 16,
  "outlier" = 2,
  "below LOD" = 0
)

model_colors <- c(
  "ww" = pal_model[1],
  "hosp" = pal_model[2],
  "cfa-wwrenewal(retro)" = pal_model[1],
  "cfa-wwrenewal(real-time)" = pal_model[5],
  "cfa-hosponlyrenewal(real-time*)" = "orange3",
  "cfa-hosponlyrenewal(retro)" = pal_model[2],
  "COVIDhub-4_week_ensemble" = pastel_model[3],
  "COVIDhub-trained_ensemble" = pal_horizons[1],
  "COVIDhub_ensemble" = pal_horizons[3],
  "MUNI-ARIMA" = pal_horizons[2],
  "UMass-trends_ensemble" = pastel_model[4],
  "UT-Osiris" = pastel_model[5],
  "CEPH-Rtrend_covid" = pastel_model[1],
  "CMU-TimeSeries" = pastel_model[2],
  "SGroup-RandomForest" = pastel_model[7],
  "UMass-gbq" = pastel_model[8],
  "UMass-sarix" = pal_model[3],
  "COVIDhub-baseline" = pastel_model[6],
  "MOBS-GLEAM_COVID" = pastel_model[8]
)

model_shapes <- c(
  "cfa-wwrenewal(retro)" = 21,
  "cfa-wwrenewal(real-time)" = 21,
  "cfa-hosponlyrenewal(real-time*)" = 22,
  "cfa-hosponlyrenewal(retro)" = 22,
  "COVIDhub-4_week_ensemble" = 23,
  "UMass-sarix" = 24,
  "CMU-TimeSeries" = 25
)


#' Get plot components (colors for now)
#'
#' @return a list of model, phase, and horizon colors to be passed to
#' `scale_fill_manual()` and `scale_color_manual()`
#' @export
plot_components <- function() {
  colors_list <- list(
    horizon_colors = horizon_colors,
    model_colors = model_colors,
    phase_colors = phase_colors,
    observation_status_colors = observation_status_colors,
    observation_status_shapes = observation_status_shapes
  )
  return(colors_list)
}

score_component_alphas <- c(
  "underprediction" = 0.95,
  "dispersion" = 0.6,
  "overprediction" = 0.95
)

#' ggplot discrete scales for forecast models
#'
#' @param name Display name for the scale. Default `"Model"`.
#' @param ... Keyword arguments passed to
#' [`ggplot2::scale_*_manual()`][ggplot2::scale_fill_manual()].
#' @return the ggplot scale
#' @export
scale_fill_model <- function(name = "Model", ...) {
  return(ggplot2::scale_fill_manual(
    name = name,
    values = model_colors
  ))
}

#' @rdname scale_fill_model
#' @export
scale_color_model <- function(name = "Model", ...) {
  return(ggplot2::scale_color_manual(
    name = name,
    values = model_colors
  ))
}

#' @rdname scale_fill_model
#' @export
scale_shape_model <- function(name = "Model", ...) {
  return(ggplot2::scale_shape_manual(
    name = name,
    values = model_shapes
  ))
}

#' ggplot discrete scales for forecast horizons
#'
#' @param name Display name for the scale. Default` "Horizon"`.
#' @param ... Keyword arguments passed to
#' [`ggplot2::scale_*_manual()`][ggplot2::scale_fill_manual()].
#' @return the ggplot scale
#'
#' @export
scale_fill_horizon <- function(name = "Horizon", ...) {
  return(ggplot2::scale_fill_manual(
    name = name,
    values = horizon_colors,
    ...
  ))
}

#' @rdname scale_fill_horizon
#' @export
scale_color_horizon <- function(name = "Horizon", ...) {
  return(ggplot2::scale_color_manual(
    name = name,
    values = horizon_colors
  ))
}


#' ggplot scale for fill alphas by name of
#' WIS/CRPS score component.
#'
#' @param ... Keyword arguments passed to
#' [ggplot2::scale_alpha_manual()]
#' @return the ggplot scale
#'
#' @export
scale_alpha_score_component <- function(...) {
  return(ggplot2::scale_alpha_manual(
    values = score_component_alphas
  ))
}

#' ggplot fill scale for score ratios
#'
#' @param ... Keyword arguments passed to
#' [ggplot2::scale_fill_gradient2()]
#' @return The scale.
#' @export
scale_fill_score_ratio <- function(...) {
  return(ggplot2::scale_fill_gradient2(
    high = "red",
    mid = "white",
    low = "blue",
    transform = "log10",
    midpoint = 1,
    guide = "colourbar",
    aesthetics = "fill",
    labels = scales::number_format(accuracy = 0.01),
    ...
  ))
}

#' Default x axis date scale
#'
#' Dates formatted in ISO YYYY-MM-DD format, weekly breaks.
#'
#' @param ... Keyword arguments passed to
#' [ggplot2::scale_x_date()].
#' @return The scale.
#' @export
scale_x_weekly_iso_date <- function(...) {
  return(scale_x_date(
    date_breaks = "1 week",
    labels = scales::date_format("%Y-%m-%d"),
    ...
  ))
}

#' Composite geometric object for barplots of scores decomposed into
#' undeprediction, overprediction, and dispersion components
#'
#' @param dispersion_alpha Alpha parameter for the dispersion tile.
#' Default 0.6.
#' @param ... keyword arguments passed to [ggplot2::geom_tile()].
#' @export
geom_decomposed_scores <- function(dispersion_alpha = 0.6, ...) {
  return(
    list(
      ggplot2::geom_tile(
        mapping = aes(
          height = .data$underprediction,
          y = .data$underprediction / 2
        ),
        linetype = "dashed",
        ...
      ),
      ggplot2::geom_tile(
        mapping = aes(
          height = .data$dispersion,
          y = .data$underprediction + .data$dispersion / 2
        ),
        alpha = dispersion_alpha,
        linetype = "solid",
        ...
      ),
      ggplot2::geom_tile(
        mapping = aes(
          height = .data$overprediction,
          y = .data$underprediction +
            .data$dispersion +
            .data$overprediction / 2
        ),
        linetype = "dotted",
        ...
      )
    )
  )
}

#' Plot posterior draws for a given parameter against
#' a known marginal prior distribution.
#'
#' @param draws_long long-format posterior draws, as the
#' output of [tidybayes::gather_draws()].
#' @param priors data frame of priors, with the same
#' faceting columns as `draws_long`.
#' @param fit_id_col Name of a column in `draws_long` that uniquely
#' identifies individual model fits within a facet. Default `"forecast_date"`.
#' @param variable_name Name for the variable column.
#' Default `".variable"`, matching  [tidybayes::gather_draws()].
#' @param value_name Name for the value column.
#' Default `".value"`, matching  [tidybayes::gather_draws()].
#' @param prior_pdf_name Column containing prior pdfs. Default
#' `"pdf"`.
#' @param row_facet Column to facet rows by. Default `"location"`.
#' @return The plot.
#' @export
plot_prior_posterior <- function(
  draws_long,
  priors,
  fit_id_col = "forecast_date",
  variable_name = ".variable",
  value_name = ".value",
  prior_pdf_name = "prior_pdf",
  row_facet = "location"
) {
  ## manually handle row and column faceting to
  ## ensure density scales are correct
  plot_col <- function(
    variable_value,
    row_value,
    x_transform,
    display_name,
    xlim,
    is_last_in_row
  ) {
    prior_data <- priors |>
      dplyr::filter(.data[[variable_name]] == !!variable_value)
    posterior_data <- draws_long |>
      dplyr::filter(
        .data[[variable_name]] == !!variable_value,
        .data[[row_facet]] == !!row_value
      )

    p <- ggplot2::ggplot() +
      ggdist::stat_slab(
        mapping = ggplot2::aes(xdist = .data[[prior_pdf_name]]),
        data = prior_data,
        color = "gray",
        alpha = 0.75,
        linetype = "dashed",
        fill = "gray"
      ) +
      ggdist::stat_slab(
        mapping = ggplot2::aes(
          x = .data[[value_name]],
          group = .data[[fit_id_col]],
          color = .data$model_type
        ),
        data = posterior_data,
        fill = NA,
        linewidth = 1.5,
        alpha = 0.3
      ) +
      get_plot_theme() +
      scale_color_model() +
      ggdist::scale_thickness_shared() +
      ggplot2::scale_x_continuous(transform = x_transform) +
      ggplot2::labs(x = display_name, y = "Relative density") +
      ggplot2::coord_cartesian(xlim = xlim, clip = "off")

    if (is_last_in_row) {
      p <- p +
        ggplot2::annotate(
          "text",
          x = I(1.05),
          y = I(0.5),
          label = row_value,
          size = 5,
          angle = 270
        )
    }

    return(p)
  }
  row_values <- unique(draws_long[[row_facet]])

  n_rows <- length(row_values)
  to_plot <- tidyr::crossing(
    priors |>
      dplyr::select(
        variable_value = ".variable",
        "x_transform",
        "display_name",
        "xlim"
      ),
    row_value = row_values
  ) |>
    dplyr::arrange(.data$row_value, .data$variable_value) |>
    dplyr::mutate(
      is_last_in_row = dplyr::lead(
        .data$row_value,
        1,
        default = ".non_value"
      ) !=
        .data$row_value
    )

  return(
    purrr::pmap(to_plot, plot_col) |>
      patchwork::wrap_plots(
        nrow = n_rows,
        guides = "collect",
        axes = "collect",
        byrow = TRUE
      ) &
      ggplot2::theme(
        legend.position = "none",
        plot.margin = ggplot2::margin(
          t = 2,
          b = 2,
          l = 2,
          r = 10
        )
      )
  )
}

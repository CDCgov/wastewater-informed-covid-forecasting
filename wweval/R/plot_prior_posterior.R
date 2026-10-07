#' Get a properly parameterized prior distribution
#' for selected model parameters.
#'
#' @param parameter_name Name of the parameter in the wwinference Stan model
#' @param prior_params Named list of prior hyperparameter values, as the
#' output of [wwinference::get_params()]
#' @return `A distributional` distribution object parameterized by the priors,
#' e.g. a [distributional::dist_normal()] object. Errors if the user requests
#' an unknown parameter.
#'
#' @export
get_parameterized_prior_dist <- function(parameter_name, prior_params) {
  pos_normal <- \(...) {
    distributional::dist_normal(...) |>
      distributional::dist_truncated(lower = 0)
  }

  dists_and_hyper_names <- list(
    "log10_g" = list(
      fn = distributional::dist_normal,
      params = list(
        mean = "log10_g_prior_mean",
        sd = "log10_g_prior_sd"
      )
    ),
    "infection_feedback" = list(
      fn = distributional::dist_lognormal,
      params = list(
        mu = "infection_feedback_prior_logmean",
        sigma = "infection_feedback_prior_logsd"
      )
    ),
    "sd_log_sigma_ww_site" = list(
      fn = pos_normal,
      params = list(
        mean = "sd_log_sigma_ww_site_prior_mode",
        sd = "sd_log_sigma_ww_site_prior_sd"
      )
    ),
    "ww_site_mod_sd" = list(
      fn = purrr::partial(pos_normal, mean = 0),
      ## mean = 0 hard-coded in wwinference Stan model
      params = list(
        sd = "ww_site_mod_sd_sd"
      )
    ),
    "eta_sd" = list(
      fn = pos_normal,
      params = list(
        mean = "eta_sd_mean",
        sd = "eta_sd_sd"
      )
    )
  )

  checkmate::assert_choice(parameter_name, names(dists_and_hyper_names))

  to_build <- dists_and_hyper_names[[parameter_name]]
  checkmate::assert_names(
    names(prior_params),
    must.include = as.character(to_build$params)
  )
  named_list_of_constructor_params <- purrr::map(to_build$params, \(x) {
    prior_params[[x]]
  })

  return(do.call(to_build$fn, named_list_of_constructor_params))
}

#' Plot posterior draws for a given parameter against
#' a known marginal prior distribution.
#'
#' @param draws_long long-format posterior draws, as the
#' output of [tidybayes::gather_draws()] with auxiliary metadata
#' columns as below.
#' @param prior_params Named list of prior hyperparameter values, as the
#' output of [wwinference::get_params()]
#' @param fit_id_col Name of a column in `draws_long` that uniquely
#' identifies individual model fits within a facet. Default `"forecast_date"`.
#' @param model_type_col Name of a column in `draws_long` that
#' identifies model types, so that [scale_fill_model()] can be applied.
#' Default `"model_type"`.
#' @param variable_name Name for the variable column.
#' Default `".variable"`, matching  [tidybayes::gather_draws()].
#' @param value_name Name for the value column.
#' Default `".value"`, matching  [tidybayes::gather_draws()].
#' @param row_facet Column to facet rows by. Default `"location"`.
#' @param custom_x_transform Optional named list mapping parameter names
#' to custom x transforms to use for that parameter's panels in the plot.
#' If a parameter name is not matched, the identity transform will be used.
#' Default `list()`, which implies using the identity transform for all
#' panels.
#' @param custom_xlim Optional named list mapping parameter names
#' to vectors specificing custom x limits to use for that parameter's
#' panels in the plot. If a parameter name is not matched, x limits will
#' be deferred to ggplot. Default `list()`, which implies deferring limits
#' for all parameters.
#' @return The plot.
#' @export
plot_prior_posterior <- function(
  draws_long,
  prior_params,
  fit_id_col = "forecast_date",
  model_type_col = "model_type",
  variable_name = ".variable",
  value_name = ".value",
  row_facet = "location",
  custom_x_transform = list(),
  custom_xlim = list()
) {
  ## manually handle row and column faceting to
  ## ensure density scales are correct
  plot_panel <- function(
    col_value,
    row_value,
    is_last_in_row
  ) {
    posterior_data <- draws_long |>
      dplyr::filter(
        .data[[variable_name]] == !!col_value,
        .data[[row_facet]] == !!row_value
      )

    xlim <- custom_xlim[[col_value]]
    x_transform <- custom_x_transform[[col_value]] %||% "identity"

    p <- ggplot2::ggplot() +
      ggdist::stat_slab(
        mapping = ggplot2::aes(
          xdist = get_parameterized_prior_dist(
            col_value,
            prior_params
          )
        ),
        data = NULL,
        color = "gray",
        alpha = 0.75,
        linetype = "dashed",
        fill = "gray"
      ) +
      ggdist::stat_slab(
        mapping = ggplot2::aes(
          x = .data[[value_name]],
          group = .data[[fit_id_col]],
          color = .data[[model_type_col]]
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
      ggplot2::labs(
        x = get_parameter_display_name(col_value),
        y = "Relative density"
      ) +
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
  col_values <- unique(draws_long[[variable_name]])
  row_values <- unique(draws_long[[row_facet]])

  n_rows <- length(row_values)
  to_plot <- tidyr::crossing(
    col_value = col_values,
    row_value = row_values
  ) |>
    dplyr::arrange(.data$row_value, .data$col_value) |>
    dplyr::mutate(
      is_last_in_row = dplyr::lead(
        .data$row_value,
        1,
        default = ".non_value"
      ) !=
        .data$row_value
    )

  return(
    purrr::pmap(to_plot, plot_panel) |>
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

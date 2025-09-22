#' Get a summary table of the number of forecasts excluded for each reason
#'
#' @param metadata a tibble containing metadata for each forecast date location
#'
#' @return a 1 row tibble with the number of forecasts for each category
#' @export
get_summary_metadata <- function(metadata) {
  metadata_summarized <- metadata |>
    dplyr::select(
      forecast_date,
      location,
      ww_data_present,
      ww_sufficient,
      any_flags_hosp,
      any_flags_ww
    )

  metadata_remove_insuff_ww <- metadata_summarized |>
    dplyr::filter(ww_data_present == 1, ww_sufficient == TRUE)

  n_insuff_ww <- nrow(metadata_summarized) -
    nrow(metadata_remove_insuff_ww)

  metadata_remove_conv_issues <- metadata_remove_insuff_ww |>
    dplyr::filter(any_flags_hosp == FALSE, any_flags_ww == FALSE)

  n_conv_issues <- nrow(metadata_remove_insuff_ww) -
    nrow(metadata_remove_conv_issues)

  summary_table <- tibble::tibble(
    n_insuff_ww,
    n_conv_issues,
    n_forecasts = nrow(metadata_remove_conv_issues)
  )

  return(summary_table)
}


#' Plot a heatmap of the metadata of reasons for excluding
#' forecasts from analysis
#'
#' @param metadata a tibble of location -forecast date metadata
#' @return a ggplot object with a heatmap colored by reason for excluding
#' @export
plot_heatmap_metadata_retro <- function(metadata) {
  metadata_summarized <- metadata |>
    dplyr::select(
      forecast_date,
      location,
      ww_data_present,
      ww_sufficient,
      any_flags_hosp,
      any_flags_ww
    ) |>
    dplyr::ungroup()

  metadata_final <- metadata_summarized |>
    dplyr::mutate(
      metadata_cat = case_when(
        ww_data_present != 1 ~ "absent or insufficient wastewater",
        ww_sufficient != TRUE ~ "absent or insufficient wastewater",
        any_flags_ww == TRUE ~ "model had convergence issues",
        any_flags_hosp == TRUE ~ "model had convergence issues",
        TRUE ~ "both models produced forecasts"
      )
    )

  p <- ggplot(metadata_final) +
    geom_tile(aes(
      x = forecast_date,
      y = location,
      fill = metadata_cat
    )) +
    scale_fill_discrete() +
    get_plot_theme(
      x_axis_dates = TRUE,
      y_axis_text_size = 4
    ) +
    scale_x_date(
      date_breaks = "1 week",
      labels = scales::date_format("%Y-%m-%d")
    ) +
      coord_cartesian(expand = 0) +
    xlab("") +
    ylab("Location") +
    labs(fill = "Metadata Information") +
    ggtitle(glue::glue(
      "Summary of retrospective comparison analysis"
    ))

  return(p)
}

#' Plot a heatmap of the metadata of Hub models submitted
#'
#' @param metadata a tibble of location -forecast date metadata
#' @param analysis_type string indicating whether this is the
#' real-time or retro analysis, which dictates how metadata is gathered
#' @return a ggplot object with a heatmap colored by reason for excluding
#' @export
plot_heatmap_metadata_hub <- function(metadata, analysis_type) {
  if (analysis_type == "retro") {
    metadata_summarized <- metadata |>
      dplyr::select(
        forecast_date,
        location,
        ww_data_present,
        ww_sufficient,
        any_flags_hosp,
        any_flags_ww
      ) |>
      dplyr::ungroup() |>
      dplyr::mutate(
        model_submitted = dplyr::case_when(
          ww_data_present != 1 ~ "hosp",
          ww_sufficient != TRUE ~ "hosp",
          any_flags_ww == TRUE ~ "hosp",
          TRUE ~ "ww"
        )
      ) |>
      dplyr::mutate(
        model_name = "cfa-wwrenewal(retro)"
      )

    metadata_hosp_only <- metadata_summarized |>
      dplyr::mutate(
        model_submitted = "hosp",
        model_name = "cfa-hosponlyrenewal(retro)"
      )

    all_metadata <- dplyr::bind_rows(
      metadata_summarized,
      metadata_hosp_only
    )
  } else if (analysis_type == "real-time") {
    # Then we need to get this info on metadata from our github!
    dates <- seq(
      from = lubridate::ymd("2024-02-05"),
      to = lubridate::ymd("2024-03-25"),
      by = "week"
    )
    df_replacements <- get_date_locs_hosp_used(dates) |>
      dplyr::mutate(
        model_submitted = "hosp"
      )
    locs <- unique(metadata$location)
    metadata_grid <- expand.grid(
      location = locs,
      forecast_date = dates
    )
    metadata_ww <- metadata_grid |>
      dplyr::left_join(
        df_replacements
      ) |>
      dplyr::mutate(
        model_submitted = ifelse(
          is.na(model_submitted),
          "ww",
          "hosp"
        ),
        model_name = "cfa-wwrenewal(real-time)"
      )
    metadata_hosp <- metadata_grid |>
      dplyr::mutate(
        model_submitted = "hosp",
        model_name = "cfa-hosponlyrenewal(real-time*)"
      )
    all_metadata <- dplyr::bind_rows(metadata_ww, metadata_hosp)
  } else {
    stop("Unexpected analysis type")
  }

  p <- ggplot(all_metadata) +
    geom_tile(aes(
      x = forecast_date,
      y = location,
      fill = model_submitted
    )) +
    scale_fill_discrete() +
    facet_wrap(~model_name) +
    get_plot_theme(
      x_axis_dates = TRUE,
      y_axis_text_size = 4
    ) +
    scale_x_date(
      date_breaks = "1 week",
      labels = scales::date_format("%Y-%m-%d")
    ) +
    coord_cartesian(expand = 0) +
    theme(legend.position = "bottom") +
    xlab("") +
    ylab("Location") +
    labs(fill = "Model submitted") +
    ggtitle(glue::glue("Summary of models used in Hub analysis"))

  return(p)
}

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
        ww_data_present != 1 ~ "Wastewater data absent",
        ww_sufficient != TRUE ~ "Wastewater data present but insufficient",
        any_flags_ww == TRUE ~ "Wastewater model had convergence issues",
        any_flags_hosp == TRUE ~ "Admissions-only model had convergence issues",
        TRUE ~ "Both models produced forecasts"
      )
    )

  p <- ggplot(metadata_final) +
    geom_tile(aes(
      x = .data$forecast_date,
      y = .data$location,
      fill = .data$metadata_cat
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

.plot_hub_metadata <- function(metadata) {
  p <- ggplot(metadata) +
    geom_tile(aes(
      x = .data$forecast_date,
      y = .data$location,
      fill = .data$model_submitted
    )) +
    scale_fill_discrete() +
    facet_wrap(~ .data$model_name) +
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

#' Plot model submission decisions in real-time
#'
#' @param forecast_dates forecast dates to plot
#' @param locations locations to plot
#' @param hosp_subtitution_table table of exclusions,
#' as produced by [parse_real_time_exclusions()].
#' @return The plot, as a ggplot object.
#' @export
plot_hub_submit_info_real_time <- function(
  forecast_dates,
  locations,
  exclusion_table
) {
  metadata_grid <- expand.grid(
    location = locations,
    forecast_date = forecast_dates
  )

  metadata_ww <- metadata_grid |>
    dplyr::left_join(exclusion_table, by = c("forecast_date", "location")) |>
    dplyr::mutate(
      model_submitted = dplyr::case_match(
        .data$exclusion,
        c("absent_ww", "manual_exclude_ww") ~ "Hospital admissions-only",
        c(NA, "insufficient_ww") ~ "Wastewater-informed",
        "manual_exclude_both" ~ "Neither"
      ),
      model_name = "cfa-wwrenewal(real-time)"
    ) |>
    dplyr::select(-"exclusion")

  metadata_hosp <- metadata_grid |>
    dplyr::mutate(
      model_submitted = "hosp",
      model_name = "cfa-hosponlyrenewal(real-time*)"
    )
  all_metadata <- dplyr::bind_rows(metadata_ww, metadata_hosp)

  return(.plot_hub_metadata(all_metadata))
}

#' Plot a heatmap of the metadata of Hub models "submitted" in the
#' retrospective analysis
#'
#' @param metadata a tibble of location and forecast date metadata
#' @return The plot.
#' @export
plot_hub_submit_info_retro <- function(metadata) {
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

  return(.plot_hub_metadata(all_metadata))
}

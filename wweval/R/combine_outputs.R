#' Combine outputs
#' @description
#' This function is a helper function specific to the current nested file
#' structure specified by the functions that save the quantiles and scores
#' for each of the model runs to disk. The function takes in the
#' vectors of scenarios, forecast dates, and locations with output model runs,
#' checks if they exist in the current file structure, and if they do
#' loads them in and binds them together to create one large dataframe with
#' all of the outputs row binded
#'
#'
#' @param output_type the type of output that is saved, one of `"quantiles"`, `"scores"`,
#' `"ww_quantiles"`, `"scores_quantiles"`, `"hosp_quantiles"`,`"errors"`,
#'  `"ww_data_flags"` or `"flags"`.
#' @param scenarios The vector of character strings of all the scenarios
#' @param forecast_dates The vector of character strings of all the forecast dates
#' @param locations The vector of character strings of all the locations
#' @param eval_output_subdir The outer subdirectory of the nested file structure
#' @param model_type The type of model, either `"ww"` or `"hosp"`
#'
#' @return combined_output: a tibble with output types for all combinations of
#' forecast_dates, locations, and scenarios
#' @export
#'
combine_outputs <- function(output_type,
                            scenarios,
                            forecast_dates,
                            locations,
                            eval_output_subdir,
                            model_type) {
  checkmate::assert_scalar(output_type)
  checkmate::assert_names(output_type,
    subset.of = c(
      "quantiles",
      "scores",
      "ww_quantiles",
      "scores_quantiles",
      "hosp_quantiles",
      "flags",
      "errors",
      "ww_data_flags"
    )
  )
  to_combine <- tibble::tibble(
    scenario = scenarios,
    forecast_date = forecast_dates,
    location = locations
  )

  load_output <- function(scenario, forecast_date, location) {
    fp <- get_filepath(
      eval_output_subdir,
      scenario,
      forecast_date,
      model_type,
      location,
      output_type,
      "tsv"
    )
    if (file.exists(fp)) {
      output <- readr::read_tsv(
        fp,
        show_col_types = FALSE
      ) |>
        dplyr::mutate(success = TRUE)
    } else {
      warning(glue::glue(
        "File missing for {scenario} ",
        "in {location} on {forecast_date}"
      ))
      output <- tibble(
        scenario = scenario,
        location = location,
        forecast_date = forecast_date,
        success = FALSE
      )
    }

    return(output)
  }

  combined <- purrr::pmap_df(to_combine, load_output)

  combined_output <- combined |>
    dplyr::filter(.data$success) |>
    dplyr::select(-"success")

  failed_output <- combined |>
    dplyr::filter(!.data$success) |>
    dplyr::select(-"success")

  if (nrow(combined_output) == 0) {
    combined_output <- NULL
  }

  if (nrow(failed_output) != 0) {
    wwinference::create_dir(file.path(
      eval_output_subdir,
      "files_missing", model_type
    ))

    readr::write_tsv(
      failed_output,
      fs::path(
        eval_output_subdir,
        "files_missing",
        model_type,
        output_type,
        ext = "tsv"
      )
    )
  }

  return(combined_output)
}

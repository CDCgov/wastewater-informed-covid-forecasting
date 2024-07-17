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
#'  or `"flags"`.
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
combine_outputs <- function(output_type =
                              c(
                                "quantiles", "scores", "ww_quantiles", "scores_quantiles",
                                "hosp_quantiles", "flags", "errors"
                              ),
                            scenarios,
                            forecast_dates,
                            locations,
                            eval_output_subdir,
                            model_type) {
  output_type <- arg_match(output_type)
  df <- tibble(
    scenario = scenarios,
    forecast_date = forecast_dates,
    location = locations
  )
  combined_output <- tibble()
  flag_failed_output <- tibble()
  for (i in seq_len(nrow(df))) {
    this_scenario <- df$scenario[i]
    this_forecast_date <- df$forecast_date[i]
    this_location <- df$location[i]

    # This is very much hard coded to the file structure. Will be replaced
    # with a single subdirectory when we have everything from azure
    fp <- get_filepath(
      eval_output_subdir,
      this_scenario,
      this_forecast_date,
      model_type,
      this_location,
      glue::glue("{output_type}"),
      "tsv"
    )


    print(fp)
    if (file.exists(fp)) {
      tryCatch(
        {
          this_output <- readr::read_tsv(fp)
          combined_output <- rbind(combined_output, this_output)
        },
        error = function(e) {}
      )
    } else {
      warning(glue::glue(
        "File missing for {this_scenario}",
        "in {this_location} on {this_forecast_date}"
      ))
      # Create a tibble of the combos that are missing, to save
      this_failed_output <- tibble(
        scenario = this_scenario,
        location = this_location,
        forecast_date = this_forecast_date
      )
      flag_failed_output <- rbind(flag_failed_output, this_failed_output)
    }
  }

  if (nrow(combined_output) == 0) {
    combined_output <- NULL
  }

  if (nrow(flag_failed_output) != 0) {
    # Save the missing files in a new subfolder in the eval_output_subdir
    cfaforecastrenewalww::create_dir(file.path(
      eval_output_subdir,
      "files_missing", model_type
    ))

    write.csv(
      flag_failed_output,
      file.path(
        eval_output_subdir, "files_missing", model_type,
        glue::glue("{output_type}.csv")
      )
    )
  }


  return(combined_output)
}

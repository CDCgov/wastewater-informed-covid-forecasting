#' Compute relative CRPS
#'
#' @param scores A tibble of scores containing the specified `id_cols`,
#' `model`, and the scoring metrics from  `scoringutils::score()` applied on
#' samples
#' @param id_cols A vector of character strings listing the columns of metadata
#' we want to keep as identifiers
#'
#' @return a pivoted wide tibble with side-by-side `ww` and `hosp` scores
#' for each `id_cols` as well as a `rel_crps` column for the relative
#' CRPS
#' @export
#'
compute_relative_crps <- function(scores,
                                  id_cols) {
  # Make sure model and crps are present in scores
  if (!all(c("model", "crps") %in% colnames(scores))) {
    cli::cli_abort(
      c(
        "Scores dataframe does not contain `crps`",
        "or `model` as a column name"
      )
    )
  }

  # Make sure id cols present
  if (!all(id_cols %in% colnames(scores))) {
    cli::cli_abort(
      c(
        "Scores dataframe does not contain all of",
        "the specified id columns"
      )
    )
  }

  # Make sure the model types are present
  model_types <- scores |>
    dplyr::distinct(model) |>
    dplyr::pull()

  if (!all(model_types %in% c("ww", "hosp"))) {
    cli::cli_abort(
      c("Model names are not `ww` and `hosp` as function expects")
    )
  }


  rel_crps <- scores |>
    tidyr::pivot_wider(
      names_from = model,
      values_from = crps,
      id_cols = !!id_cols
    ) |>
    dplyr::mutate(
      rel_crps = ww / hosp
    )

  return(rel_crps)
}

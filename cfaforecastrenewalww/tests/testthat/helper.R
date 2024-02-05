get_nonmatrix_names_from_draws <- function(draws) {
  posterior::as_draws_list(draws)[[1]] %>%
    names() %>%
    strsplit(split = "[", fixed = TRUE) %>%
    sapply(function(x) {
      x[1]
    }) %>%
    unique()
}

get_par_dims_flat <- function(draws) {
  par_names_no_dim <- get_nonmatrix_names_from_draws(draws)
  par_names_with_dim <- posterior::as_draws_list(draws)[[1]] %>%
    names()
  counts <- sapply(par_names_no_dim, function(par) {
    full <- paste0("^", par, "$")
    pre_dim <- paste0("^", par, "\\[")
    sum(
      grepl(full, par_names_with_dim) | grepl(pre_dim, par_names_with_dim)
    )
  })
  return(counts)
}

logit_fn <- function(p) {
  stats::qlogis(p)
}

inv_logit_fn <- function(x) {
  stats::plogis(x)
}

skip_if_missing_secrets <- function(secrets) {
  present <- sapply(secrets, function(secret) {
    Sys.getenv(secret) != ""
  })

  if (any(!present)) {
    skip("Requisite information not available to pull from this API.")
  }
}

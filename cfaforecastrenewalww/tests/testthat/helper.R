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

#' Reference R implementation of a zero-mean AR(1)
#' assembly from a set of z scores,
#' a standard deviation, and
#' an autocorrelation coefficient
#'
#' @param z a vector of z scores
#' @param sd the standard deviation of the AR(1) process
#' error distribution, as a scalar
#' @param ac the autocorrelation coefficient of the AR(1)
#' process, as a scalar
#' @param stationary Whether to initialize the process
#' at stationarity. Boolean, default `FALSE`.
#' @return Vector of values of the zero-mean AR(1) process
ar1_from_z_scores <- function(z, sd, ar, stationary = FALSE) {
  x <- rep(NA, length(z))

  x[1] <- z[1] * sd
  if (stationary) {
    stat_sd <- sd / sqrt(1 - ar * ar)
    x[1] <- z[1] * stat_sd
  }

  for (i in 2:length(z)) {
    x[i] <- ar * x[i - 1] + z[i] * sd
  }

  return(x)
}

#' Reference R implementation of a first
#' differenced zero-mean AR(1) assembly
#' from an initial
#' value, a set of z scores,
#' a standard deviation, and
#' an autocorrelation coefficient
#'
#' @param x0 the initial value of the first difference
#' ar1 process
#' @param ar the autocorrelation coefficient of the
#' underlying zero-mean AR(1) process, as a scalar
#' @param sd the standard deviation, of the underlying
#' AR(1) process, as a scalar
#' @param z a vector of z scores
#' @param stationary Whether to initialize the underlying
#' AR(1) on the first differences at stationarity.
#' Boolean, default `FALSE`.
#' @return Vector of values of the first differenced
#' zero-mean AR(1) process
diff_ar1_from_z_scores <- function(x0, ar, sd, z, stationary = FALSE) {
  n <- length(z) + 1
  diffs <- rep(NA, n)
  diffs[1] <- x0
  diffs[2:n] <- ar1_from_z_scores(
    z, sd, ar,
    stationary = stationary
  )

  return(cumsum(diffs))
}



#' Alternative R implementation of a first
#' differenced zero-mean AR(1) assembly
#' from an initial
#' value, a set of z scores,
#' a standard deviation, and
#' an autocorrelation coefficient
#'
#' @param x0 the initial value of the first difference
#' ar1 process
#' @param ar the autocorrelation coefficient of the
#' underlying zero-mean AR(1) process, as a scalar
#' @param sd the standard deviation, of the underlying
#' AR(1) process, as a scalar
#' @param z a vector of z scores
#' @param stationary Whether to initialize the underlying
#' AR(1) on the first differences at stationarity.
#' Boolean, default `FALSE`.
#' @return Vector of values of the first differenced
#' zero-mean AR(1) process
diff_ar1_from_z_scores_alt <- function(x0, ar, sd, z, stationary = FALSE) {
  n <- length(z) + 1
  x <- rep(NA, n)
  x[1] <- x0
  x[2] <- x[1] + sd * z[1]

  for (i in 3:n) {
    x[i] <- x[i - 1] + ar * (x[i - 1] - x[i - 2]) + sd * z[i - 1]
  }

  return(x)
}

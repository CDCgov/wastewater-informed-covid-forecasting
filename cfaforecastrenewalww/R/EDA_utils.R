# Functions used in EDA.Rmd-----------------------------------------------------

#' Get y range required to zoom a boxplot in to ignore outliers
#' df is the data, x the grouping variable, y the variable to be boxplotted
#' @export
zoom_boxplot_y <- function(df, x, y) {
  whisker_df <- df %>% reframe(boxplot_whiskers(!!sym(y)), .by = !!sym(x))
  return(range(whisker_df[[2]]))
}

#' Get the upper and lower ends of the whiskers of a single boxplot of data in x
#' @export
boxplot_whiskers <- function(x) {
  boxplot.stats(x)$stats[c(1, 5)]
}

#' x -> (x - location(x))/dispersion(x)
#' location is mean, dispersion is SD or MAD
#' @export
standardize <- function(x, use_mad = FALSE) {
  denom <- sd(x, na.rm = TRUE)
  if (use_mad) {
    denom <- mad(x, na.rm = TRUE)
  }
  (x - mean(x, na.rm = TRUE)) / denom
}

#' As standardize, but on log scale, possibly removing zeros (or near-zeros)
#' @export
log_standardize <- function(x, use_mad = FALSE, zero_threshold = 1e-8) {
  x[x < zero_threshold] <- NA
  standardize(log(x), use_mad = use_mad)
}

#' Entropy of probability mass function p
#' @export
entropy <- function(p) {
  summand <- p * log(p)
  -sum(summand[is.finite(summand)])
}

#' Entropy of sample from categorical distribution
#' @export
categorical_entropy <- function(x, cats = NA) {
  n <- length(x)
  if (any(is.na(cats))) {
    cats <- unique(x)
  }
  p <- sapply(cats, function(k) {
    sum(x == k) / n
  })
  return(entropy(p))
}

#' Is there any variation in x?
#' @export
varies <- function(x, exclude_na = TRUE) {
  if (exclude_na) {
    x <- x[!is.na(x)]
  }
  length(unique(x)) > 1
}

#' Maximum finite difference of x
#' @export
max_delta <- function(x) {
  max(abs(x[-1] - x[-length(x)]))
}

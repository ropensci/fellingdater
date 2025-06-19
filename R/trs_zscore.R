#' Standardize tree-ring series to z-scores
#'
#' Converts each series (column) in an rwl-style `data.frame` to z-scores by subtracting the mean and dividing by the standard deviation.
#'
#' @param x A `data.frame` of class `"rwl"`, where each column is a tree-ring series.
#'
#' @return A `data.frame` of the same dimensions as `x`, with each column transformed to z-scores.
#'
#' @examples
#' rwl <- trs_pseudo_rwl(n_series = 3, series_length = 50, end_date = 1990)
#' z_rwl <- trs_zscore(rwl)
#' apply(z_rwl, 2, mean, na.rm = TRUE) # should be ~0
#' apply(z_rwl, 2, sd, na.rm = TRUE) # should be ~1
#'
#' @export
trs_zscore <- function(x) {
  # Input validation
  if (!is.data.frame(x)) {
    stop("Input must be a data.frame")
  }

  if (nrow(x) == 0 || ncol(x) == 0) {
    stop("Input data.frame cannot be empty")
  }

  original_class <- class(x)
  years <- rownames(x)

  z <- as.data.frame(lapply(x, function(y) {
    # Remove NAs for calculation
    valid_vals <- y[!is.na(y)]

    # Check for insufficient data or zero variance
    if (length(valid_vals) < 2) {
      return(rep(NA_real_, length(y)))
    }

    mean_y <- mean(valid_vals)
    sd_y <- stats::sd(valid_vals)

    # Handle zero variance case
    if (sd_y == 0 || is.na(sd_y)) {
      return(rep(NA_real_, length(y)))
    }
    # Calculate z-scores
    (y - mean_y) / sd_y
  }))

  rownames(z) <- years
  class(z) <- original_class
  return(z)
}

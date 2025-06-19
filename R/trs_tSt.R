#' Compute Student's t-statistics from correlation coefficients
#'
#' This function computes correlation coefficients and their corresponding
#' Student's t-statistics between all pairs of series from two tree-ring datasets.
#'
#' @param x A data frame of test tree-ring series in `rwl` format (years as rownames,
#'   series as columns). All columns must be numeric.
#' @param y A data frame of reference tree-ring series in `rwl` format.
#' @param min_overlap Integer. Minimum number of overlapping years required between
#'   series pairs to compute statistics. Must be >= 3. Default is 30.
#' @param as_df Logical. If `TRUE`, returns results as data frames. If `FALSE`,
#'   returns as matrices. Default is `TRUE`.
#'
#' @return A list containing:
#' \describe{
#'   \item{r}{Correlation coefficients between series pairs}
#'   \item{t}{Student's t-statistics}
#'   \item{overlap}{Number of overlapping years between series pairs}
#' }
#'
#' @details
#' The function computes Pearson correlation coefficients between all pairs of
#' series from the two input datasets, then converts these to Student's t-statistics
#' using the formula: t = r * sqrt(n-2) / sqrt(1-r²), where n is the number of
#' overlapping observations.
#'
#' @export
trs_tSt <- function(x, y = NULL, min_overlap = 30, as_df = FALSE) {
  stopifnot(is.data.frame(x))
  if (is.null(y)) y <- x
  stopifnot(is.data.frame(y))

  nx <- ncol(x)
  ny <- ncol(y)

  r_mat <- matrix(NA_real_,
    nrow = nx, ncol = ny,
    dimnames = list(colnames(x), colnames(y))
  )
  tSt_mat <- matrix(NA_real_,
    nrow = nx, ncol = ny,
    dimnames = list(colnames(x), colnames(y))
  )
  overlap_mat <- matrix(NA_integer_,
    nrow = nx, ncol = ny,
    dimnames = list(colnames(x), colnames(y))
  )

  for (i in seq_len(nx)) {
    for (j in seq_len(ny)) {
      xi <- x[, i]
      yj <- y[, j]
      valid_idx <- which(!is.na(xi) & !is.na(yj))
      ovl <- length(valid_idx)
      overlap_mat[i, j] <- ovl

      if (ovl >= min_overlap) {
        r <- suppressWarnings(stats::cor(xi[valid_idx], yj[valid_idx], method = "pearson"))
        r_mat[i, j] <- r

        if (!is.na(r) && abs(r) < 0.99999 && ovl > 2) {
          tSt_mat[i, j] <- round(r * sqrt(ovl - 2) / sqrt(1 - r^2), 2)
        } else if (!is.na(r) && abs(r) >= 0.99999) {
          tSt_mat[i, j] <- sign(r) * Inf
        }
      }
    }
  }

  if (as_df) {
    df <- expand.grid(series = colnames(x), reference = colnames(y), KEEP.OUT.ATTRS = FALSE)
    df$r_pearson <- as.vector(r_mat)
    df$t_St <- as.vector(tSt_mat)
    df$overlap <- as.vector(overlap_mat)
    return(df)
  } else {
    return(list(r_pearson = r_mat, t_St = tSt_mat, overlap = overlap_mat))
  }
}

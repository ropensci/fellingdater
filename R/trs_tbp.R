#' Compute Baillie & Pilcher-style t-values between all series in two rwl-style data frames
#'
#' This function computes crossdating t-statistics between all series in two datasets
#' using the Baillie & Pilcher (1973) method. Series are standardized using a 5-year
#' centered moving average and log-transformed before computing correlations and t-values.
#'
#' @param x A data frame of test tree-ring series in `rwl` format (years as rownames,
#'   series as columns). All columns must be numeric.
#' @param y A data frame of reference tree-ring series in `rwl` format. If `NULL`,
#'   uses `x` for self-comparison. Default is `NULL`.
#' @param min_overlap Integer. Minimum number of overlapping years required between
#'   series pairs to compute t-values. Must be >= 3. Default is 50.
#' @param as_df Logical. If `TRUE`, returns results in long-format data frame.
#'   If `FALSE`, returns list of matrices. Default is `FALSE`.
#' @param transform Logical. If `TRUE`, applies Baillie & Pilcher transformation
#'   via `trs_tbp_transform()`. If `FALSE`, assumes input data is already transformed.
#'   Default is `TRUE`.
#'
#' @return Depending on `as_df` parameter:
#' \itemize{
#'   \item If `as_df = FALSE` (default): A list containing:
#'     \itemize{
#'       \item `t_BP`: Matrix of t-values with test series as rows, reference series as columns
#'       \item `overlap`: Matrix of overlap counts (number of common years) with same dimensions
#'     }
#'   \item If `as_df = TRUE`: A data frame with columns:
#'     \itemize{
#'       \item `series`: Name of test series
#'       \item `reference`: Name of reference series
#'       \item `t_BP`: Baillie & Pilcher t-value
#'       \item `overlap`: Number of overlapping years
#'     }
#' }
#'
#' @details
#' The Baillie & Pilcher method involves:
#' \enumerate{
#'   \item Alignment of series by common years
#'   \item Application of 5-year centered moving average to each series
#'   \item Log transformation: \eqn{\log(100 \times \text{value} / \text{moving average})}
#'   \item Computation of Pearson correlation between transformed series
#'   \item Calculation of t-statistic: \eqn{t = r \sqrt{(n-4-2)} / \sqrt{1-r^2}}
#' }
#'
#' Where \eqn{n} is the number of overlapping observations and is adjusted by
#' subtracting 4 to account for degrees of freedom lost in the moving average.
#'
#' Negative correlations are set to 0, and perfect correlations (|r| = 1) result
#' in infinite t-values.
#'
#' @references
#' Baillie, M.G.L. & Pilcher, J.R. (1973). A simple crossdating program for
#' tree-ring research. *Tree-Ring Bulletin*, 33, 7–14.
#'
#' @examples
#' \dontrun{
#' # Create sample data
#' rwl_test <- trs_pseudo_rwl(n_series = 3, series_length = 100, end_date = 2020)
#' rwl_ref <- trs_pseudo_rwl(n_series = 2, series_length = 80, end_date = 2015)
#'
#' # Compute t-values (matrix format)
#' result <- trs_tbp(rwl_test, rwl_ref, min_overlap = 30)
#' print(result$t_BP)
#' print(result$overlap)
#'
#' # Compute t-values (data frame format)
#' result_df <- trs_tbp(rwl_test, rwl_ref, min_overlap = 30, as_df = TRUE)
#' print(result_df)
#'
#' # Self-comparison within single dataset
#' self_comparison <- trs_tbp(rwl_test, min_overlap = 40)
#' }
#'
#' @seealso \code{\link{trs_tbp_transform}} for the transformation function
#'
#' @export
trs_tbp <- function(x, y = NULL, min_overlap = 50, as_df = FALSE, transform = TRUE) {
  stopifnot(is.data.frame(x))
  if (is.null(y)) y <- x
  stopifnot(is.data.frame(y))

  if (!all(sapply(x, is.numeric)) || !all(sapply(y, is.numeric))) {
    stop("Both 'x' and 'y' must be numeric data.frames (rwl-style).")
  }
  if (min_overlap < 3 || min_overlap %% 1 != 0) {
    stop("'min_overlap' should be a single integer >= 3")
  }

  common_years <- intersect(rownames(x), rownames(y))
  if (length(common_years) < 5) {
    stop("Not enough overlapping years between 'x' and 'y'")
  }

  x <- x[common_years, , drop = FALSE]
  y <- y[common_years, , drop = FALSE]

  if (transform) {
    tbp_x <- trs_tbp_transform(x)
    tbp_y <- trs_tbp_transform(y)
  } else {
    tbp_x <- as.matrix(x)
    tbp_y <- as.matrix(y)
  }

  nx <- ncol(tbp_x)
  ny <- ncol(tbp_y)

  tbp_mat <- matrix(NA_real_,
    nrow = nx, ncol = ny,
    dimnames = list(colnames(x), colnames(y))
  )
  overlap_mat <- matrix(NA_integer_,
    nrow = nx, ncol = ny,
    dimnames = list(colnames(x), colnames(y))
  )

  for (i in seq_len(nx)) {
    for (j in seq_len(ny)) {
      xi <- tbp_x[, i]
      yj <- tbp_y[, j]
      shared <- which(!is.na(xi) & !is.na(yj))
      ovl <- length(shared)
      overlap_mat[i, j] <- ovl

      if (ovl >= min_overlap) {
        r <- suppressWarnings(stats::cor(xi[shared], yj[shared], method = "pearson"))
        if (!is.na(r) && abs(r) >= 0.99999) {
          tbp_mat[i, j] <- sign(r) * Inf
        } else {
          if (is.na(r) || r < 0) r <- 0
          adj_ovl <- max(ovl - 4, 1)
          tbp_mat[i, j] <- round(r * sqrt(adj_ovl - 2) / sqrt(1 - r^2), 2)
        }
      }
    }
  }

  if (as_df) {
    df <- expand.grid(series = colnames(x), reference = colnames(y), KEEP.OUT.ATTRS = FALSE)
    df$t_BP <- as.vector(tbp_mat)
    df$overlap <- as.vector(overlap_mat)
    return(df)
  }

  return(list(
    t_BP = tbp_mat,
    overlap = overlap_mat
  ))
}

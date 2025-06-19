#' Compute Hollstein-style t-values between all series in two rwl-style data frames
#'
#' This function calculates Hollstein (1980) t-values for all pairwise comparisons
#' between tree-ring series in two data frames, using log-transformed growth ratios.
#' The method computes year-to-year growth ratios and applies correlation-based
#' t-statistics for crossdating analysis.
#'
#' @param x A data frame of test tree-ring series in `rwl` format (years as rownames,
#'   series as columns). All columns must be numeric.
#' @param y A data frame of reference tree-ring series in `rwl` format. If `NULL`,
#'   uses `x` for self-comparison. Default is `NULL`.
#' @param min_overlap Integer. Minimum number of overlapping years required between
#'   series pairs to compute t-values. Must be >= 3. Default is 50.
#' @param as_df Logical. If `TRUE`, returns results in long-format data frame.
#'   If `FALSE`, returns list of matrices. Default is `FALSE`.
#' @param transform Logical. If `TRUE`, applies Hollstein transformation (log growth ratios).
#'   If `FALSE`, assumes input data is already transformed. Default is `TRUE`.
#'
#' @return Depending on `as_df` parameter:
#' \itemize{
#'   \item If `as_df = FALSE` (default): A list containing:
#'     \itemize{
#'       \item `t_Ho`: Matrix of Hollstein t-values with test series as rows, reference series as columns
#'       \item `overlap`: Matrix of overlap counts (number of common years) with same dimensions
#'     }
#'   \item If `as_df = TRUE`: A data frame with columns:
#'     \itemize{
#'       \item `series`: Name of test series
#'       \item `reference`: Name of reference series
#'       \item `t_Ho`: Hollstein t-value
#'       \item `overlap`: Number of overlapping years
#'     }
#' }
#'
#' @details
#' The Hollstein method involves:
#' \enumerate{
#'   \item Alignment of series by common years
#'   \item Computation of log-transformed growth ratios: \eqn{100 \times \log_{10}(x_t / x_{t-1})}
#'   \item Calculation of Pearson correlation between transformed series
#'   \item Computation of t-statistic: \eqn{t = r \sqrt{n-3} / \sqrt{1-r^2}}
#' }
#'
#' Where \eqn{n} is the number of overlapping observations (original years, not growth ratios).
#' The degrees of freedom are adjusted by subtracting 3 to account for the growth ratio transformation.
#'
#' Negative correlations are set to 0, and perfect correlations (|r| ≥ 1) result
#' in infinite t-values to handle floating-point precision issues.
#'
#' @references
#' Hollstein, E. (1980). *Mitteleuropäische Eichenchronologie*. Verlag Philipp von Zabern, Mainz.
#'
#' @examples
#' \dontrun{
#' # Create sample data
#' rwl_test <- trs_pseudo_rwl(n_series = 3, series_length = 100, end_date = 2020)
#' rwl_ref <- trs_pseudo_rwl(n_series = 2, series_length = 80, end_date = 2015)
#'
#' # Compute Hollstein t-values (matrix format)
#' result <- trs_tho(rwl_test, rwl_ref, min_overlap = 30)
#' print(result$t_Ho)
#' print(result$overlap)
#'
#' # Compute t-values (data frame format)
#' result_df <- trs_tho(rwl_test, rwl_ref, min_overlap = 30, as_df = TRUE)
#' print(result_df)
#'
#' # Self-comparison within single dataset
#' self_comparison <- trs_tho(rwl_test, min_overlap = 40)
#' }
#'
#' @seealso \code{\link{trs_tbp}} for Baillie & Pilcher t-values
#'
#' @export
trs_tho <- function(x, y = NULL, min_overlap = 50, as_df = FALSE, transform = TRUE) {
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
  if (length(common_years) < 3) {
    stop("Not enough overlapping years between 'x' and 'y'")
  }

  x <- x[common_years, , drop = FALSE]
  y <- y[common_years, , drop = FALSE]

  if (transform) {
    wuch_x <- trs_tho_transform(x)
    wuch_y <- trs_tho_transform(y)
  } else {
    wuch_x <- as.matrix(x)
    wuch_y <- as.matrix(y)
  }

  nx <- ncol(wuch_x)
  ny <- ncol(wuch_y)

  tho_mat <- matrix(NA_real_,
    nrow = nx, ncol = ny,
    dimnames = list(colnames(x), colnames(y))
  )
  overlap_mat <- matrix(NA_integer_,
    nrow = nx, ncol = ny,
    dimnames = list(colnames(x), colnames(y))
  )

  for (i in seq_len(nx)) {
    for (j in seq_len(ny)) {
      xi <- wuch_x[, i]
      yj <- wuch_y[, j]
      shared_years <- which(!is.na(xi) & !is.na(yj))
      ovl <- length(shared_years) + 1
      overlap_mat[i, j] <- ovl

      if (ovl >= min_overlap) {
        r <- suppressWarnings(stats::cor(xi[shared_years], yj[shared_years], method = "pearson"))

        if (!is.na(r) && abs(r) >= 0.999999) {
          tho_mat[i, j] <- sign(r) * Inf
        } else {
          if (is.na(r) || r < 0) r <- 0
          df_adj <- ovl - 3 # 2 - 1 degrees of freedom adjustment
          tho_mat[i, j] <- round(r * sqrt(df_adj) / sqrt(1 - r^2), 2)
        }
      }
    }
  }

  if (as_df) {
    df <- expand.grid(series = colnames(x), reference = colnames(y), KEEP.OUT.ATTRS = FALSE)
    df$t_Ho <- as.vector(tho_mat)
    df$overlap <- as.vector(overlap_mat)
    return(df)
  }

  return(list(
    t_Ho = tho_mat,
    overlap = overlap_mat
  ))
}

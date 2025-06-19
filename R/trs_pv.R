#' Computes statistics to describe parallel variations between two rwl-style data frames
#'
#' Compares tree-ring series in two data frames using Gleichläufigkeit (GLK) and
#' Synchronous Growth Changes (SGC), adapted from the `dplR` package.
#'
#' @param x A data frame of test tree-ring series (`rwl` format: years in rows, series in columns).
#' @param y A data frame of reference series (`rwl` format).
#' @param min_overlap Integer. Minimum number of overlapping years required for computing GLK/SGC statistics. Default is 50.
#' @param prob Logical. If TRUE (default), computes two-tailed p-values for GLK and SGC.
#' @param as_df Logical. If TRUE, returns a long-format data.frame with all combinations and results. Default is FALSE.
#'
#' @return If `as_df = FALSE`, a list with components:
#' \describe{
#'   \item{glk}{Matrix of Gleichläufigkeit values.}
#'   \item{glk_p}{Matrix of GLK p-values (if \code{prob = TRUE}).}
#'   \item{sgc}{Matrix of synchronous growth change values.}
#'   \item{ssgc}{Matrix of semi-synchronous growth change values.}
#'   \item{sgc_p}{Matrix of SGC p-values (if \code{prob = TRUE}).}
#'   \item{overlap}{Matrix of the number of overlapping years used in each comparison.}
#' }
#'
#' If `as_df = TRUE`, returns a data frame with columns:
#' \code{series}, \code{reference}, \code{glk}, \code{glk_p}, \code{sgc}, \code{ssgc}, \code{sgc_p}, and \code{overlap}.
#'
#' @export
trs_pv <- function(x, y = NULL, min_overlap = 50, prob = TRUE, as_df = FALSE) {
  stopifnot(is.data.frame(x))
  if (is.null(y)) y <- x
  stopifnot(is.data.frame(y))

  if (!all(sapply(x, is.numeric)) || !all(sapply(y, is.numeric))) {
    stop("Both 'x' and 'y' must be numeric data.frames (rwl-style).")
  }
  if (min_overlap < 3 || min_overlap %% 1 != 0) {
    stop("'min_overlap' should be a single integer >= 3")
  }
  if (!is.logical(prob) || !is.logical(as_df)) {
    stop("'prob' and 'as_df' must be logical values (TRUE/FALSE)")
  }
  if (min_overlap < 50) {
    warning("The minimum number of overlap is lower than 50. This might lead to statistically insignificant matches.")
  }

  common_years <- intersect(rownames(x), rownames(y))
  if (length(common_years) < 2) {
    stop("Not enough overlapping years between 'x' and 'y'")
  }
  x <- x[common_years, , drop = FALSE]
  y <- y[common_years, , drop = FALSE]

  trs_sign <- sign(apply(x, 2, diff))
  ref_sign <- sign(apply(y, 2, diff))

  nx <- ncol(x)
  ny <- ncol(y)

  make_mat <- function() {
    matrix(NA_real_, nrow = nx, ncol = ny, dimnames = list(colnames(x), colnames(y)))
  }

  glk_mat <- make_mat()
  glk_p <- if (prob) make_mat() else NULL
  sgc_mat <- make_mat()
  ssgc_mat <- make_mat()
  sgc_p <- if (prob) make_mat() else NULL
  overlap_mat <- make_mat()

  for (i in seq_len(nx)) {
    trs_i <- trs_sign[, i]
    for (j in seq_len(ny)) {
      ref_j <- ref_sign[, j]
      valid_idx <- which(!is.na(trs_i) & !is.na(ref_j))
      ovl <- length(valid_idx) + 1 # due to diff()
      overlap_mat[i, j] <- ovl # Always record actual overlap

      if (ovl >= min_overlap) {
        gc <- abs(trs_i[valid_idx] - ref_j[valid_idx])

        glk_mat[i, j] <- 1 - sum(gc) / (2 * (ovl - 1))
        sgc_mat[i, j] <- sum(gc == 0) / (ovl - 1)
        ssgc_mat[i, j] <- sum(gc == 1) / (ovl - 1)
      }
    }
  }

  # Compute p-values
  if (prob) {
    s_mat <- 1 / (2 * sqrt(overlap_mat))

    z_glk <- (glk_mat - 0.5) / s_mat
    glk_p[] <- 2 * (1 - pnorm(z_glk))

    z_sgc <- (sgc_mat - 0.5) / s_mat
    sgc_p[] <- 2 * (1 - pnorm(z_sgc))
  }

  # Return as data.frame if requested
  if (as_df) {
    df <- expand.grid(series = colnames(x), reference = colnames(y), KEEP.OUT.ATTRS = FALSE)
    df$glk <- round(as.vector(glk_mat) * 100, 1)
    df$glk_p <- if (prob) as.vector(glk_p) else NA
    df$sgc <- round(as.vector(sgc_mat) * 100, 1)
    df$ssgc <- round(as.vector(ssgc_mat) * 100, 1)
    df$sgc_p <- if (prob) as.vector(sgc_p) else NA
    df$overlap <- as.vector(overlap_mat)
    return(df)
  }

  # Return as list
  results <- list(
    glk = round(glk_mat * 100, 1),
    sgc = round(sgc_mat * 100, 1),
    ssgc = round(ssgc_mat * 100, 1),
    overlap = overlap_mat
  )
  if (prob) {
    results$glk_p <- glk_p
    results$sgc_p <- sgc_p
  }

  return(results)
}

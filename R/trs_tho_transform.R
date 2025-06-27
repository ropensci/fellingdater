#' Pre-transform tree-ring series using Hollstein method
#'
#' Computes log-transformed growth ratios for tree-ring series following
#' the Hollstein (1980) method: \code{100 * log10(value_t / value_(t-1))}.
#' This transformation emphasizes year-to-year growth variations.
#'
#' @param x A rwl-style data frame with years as rownames and series as columns.
#'   All columns must be numeric.
#'
#' @return A matrix of log-transformed growth ratios with the same dimensions
#'   as the input. The first row (first year) is set to NA since no previous
#'   year exists for ratio calculation. Row names match the input data.
#'
#' @details
#' The Hollstein transformation computes year-to-year growth ratios:
#'
#' \deqn{G_t = 100 \times \log_{10}(x_t / x_{t-1})}
#'
#' where \eqn{x_t} is the ring width at year \eqn{t}. This transformation:
#' \itemize{
#'   \item Emphasizes relative changes between consecutive years
#'   \item Reduces the influence of long-term trends
#'   \item Makes series more suitable for crossdating analysis
#' }
#'
#' Values that would result in undefined logarithms (zero or negative ratios)
#' are set to NA. The first year of each series is always NA since no
#' previous year exists for ratio calculation.
#'
#' @references
#' Hollstein, E. (1980). *Mitteleuropäische Eichenchronologie*.
#' Verlag Philipp von Zabern, Mainz.
#'
#' @examples
#' \dontrun{
#' # Create sample rwl data
#' rwl_data <- trs_pseudo_rwl(n_series = 3, series_length = 50, end_date = 2020)
#'
#' # Apply Hollstein transformation
#' transformed <- trs_tho_transform(rwl_data)
#'
#' # Check dimensions (should be one row fewer)
#' nrow(rwl_data) # Original rows
#' nrow(transformed) # One fewer row
#'
#' # View first few transformed values
#' head(transformed)
#' }
#'
#' @seealso
#' \code{\link{trs_tbp_transform}} for Baillie & Pilcher transformation,
#' \code{\link{trs_tho}} for computing Hollstein t-values
#'
#' @export
trs_tho_transform <- function(x) {
  stopifnot(is.data.frame(x), all(sapply(x, is.numeric)))

  # Convert to matrix for efficient computation
  vals <- as.matrix(x)
  n_rows <- nrow(vals)
  n_cols <- ncol(vals)

  if (n_rows < 2) {
    stop("Input data must have at least 2 rows to compute growth ratios")
  }

  # Initialize output matrix with same dimensions as input
  log_growth <- matrix(NA_real_, nrow = n_rows, ncol = n_cols)
  rownames(log_growth) <- rownames(x)
  colnames(log_growth) <- colnames(x)

  # Compute growth ratios for rows 2 to n: current year / previous year
  for (i in 2:n_rows) {
    ratios <- vals[i, ] / vals[i - 1, ]
    # Apply log10 transformation and scale by 100
    # suppressWarnings to handle cases where ratios <= 0
    log_growth[i, ] <- suppressWarnings(100 * log10(ratios))
  }

  # Set infinite or NaN values to NA (from zero or negative ratios)
  log_growth[!is.finite(log_growth)] <- NA

  # First row remains NA (no previous year for ratio calculation)

  return(data.frame(log_growth))
}

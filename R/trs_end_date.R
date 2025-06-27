#' Assign calendar years to tree-ring series in an rwl-style data frame
#'
#' Sets the row names (years) of an rwl-style data frame based on a specified end year.
#' Optionally trims trailing rows with only `NA` values.
#'
#' @param x A `data.frame` of class `"rwl"`, with each column representing a tree-ring series.
#' @param end_year An integer. The calendar year assigned to the most recent tree ring (last row).
#' @param trim Logical. If `TRUE`, the data frame is trimmed to remove leading and trailing rows with no data (see \code{\link{trs_trim}}).
#'
#' @return A `data.frame` with row names set to calendar years.
#'
#' @examples
#' rwl <- trs_pseudo_rwl(n_series = 2, series_length = 50)
#' trs_end_date(rwl, end_year = 2000)
#' trs_end_date(rwl, end_year = 2000, trim = TRUE)
#'
#' @export
trs_end_date <- function(x, end_year = 2025, trim = FALSE) {
  # Input validation
  if (!is.data.frame(x)) {
    stop("'x' must be a data.frame.")
  }
  if (!is.numeric(end_year) || length(end_year) != 1 || !is.finite(end_year)) {
    stop("'end_year' should be a single numeric value representing a calendar year.")
  }
  if (!is.logical(trim) || length(trim) != 1) {
    stop("'trim' must be a single logical value.")
  }
  if (ncol(x) != 1) message("End date set for all series in x")

  original_class <- class(x)

  series_length <- nrow(x)
  years <- seq(end_year - series_length + 1, end_year, 1)
  rownames(x) <- years

  if (trim) {
    x <- trs_trim(x)
  }

  class(x) <- original_class

  return(x)
}

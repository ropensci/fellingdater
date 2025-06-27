#' Select series from an rwl-style data frame
#'
#' Extracts one or more tree-ring series (columns) from a data frame of class `"rwl"`.
#' Optionally adds the rownames (assumed to be years) as a column.
#'
#' @param x A `data.frame` of class `"rwl"`, where each column is a tree-ring width series and rownames represent years.
#' @param series A character string or vector specifying the name(s) of the series (columns) to extract.
#' @param rownames_to_years Logical. If `TRUE`, the rownames of `x` (years) are added as a column named \code{years}.
#' @param trim Logical. If `TRUE`, leading and trailing all-NA rows are removed using `trs_trim()`.
#'
#' @return A `data.frame` with the selected series. If `rownames_to_years = TRUE`, the result will have an additional \code{year} column.
#'   If `trim = TRUE`, leading and trailing all-NA rows are removed. If a single series is selected, returns a single-column data frame.
#'
#' @examples
#' rwl <- trs_pseudo_rwl(n_series = 5, series_length = 60, end_date = 2000)
#'
#' # Select single series
#' trs_select(rwl, series = "trs_1")
#' trs_select(rwl, series = "trs_2", rownames_to_years = TRUE)
#'
#' # Select multiple series
#' trs_select(rwl, series = c("trs_1", "trs_2"))
#' trs_select(rwl, series = c("trs_1", "trs_3", "trs_5"), rownames_to_years = TRUE)
#'
#' @export
trs_select <- function(x, series, rownames_to_years = FALSE, trim = FALSE) {
  # Input validation
  if (!is.data.frame(x)) {
    stop("Input must be a data.frame.")
  }

  if (!is.character(series) || length(series) == 0) {
    stop("`series` must be a non-empty character vector.")
  }

  if (!is.logical(rownames_to_years) || length(rownames_to_years) != 1) {
    stop("`rownames_to_years` must be a single logical value.")
  }

  if (!is.logical(trim) || length(trim) != 1) {
    stop("`trim` must be a single logical value.")
  }

  # Check if all requested series exist
  missing_series <- series[!series %in% names(x)]
  if (length(missing_series) > 0) {
    if (length(missing_series) == 1) {
      stop(paste("Series", shQuote(missing_series), "not found in dataframe."))
    } else {
      stop(paste("Series", paste(shQuote(missing_series), collapse = ", "), "not found in dataframe."))
    }
  }

  original_class <- class(x)

  # Extract selected series
  z <- x[, series, drop = FALSE]

  # Trim selected series if requested
  if (trim) {
    z <- trs_trim(z)
  }

  # Add year column if requested
  if (rownames_to_years) {
    z$year <- as.numeric(rownames(x))
  }

  class(z) <- original_class
  return(z)
}

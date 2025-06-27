#' Trim leading and trailing rows with only NA values from a tree-ring data frame
#'
#' Removes rows at the beginning and end of a `data.frame` of class `"rwl"` that contain only missing values across all series.
#'
#' @param x A `data.frame` or matrix where each column represents a tree-ring series and rows are years. Rownames must be numeric years.
#' @param rownames_to_years Logical. If `TRUE`, the rownames (years) will be transferred to a new column named \code{year}.
#'
#' @return A trimmed `data.frame` with all leading and trailing all-NA rows removed. If \code{rownames_to_years = TRUE}, a column \code{year} is added.
#'
#' @examples
#' x <- trs_pseudo_rwl(n_series = 3, series_length = 80, end_date = 1990)
#' x[1:5, ] <- NA # Add leading NA rows
#' x[81:85, ] <- NA # Add trailing NA rows
#' dim(x)
#' x_trim <- trs_trim(x)
#' dim(x_trim)
#'
#' # With year column added
#' head(trs_trim(x, rownames_to_years = TRUE))
#'
#' @export
trs_trim <- function(x, rownames_to_years = FALSE) {
  if (!is.data.frame(x)) stop("Input must be a data.frame.")

  if (!is.numeric(as.numeric(rownames(x)))) {
    stop("Rownames must be numeric years.")
  }

  original_class <- class(x)

  not_all_na <- apply(x, 1, function(row) !all(is.na(row)))
  keep_rows <- which(not_all_na)
  # row_sums <- rowSums(x, na.rm = TRUE)
  # keep_rows <- which(row_sums > 0)
  x_trim <- x[min(keep_rows):max(keep_rows), , drop = FALSE]

  if (rownames_to_years) {
    x_trim$year <- as.numeric(rownames(x_trim))
  }
  class(x_trim) <- original_class
  return(x_trim)
}

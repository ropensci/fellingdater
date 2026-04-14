#' Removes one or more series from an rwl-style data frame
#'
#' Removes one or more tree-ring series (columns) from a data frame of class `"rwl"`. Optionally adds the rownames (assumed to be years) as a column.
#'
#' @param x A `data.frame` of class `"rwl"`, where each column is a tree-ring width series and rownames represent years.
#' @param series A character vector specifying the names of the series (columns) to remove.
#' @param trim Logical. If `TRUE`, leading and trailing all-NA rows are removed using `trs_trim()`.

#'
#' @return A `data.frame` with the specified series removed.
#'
#' @examples
#' rwl <- trs_pseudo_rwl(n_series = 3, series_length = 60, end_date = 2000)
#' trs_remove(rwl, series = "trs_1", trim = TRUE)
#' trs_remove(rwl, series = c("trs_1", "trs_3"))
#'
#' @export
trs_remove <- function(x, series, trim = FALSE) {
     # rownames_to_years = FALSE, removed
     # Input validation
     if (!is.data.frame(x)) {
          stop("Input must be a data.frame.")
     }

     if (!is.character(series)) {
          stop("'series' must be a character vector.")
     }

     # if (!is.logical(rownames_to_years) || length(rownames_to_years) != 1) {
     #   stop("'rownames_to_years' must be a single logical value.")
     # }

     if (!is.logical(trim) || length(trim) != 1) {
          stop("'trim' must be a single logical value.")
     }

     original_class <- class(x)

     missing_series <- setdiff(series, names(x))
     if (length(missing_series) > 0) {
          stop(paste(
               "The following series were not found in the data.frame:",
               paste(shQuote(missing_series), collapse = ", ")
          ))
     }

     # Check if removing all columns
     remaining_cols <- setdiff(names(x), series)
     if (length(remaining_cols) == 0) {
          # check!!! && !rownames_to_years
          stop("This would create an empty data.frame.") # Cannot remove all series unless 'rownames_to_years = TRUE'
     }

     # Remove specified columns
     z <- x[, !names(x) %in% series, drop = FALSE]

     # Trim selected series if requested
     if (trim) {
          z <- trs_trim(z)
     }

     class(z) <- original_class
     return(z)
}

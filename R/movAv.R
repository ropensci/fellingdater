#' movAV: computes a running mean on a time series
#'
#' @description
#' This is a vectorized function that computes a running mean/moving average
#'   over a window width `w` on a continuous (time) series.
#'
#' @param x A `numeric` vector of `length(x)`.
#' @param w The width of the window. When w is even, one more value from the
#'   future is included.
#' @param align Should be "center (default), "left" or "right".
#'
#'  * `align = "center"`: average assigned to the center of the window.
#'  * `align = "left"`: average of current value and next (w-1) values.
#'  * `align = "right"`: average of current value and previous (w-1) values
#'
#' @param edges Defines how values are computed at the start and end of the
#'  series:
#'  * `edges = "fill"`: average of reducing number of values near the edges of
#'  the vector
#'  (this is the default setting).
#'  * `edges = "nofill"`: fill with `NA` when window does not include w values.
#'
#' @return A `numeric` vector of `length(x)`.
#' @export

movAv <- function(x,
                  w = 11,
                  align = "center",
                  edges = "fill") {
     if (align == "center") {
          before <- floor((w - 1) / 2)
          after  <- ceiling((w - 1) / 2)
     } else if (align == "right") {
          before <- w - 1
          after  <- 0
     } else if (align == "left") {
          before <- 0
          after  <- w - 1
     } else {
          print("'align' should be 'center', 'left' or 'right'")
     }

     run_mean <- matrix(NA, nrow = length(x), ncol = 1)
     n <- length(x)
     if (edges == "fill") {
          for (i in 1:n) {
               if (is.na(x[i]) == FALSE) {
                    run_mean[i] <-
                         mean(x[max(0, (i - before)):(i + after)], na.rm = TRUE)
               }
               else if (is.na(x[i]) == TRUE) {
                    run_mean[i] <- NA
               }
               else{
                    print("Error in dataframe. Missing values?")
               }
          }

     } else if (edges == "nofill") {
          for (i in 1:n) {
               # when x start with non-NA
               if (i - before <= 0) {
                    run_mean[i] <- NA
               } else {
                    run_mean[i] <-
                         mean(x[max(0, (i - before)):(i + after)], na.rm = FALSE)
               }
          }

     } else {
          print("Take care of the edges! 'Edges' should be 'nofill' or 'fill'.")
     }

     return(run_mean)
}

#' Compute a running mean on a time series
#'
#' @description This vectorized function computes a running mean/moving average
#'   over a continuous (time) series with a specified window width.
#'
#' @param x A `numeric` vector of `length(x)`, with the time series data.
#' @param w The width of the moving average window. When `w`is even, one more
#'   value from the future is included.
#' @param align The alignment of the window relative to the current data point.
#'   It can be one of:
#'
#'  * `align = "center"`: The average is assigned to the center of the window (default).
#'  * `align = "left"`: The average includes the current value and the next (w-1) values.
#'  * `align = "right"`: The average includes the current value and the previous (w-1) values.
#'
#' @param edges Defines how values are computed at the start and end of the
#'   series. Options are:
#'  * `edges = "fill"`: The average is calculated with a decreasing number of
#'  values near the edges of the vector (default).
#'  * `edges = "nofill"`: The result includes `NA` values when the window does
#'   not cover w values.
#'
#' @return A `numeric` vector of the same length of `x` with the computed
#'   running mean values.
#' @examples
#' num_vec <- sample(seq(50, 100, 1), 100, replace = TRUE)
#' filtered <- mov_av(num_vec, w = 5, align = "center", edges = "nofill")
#' plot(num_vec, type = "l")
#' lines(filtered, col = "darkblue")
#'
#' @export

mov_av <- function(x,
                   w = 11,
                   align = "center",
                   edges = "fill") {

  if(!align %in% c("center", "left", "right")){
    stop("'align' should be 'center', 'left' or 'right'")
  }
  if(!edges %in% c("fill", "nofill")){
    stop("Take care of the edges! 'Edges' should be 'nofill' or 'fill'.")
  }

  if (align == "center") {
    before <- floor((w - 1) / 2)
    after <- ceiling((w - 1) / 2)
  }
  if (align == "right") {
    before <- w - 1
    after <- 0
  }
  if (align == "left") {
    before <- 0
    after <- w - 1
  }

  run_mean <- matrix(NA, nrow = length(x), ncol = 1)
  n <- length(x)
  if (edges == "fill") {
    for (i in 1:n) {
      if (is.na(x[i]) == FALSE) {
        run_mean[i] <-
          mean(x[max(0, (i - before)):(i + after)], na.rm = TRUE)
      } else if (is.na(x[i]) == TRUE) {
        run_mean[i] <- NA
      } else {
        print("Error in dataframe. Are there missing values?")
      }
    }
  }
  if (edges == "nofill") {
    for (i in 1:n) {
      if (i - before <= 0) {
        run_mean[i] <- NA
      } else {
        run_mean[i] <-
          mean(x[max(0, (i - before)):(i + after)], na.rm = FALSE)
      }
    }
  }

  return(run_mean)
}

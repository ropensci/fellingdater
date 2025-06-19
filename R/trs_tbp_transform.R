#' Pre-transform tree-ring series using Baillie & Pilcher method
#'
#' Applies a 5-year centered moving average and computes the log index:
#' \code{log(100 * value / MA5)}. Uses edge mirroring to maintain
#' constant window size and avoid NA values.
#'
#' @param x A rwl-style data frame.
#'
#' @return A matrix of log-transformed index values.
#' @export
trs_tbp_transform <- function(x) {
     stopifnot(is.data.frame(x), all(sapply(x, is.numeric)))

     mov_av_tbp <- function(x, w = 5) {
          n <- length(x)
          if (n < 3) {
               return(rep(NA, n))
          } # Handle very short series

          # Mirror padding: reflect 2 values at each edge
          # For a 5-point window, we need 2 extra points on each side
          pad_size <- floor(w / 2)

          # Create mirrored series
          # Left mirror: reverse order of first pad_size+1 values, exclude the boundary
          left_mirror <- rev(x[2:(pad_size + 1)])
          # Right mirror: reverse order of last pad_size+1 values, exclude the boundary
          right_mirror <- rev(x[(n - pad_size):(n - 1)])

          # Combine: mirrored_left + original + mirrored_right
          x_padded <- c(left_mirror, x, right_mirror)

          # Apply standard 5-point moving average to padded series
          k <- rep(1, w)
          not_na <- as.numeric(!is.na(x_padded))
          x_filled <- x_padded
          x_filled[is.na(x_padded)] <- 0
          sum_x <- stats::filter(x_filled, k, sides = 2)
          count_x <- stats::filter(not_na, k, sides = 2)
          result_padded <- as.numeric(sum_x / count_x)
          result_padded[count_x == 0] <- NA

          # Extract the original length portion (remove padding)
          result <- result_padded[(pad_size + 1):(pad_size + n)]

          return(result)
     }

     ma <- apply(as.matrix(x), 2, mov_av_tbp)
     log_trans <- suppressWarnings(log(100 * as.matrix(x) / ma))
     rownames(log_trans) <- rownames(x)
     return(data.frame(log_trans))
}

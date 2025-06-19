#' Create a pseudo-population of tree-ring series
#'
#' Generates a synthetic tree-ring width dataset in `rwl` format,
#' suitable for testing crossdating and chronology construction methods.
#'
#' @param n_series Integer. Number of tree-ring series to generate.
#' @param series_length Numeric or numeric vector of length 2. If a single number,
#'   all series have this fixed length. If a vector of two values, series lengths are
#'   randomly sampled between the minimum and maximum values (inclusive).
#' @param end_date Numeric or numeric vector of length 2. If a single number, all
#'   series end in this year. If a vector of two values, series end years are randomly
#'   sampled within this range.
#' @param related Logical. If `TRUE`, all series share a common growth pattern
#'   (i.e., internally correlated). If `FALSE`, each series is generated independently.
#' @param trend Character. Type of trend to apply to series. Options: `"neg_lin"` for
#'   a negative linear trend, `"neg_exp"` for a negative exponential trend, or `NULL` for no trend.
#' @param neg_lin_param Numeric. Slope for the linear decline (typically between -0.1 and -1.0).
#' @param neg_exp_param Numeric. Rate parameter for the exponential decline (typically between 0.01 and 0.1).
#' @param ar Logical. If `TRUE`, adds autocorrelation using an AR model. Note: when `related = TRUE`,
#'   AR is applied to the common base signal; when `related = FALSE`, AR is applied to each series individually.
#' @param ar_params Numeric vector. Autoregressive coefficients (e.g., `c(0.8)`, or `c(0.6, 0.2)`).
#' @param mean_rw Numeric. Mean ring width around which the data is centered (default is 100).
#' @param noise_sd Numeric. Standard deviation of noise added to each series (default is 8).
#' @param common_signal_strength Numeric. Strength of common signal when `related = TRUE` (0-1).
#'   Higher values produce higher inter-series correlations (rbar). Default 0.75 typically
#'   produces rbar around 0.5-0.7, which is realistic for tree-ring chronologies.
#' @param prefix Character. Prefix used when naming each tree-ring series (default is `"trs_"`).
#' @param seed Numeric. Random seed for reproducible results. If `NULL`, no seed is set.
#'
#' @return A `data.frame` of class `"rwl"`, with years as row names and each column
#'   representing one synthetic tree-ring series.
#'
#' @examples
#' # Basic usage
#' pseudo_data <- trs_pseudo_rwl(n_series = 5, series_length = 50)
#'
#' # Variable lengths and end dates with reproducible results
#' pseudo_data <- trs_pseudo_rwl(
#'   n_series = 10,
#'   series_length = c(80, 120),
#'   end_date = c(2020, 2024),
#'   seed = 123
#' )
#'
#' # Related series with strong common signal (high rbar)
#' pseudo_data <- trs_pseudo_rwl(
#'   n_series = 8,
#'   series_length = 100,
#'   related = TRUE,
#'   common_signal_strength = 0.85, # Should produce rbar ~ 0.7-0.8
#'   trend = "neg_exp",
#'   ar = TRUE,
#'   ar_params = c(0.7, 0.2),
#'   seed = 456
#' )
#'
#' @import stats
#'
#' @export
trs_pseudo_rwl <- function(n_series = 10,
                           series_length = 100,
                           end_date = 2024,
                           related = TRUE,
                           trend = NULL,
                           neg_lin_param = -0.1,
                           neg_exp_param = 0.05,
                           ar = TRUE,
                           ar_params = 0.7,
                           mean_rw = 100,
                           noise_sd = 8,
                           common_signal_strength = 0.75,
                           prefix = "trs_",
                           seed = NULL) {
  # Set seed if provided
  if (!is.null(seed)) {
    if (!is.numeric(seed) || length(seed) != 1) {
      stop("`seed` must be a single numeric value or NULL")
    }
    set.seed(seed)
  }

  # Input validation
  if (!is.numeric(n_series) || length(n_series) != 1 || n_series < 1 || n_series != as.integer(n_series)) {
    stop("`n_series` must be a positive integer")
  }
  if (!(length(series_length) %in% c(1, 2)) || !is.numeric(series_length)) {
    stop("`series_length` should be numeric or a vector of length 2")
  }
  if (length(series_length) == 2 && series_length[2] <= series_length[1]) {
    stop("Second value of `series_length` should be larger than the first")
  }
  if (any(series_length < 1)) {
    stop("`series_length` values must be positive")
  }
  if (!(length(end_date) %in% c(1, 2)) || !is.numeric(end_date)) {
    stop("`end_date` should be numeric or a vector of length 2")
  }
  if (length(end_date) == 2 && end_date[2] <= end_date[1]) {
    stop("Second value of `end_date` should be larger than the first")
  }
  if (!is.logical(related) || length(related) != 1) {
    stop("`related` must be a single logical value")
  }
  if (!is.null(trend) && !trend %in% c("neg_lin", "neg_exp")) {
    stop("`trend` must be NULL, 'neg_lin', or 'neg_exp'")
  }
  if (!is.logical(ar) || length(ar) != 1) {
    stop("`ar` must be a single logical value")
  }
  if (!is.numeric(ar_params) || any(!is.finite(ar_params))) {
    stop("`ar_params` must be numeric and finite")
  }
  if (ar && any(abs(ar_params) >= 1)) {
    warning("|AR parameter| >= 1 or sum of parameters is > 1, and may not ensure stationarity")
  }
  if (!is.numeric(mean_rw) || length(mean_rw) != 1 || mean_rw <= 0) {
    stop("`mean_rw` must be a positive numeric value")
  }
  if (!is.numeric(noise_sd) || length(noise_sd) != 1 || noise_sd <= 0) {
    stop("`noise_sd` must be a positive numeric value")
  }
  if (!is.numeric(common_signal_strength) || length(common_signal_strength) != 1 ||
    common_signal_strength < 0 || common_signal_strength > 1) {
    stop("`common_signal_strength` must be a numeric value between 0 and 1")
  }
  if (!is.character(prefix) || length(prefix) != 1) {
    stop("`prefix` must be a single character string")
  }

  # Helper functions
  get_length <- function() {
    if (length(series_length) == 1) {
      return(series_length)
    } else {
      return(sample(series_length[1]:series_length[2], 1))
    }
  }

  get_end_date <- function() {
    if (length(end_date) == 1) {
      return(end_date)
    } else {
      return(sample(end_date[1]:end_date[2], 1))
    }
  }

  get_signal_components <- function(common_signal_strength) {
    # Split signal strength between base (common) and individual components
    # Higher common_signal_strength = more correlation between series
    base_strength <- sqrt(common_signal_strength)
    individual_strength <- sqrt(1 - common_signal_strength)

    return(list(
      base = base_strength,
      individual = individual_strength
    ))
  }

  apply_trend <- function(series_vals, trend_type, length_i, neg_lin_param, neg_exp_param, mean_rw) {
    if (is.null(trend_type)) {
      return(series_vals)
    }

    if (trend_type == "neg_lin") {
      lin_param_jit <- neg_lin_param + stats::rnorm(1, mean = 0, sd = abs(neg_lin_param) * 0.15)
      trend_curve <- seq(0, length_i - 1) * lin_param_jit
      return(series_vals + trend_curve)
    }

    if (trend_type == "neg_exp") {
      exp_param_jit <- neg_exp_param + stats::rnorm(1, mean = 0, sd = abs(neg_exp_param) * 0.15)
      mean_rw_jit <- mean_rw + stats::rnorm(1, mean = 0, sd = abs(mean_rw) * 0.1)
      trend_curve <- mean_rw_jit * exp(-exp_param_jit * seq(0, length_i - 1))
      return(series_vals + trend_curve - mean(trend_curve))
    }
  }

  # Generate series metadata
  series_metadata <- data.frame(
    length = sapply(1:n_series, function(x) get_length()),
    end_year = sapply(1:n_series, function(x) get_end_date())
  )
  series_metadata$start_year <- series_metadata$end_year - series_metadata$length + 1

  # Determine full year range
  min_year <- min(series_metadata$start_year)
  max_year <- max(series_metadata$end_year)
  all_years <- min_year:max_year
  n_years <- length(all_years)

  # Pre-allocate result matrix
  result_matrix <- matrix(NA_real_, nrow = n_years, ncol = n_series)
  rownames(result_matrix) <- all_years
  colnames(result_matrix) <- paste0(prefix, formatC(1:n_series, width = nchar(n_series), flag = "0"))

  # Calculate signal components
  signal_components <- get_signal_components(common_signal_strength)

  # Generate common base signal if related = TRUE
  if (related) {
    if (ar) {
      full_base <- as.numeric(stats::arima.sim(
        model = list(ar = ar_params),
        n = n_years,
        sd = noise_sd * signal_components$base
      ))
    } else {
      full_base <- stats::rnorm(n_years, mean = 0, sd = noise_sd * signal_components$base)
    }
    names(full_base) <- all_years
  }

  # Generate individual series
  for (i in 1:n_series) {
    length_i <- series_metadata$length[i]
    start_i <- series_metadata$start_year[i]
    end_i <- series_metadata$end_year[i]
    years_i <- start_i:end_i
    year_indices <- match(years_i, all_years)

    # Generate base series values
    if (related) {
      # Use common base signal plus individual variation
      base_signal <- full_base[match(years_i, names(full_base))]
      if (ar) {
        # Apply AR to individual component
        individual_component <- as.numeric(stats::arima.sim(
          model = list(ar = ar_params),
          n = length_i,
          sd = noise_sd * signal_components$individual
        ))
        series_i <- base_signal + individual_component + mean_rw
      } else {
        series_i <- base_signal + stats::rnorm(length_i,
          mean = mean_rw,
          sd = noise_sd * signal_components$individual
        )
      }
    } else {
      # Generate independent series
      if (ar) {
        # Generate AR series centered around mean_rw
        ar_series <- as.numeric(stats::arima.sim(
          model = list(ar = ar_params),
          n = length_i,
          sd = noise_sd
        ))
        series_i <- ar_series + mean_rw
      } else {
        series_i <- stats::rnorm(length_i, mean = mean_rw, sd = noise_sd)
      }
    }

    # Apply trend
    series_i <- apply_trend(series_i, trend, length_i, neg_lin_param, neg_exp_param, mean_rw)

    # Round to integers (ring widths are typically measured as integers)
    series_i <- round(series_i, 0)

    # Store in result matrix
    result_matrix[year_indices, i] <- series_i
  }

  # Convert to data.frame and set proper class
  pseudo_rwl <- as.data.frame(result_matrix)

  # Remove rows with all NAs using existing trs_trim() function
  pseudo_rwl <- trs_trim(pseudo_rwl)

  # Set proper class
  class(pseudo_rwl) <- c("rwl", "data.frame")

  return(pseudo_rwl)
}

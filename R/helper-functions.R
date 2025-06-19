#' Helper function to scale a Probability Density Function (PDF) to a
#' Probability Frequency Function.
#' @noRd
d_dens <- function(densfun = densfun,
                   x = x,
                   param1 = 0,
                   param2 = 1,
                   log = FALSE,
                   n = 1) {
     if (!densfun %in% c("lognormal", "normal", "weibull", "gamma")) {
          stop(sprintf(
               "!!! '%s' is not a supported distribution !!!",
               densfun
          ))
     }
     if (densfun == "lognormal") {
          n * stats::dlnorm(
               x = x,
               meanlog = param1,
               sdlog = param2,
               log = log
          )
     } else if (densfun == "normal") {
          n * stats::dnorm(
               x = x,
               mean = param1,
               sd = param2,
               log = log
          )
     } else if (densfun == "weibull") {
          n * stats::dweibull(
               x = x,
               shape = param1,
               scale = param2,
               log = log
          )
     } else if (densfun == "gamma") {
          n * stats::dgamma(
               x = x,
               shape = param1,
               rate = param2,
               log = log
          )
     }
}


#' Helper function to rescale probabilities between [0, 1].
#' @noRd
rescale <- function(x,
                    floor = 0,
                    ceiling = 1) {
     if (max(x, na.rm = TRUE) == 0) {
          x
     } else {
          (x - min(x, na.rm = TRUE)) * (ceiling - floor) /
               (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
     }
}


#' Helper function to check input for sw_combine, fd_report and sw_sum
#' @noRd

check_input <- function(x = x,
                        series = series,
                        last = last,
                        n_sapwood = n_sapwood,
                        waneyedge = waneyedge,
                        sw_data = sw_data,
                        cred_mass = cred_mass,
                        densfun = densfun) {
     if (!is.data.frame(x)) {
          stop("Input 'x' must be a dataframe.")
     }

     # Check columns exist
     if (!series %in% names(x)) {
          stop("--> column 'series' does not exist")
     }
     if (!last %in% names(x)) {
          stop("--> column 'last' does not exist")
     }
     if (!n_sapwood %in% names(x)) {
          stop("--> column 'n_sapwood' does not exist")
     }
     if (!waneyedge %in% names(x)) {
          stop("--> column 'waneyedge' does not exist")
     }
     series <- x[, series] # check for NA's
     if (any(is.na(series))) {
          stop("--> some 'series' have no id")
     }

     if (is.character(x[, n_sapwood])) {
          # was !is.numeric !!!
          stop("--> 'n_sapwood' must be a numeric vector")
     }

     if (!is.numeric(x[, last])) {
          stop("--> 'last' must be a numeric vector")
     }

     if (!is.logical(x[, waneyedge])) {
          # check is.logical
          stop("--> 'waneyedge' should be a logical vector (TRUE/FALSE),
indicating the presence of waney edge.\n")
     }

     if (is.na(cred_mass) || cred_mass <= 0 || cred_mass >= 1) {
          stop("--> cred_mass must be between 0 and 1")
     }

     if (!is.character(sw_data)) {
          stop("--> sw_data should be one of `sw_data_overview()`
         or the name a data.frame with numeric values in columns
         `n_sapwood` and `count`.)")
     }

     if (all(
          !sw_data %in% sw_data_overview(),
          !exists(sw_data),
          !sw_data %in% colnames(x)
     )) {
          stop(
               sprintf(
                    "--> sw_data should be one of `sw_data_overview()`
or the name a data.frame with columns `n_sapwood` and `count`, not '%s'.",
                    sw_data
               )
          )
     }

     if (!densfun %in% c("lognormal", "normal", "weibull", "gamma")) {
          stop(
               sprintf(
                    "\n'%s' is not a supported distribution.
          \n`densfun` must be one of c('lognormal', 'normal', 'weibull', 'gamma')",
                    densfun
               )
          )
     }
}

#' Helper function to define a custom theme for plotting in ggplot
#' @noRd
theme_fdr <- function() {
     ggplot2::theme_minimal() +
          ggplot2::theme(
               plot.title = ggtext::element_markdown(),
               plot.title.position = "plot",
               axis.text = ggplot2::element_text(size = 10),
               panel.grid.minor.y = ggplot2::element_blank(),
               panel.grid.major.y = ggplot2::element_blank(),
               legend.position = "none",
               strip.background = ggplot2::element_blank(),
               strip.text.y = ggplot2::element_blank()
          )
}


#' Helper function to check input densfun
#' @noRd
check_densfun <- function(x) {
     if (!x %in% c("lognormal", "normal", "weibull", "gamma")) {
          stop(
               sprintf(
                    "\n'%s' is not a supported distribution.
          \n`densfun` must be one of c('lognormal', 'normal', 'weibull', 'gamma')",
                    x
               )
          )
     }
}

#' Helper function to check input plot
#' @noRd
check_plot <- function(x) {
     if (!is.logical(x)) {
          stop(
               sprintf(
                    "'plot' should be TRUE or FALSE, not '%s'",
                    x
               )
          )
     }
}

#' Helper function to check input cred_mass
#' @noRd
check_cred_mass <- function(x) {
     if (is.na(x) || x <= 0 || x >= 1) {
          stop("--> cred_mass must be between 0 and 1")
     }
}


#' Helper function to check input n_sapwood
#' @noRd
check_n_sapwood <- function(x) {
     if (is.na(x)) {
          message("--> n_sapwood must be a postitive numeric value, not 'NA'")
          return(NA_integer_)
     }

     if (!is.numeric(x)) {
          stop("--> n_sapwood must be a positive numeric value")
     }

     if (x < 0) {
          stop("--> n_sapwood must be a positive numeric value")
     }

     if (isTRUE(x %% 1 != 0)) {
          stop("--> n_sapwood must be an integer (no decimals allowed!)")
     }
}

#' Check if user-supplied sapwood data is valid
#'
#' This function validates a user-defined sapwood dataset by ensuring that it is a
#' `data.frame` with numeric `n_sapwood` and `count` columns, and that all
#' `n_sapwood` values are strictly positive.
#'
#' @param data An object to be tested (typically passed as `sw_data` in `sw_model()`).
#'
#' @return Invisibly returns `TRUE` if all checks pass. Otherwise, an error is thrown.
#'
#' @keywords internal
#' @noRd

check_sapwood_data_user <- function(data) {
     if (!is.data.frame(data)) {
          stop("User-defined sapwood data must be a data.frame.")
     }

     required_cols <- c("n_sapwood", "count")
     if (!all(required_cols %in% names(data))) {
          stop("User-defined data.frame must contain columns: `n_sapwood` and `count`.")
     }

     if (!is.numeric(data$n_sapwood) || !is.numeric(data$count)) {
          stop("Columns `n_sapwood` and `count` must both be numeric.")
     }

     if (anyNA(data$n_sapwood) || anyNA(data$count)) {
          stop("--> Columns `n_sapwood` and `count` must not contain NA values.")
     }

     if (any(data$n_sapwood <= 0)) {
          invalid_vals <- unique(data$n_sapwood[data$n_sapwood <= 0])
          stop(
               "All `n_sapwood` values must be strictly positive. Invalid values found: ",
               paste(invalid_vals, collapse = ", ")
          )
     }

     invisible(TRUE)
}

#' Check for duplicate or missing series labels
#'
#' @description This helper function checks that all series labels are unique and non-missing.
#' It is designed for use in workflows where series labels (e.g., tree-ring series IDs) are used
#' to identify and merge data, such as in [sw_sum()]. If any duplicate or missing values are
#' detected, an informative error is raised.
#'
#' @param x A character vector containing the series labels (typically a column extracted from
#' a data frame).
#'
#' @return No return value. If all labels are valid, the function exits silently. If duplicate or
#' missing labels are found, the function raises an error with a descriptive message.
#'
#' @keywords internal
#' @noRd
#'
check_duplicate_labels <- function(x) {
     dup_labs <- unique(x[duplicated(x) & !is.na(x)])
     no_label <- sum(is.na(x))

     msg <- character()

     if (length(dup_labs) > 0) {
          msg <- c(
               msg,
               paste("Duplicated series labels detected:", paste(dup_labs, collapse = ", "))
          )
     }

     if (no_label > 0) {
          msg <- c(
               msg,
               paste(no_label, "series have missing (NA) labels.")
          )
     }

     if (length(msg) > 0) {
          stop(paste(msg, collapse = "\n"))
     }
}

#' Check if input is a data.frame with one series
#'
#' Validates that the input is a data.frame with exactly one column (series).
#' Intended for use in functions that operate on single tree-ring series.
#'
#' @param x An object to check.
#' @param arg_name A string indicating the argument name (e.g., "x" or "y") for informative error messages.
#'
#' @return Invisibly returns `TRUE` if the input is valid; otherwise, throws an error.
#' @keywords internal
#'
check_single_series <- function(x, arg_name = "x") {
     if (!is.data.frame(x) || ncol(x) != 1) {
          stop(sprintf("`%s` must be a data.frame with exactly one series (one column). Use `trs_select()` to extract a single series.", arg_name), call. = FALSE)
     }
     invisible(TRUE)
}

check_consecutive <- function(df) {
     df_name <- deparse(substitute(df))
     years <- as.integer(rownames(df))
     if (any(is.na(years))) {
          stop(paste("Rownames of", df_name, "are not all numeric."))
     }
     if (!all(diff(years) == 1)) {
          stop(paste("Rownames of", df_name, "are not a continuous sequence of years."))
     }
}

#' Detect synchronous growth changes between two tree-ring series ( helper for trs_plot_dated() )
#'
#' Identifies years when two tree-ring series show synchronous growth changes (SGC)
#' - both increasing or both decreasing from the previous year. Based on the logic
#' used in trs_pv() for distinguishing between synchronous and semi-synchronous growth.
#'
#' @param x A `data.frame` with one column of ring-width values and years as row names.
#' @param y A `data.frame` with one column of ring-width values and years as row names.
#'
#' @return A `data.frame` with two columns:
#'   \item{pv_logi}{Logical vector indicating synchronous growth changes (TRUE) or not (FALSE)}
#'   \item{year}{Numeric vector of years corresponding to the logical values}
#'
#' @details
#' The function compares year-to-year growth direction changes between two series
#' using the same logic as trs_pv():
#' - Synchronous Growth Changes (SGC): Both series change in the same direction (both + or both -)
#' - Semi-synchronous/Asynchronous: One series changes while other doesn't, or they change in opposite directions
#'
#' Only years with synchronous growth changes receive TRUE values.
#' Years with missing values in either series are marked as FALSE.
#' The first year cannot be evaluated (no previous year for comparison) and is marked as FALSE.
#'
#' @examples
#' # Create sample data
#' years <- 1950:1980
#' x <- data.frame(series1 = runif(length(years), 0.5, 2.0))
#' rownames(x) <- years
#' y <- data.frame(series2 = runif(length(years), 0.8, 1.8))
#' rownames(y) <- years
#'
#' # Detect synchronous growth changes
#' sync_growth <- sgc_for_plot(x, y)
#' head(sync_growth)
#'
#' @export
sgc_for_plot <- function(x, y) {
     # Input validation
     if (!is.data.frame(x) || ncol(x) != 1) {
          stop("x must be a data.frame with exactly one column")
     }
     if (!is.data.frame(y) || ncol(y) != 1) {
          stop("y must be a data.frame with exactly one column")
     }

     # Get years from row names and find common years
     common_years <- intersect(rownames(x), rownames(y))
     if (length(common_years) < 2) {
          stop("Need at least 2 overlapping years")
     }

     # Sort years and subset data to common years
     common_years <- sort(as.numeric(common_years))
     x_subset <- x[as.character(common_years), 1, drop = TRUE]
     y_subset <- y[as.character(common_years), 1, drop = TRUE]

     # Calculate year-to-year changes and their signs (following trs_pv logic)
     x_sign <- sign(diff(x_subset))
     y_sign <- sign(diff(y_subset))

     # Calculate growth comparison (gc) as in trs_pv
     # gc = 0 means synchronous (same direction), gc = 1 means semi-synchronous/asynchronous
     valid_idx <- which(!is.na(x_sign) & !is.na(y_sign))

     # Initialize result vector for all years (including first year which gets FALSE)
     n_years <- length(common_years)
     sync_logical <- logical(n_years)

     # For years where we can calculate differences (years 2 onwards)
     if (length(valid_idx) > 0) {
          gc <- abs(x_sign[valid_idx] - y_sign[valid_idx])
          # Only synchronous growth changes (gc == 0) get TRUE
          sgc_logical <- (gc == 0)
          ssgc_logical <- (gc == 1)

          # Map back to the full year sequence (adding 1 because diff removes first year)
          sgc_logical[valid_idx + 1] <- sgc_logical
          ssgc_logical[valid_idx + 1] <- ssgc_logical
     }

     # Create result data frame
     result <- data.frame(
          sgc_logi = sgc_logical,
          ssgc_logi = ssgc_logical,
          year = common_years,
          stringsAsFactors = FALSE
     )

     # Set row names to years for consistency
     rownames(result) <- as.character(common_years)

     return(result)
}

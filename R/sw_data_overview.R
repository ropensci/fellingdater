#' Overview of available sapwood data sets in the `fellingdater` package
#'
#' @description This function provides an overview of the available sapwood data
#'   sets and models within the `fellingdater` package.
#'
#' @return A `character` vector with the names of all available data sets with
#'   sapwood counts in the `fellingdater` package.
#'
#' @export
#'
#' @examples
#' # Get an overview of available sapwood data sets
#' sw_data_overview()
#'
sw_data_overview <- function() {
     tmp <- utils::data(package = "fellingdater")
     tmp <- tmp$results
     tmp <- tmp[, 3]
     tmp[!grepl("sw_example", tmp)]
}

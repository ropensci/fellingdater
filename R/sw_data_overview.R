#' Overview of available sapwood data sets in the `fellingdateR` package
#'
#' @description This function provides an overview of the available sapwood data sets and models within the `fellingdateR` package.
#'
#' @return A `character` vector with the names of all available data sets
#'  with sapwood counts in the `fellingdateR` package.
#'
#' @export
#'
#' @examples
#' # Get an overview of available sapwood data sets
#' sw_data_overview()
#'
sw_data_overview <- function() {

     tmp <- utils::data(package = "fellingdateR")
     tmp <- tmp$results
     tmp[, 3]
}

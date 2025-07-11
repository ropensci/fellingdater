% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trs_trim.R
\name{trs_trim}
\alias{trs_trim}
\title{Trim leading and trailing rows with only NA values from a tree-ring data frame}
\usage{
trs_trim(x, rownames_to_years = FALSE)
}
\arguments{
\item{x}{A \code{data.frame} or matrix where each column represents a tree-ring series and rows are years. Rownames must be numeric years.}

\item{rownames_to_years}{Logical. If \code{TRUE}, the rownames (years) will be transferred to a new column named \code{year}.}
}
\value{
A trimmed \code{data.frame} with all leading and trailing all-NA rows removed. If \code{rownames_to_years = TRUE}, a column \code{year} is added.
}
\description{
Removes rows at the beginning and end of a \code{data.frame} of class \code{"rwl"} that contain only missing values across all series.
}
\examples{
x <- trs_pseudo_rwl(n_series = 3, series_length = 80, end_date = 1990)
x[1:5, ] <- NA # Add leading NA rows
x[81:85, ] <- NA # Add trailing NA rows
dim(x)
x_trim <- trs_trim(x)
dim(x_trim)

# With year column added
head(trs_trim(x, rownames_to_years = TRUE))

}

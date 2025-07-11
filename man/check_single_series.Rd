% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper-functions.R
\name{check_single_series}
\alias{check_single_series}
\title{Check if input is a data.frame with one series}
\usage{
check_single_series(x, arg_name = "x")
}
\arguments{
\item{x}{An object to check.}

\item{arg_name}{A string indicating the argument name (e.g., "x" or "y") for informative error messages.}
}
\value{
Invisibly returns \code{TRUE} if the input is valid; otherwise, throws an error.
}
\description{
Validates that the input is a data.frame with exactly one column (series).
Intended for use in functions that operate on single tree-ring series.
}
\keyword{internal}

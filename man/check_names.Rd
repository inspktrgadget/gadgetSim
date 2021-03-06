% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{check_names}
\alias{check_names}
\title{Logical test to see if name(s) are in an object}
\usage{
check_names(x, obj)
}
\arguments{
\item{x}{Character. Regular expression of name}

\item{obj}{Named object. Can be a vector, list, data.frame, or any other
object that has a names attribute}
}
\value{
Logical. TRUE if names \code{x} exist in \code{obj}
}
\description{
This function tests to see if names \code{x} are found in an object and
returns TRUE if they are
}
\examples{
dat <- data.frame(a = 1, b = 2)
check_names("a", dat)
check_names("c", dat)
}

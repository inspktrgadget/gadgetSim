% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_stockfile_functions.R
\name{format_stockfile}
\alias{format_stockfile}
\title{Format an object of class \code{gadget_stock} to prepare for writing to file}
\usage{
format_stockfile(stockfile)
}
\arguments{
\item{stockfile}{}
}
\value{
A list with all components appropriately formatted for writing
}
\description{
This function is used internally by \code{\link{write_gadget_file}},
specifically the method for class \code{gadget_stock}. It formats the various
components of a \code{gadget_stock} stockfile so that it is ready to be
written to file
}
\examples{
# see created object from ?make_gadget_stockfile
format_stockfile(cod)
}

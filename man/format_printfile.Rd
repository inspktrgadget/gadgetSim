% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/printfile_functions.R
\name{format_printfile}
\alias{format_printfile}
\title{Format printfile component for writing to printfile}
\usage{
format_printfile(printfile_comp)
}
\arguments{
\item{printfile_comp}{A list of class pertaining to a Gadget printfile component,
see \code{\link{update_printfile}}}
}
\value{
Character vector of the printfile component ready to be printed to file complete
with tabs and newlines
}
\description{
Converts a list of class pertaining to \code{\link{printfile_defaults}} into a character
vector ready to be written to file
}
\examples{
cod <- list(stockname = "cod", printfile = "printfile")
cod_std <- update_printfile(stock_std, cod)
cod_std_pf <- format_printfile(cod_std)
}

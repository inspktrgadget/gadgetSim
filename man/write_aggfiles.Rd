% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write_gadget_files.R
\name{write_aggfiles}
\alias{write_aggfiles}
\alias{write_likelihood_aggfiles}
\alias{write_likelihood_datafiles}
\alias{write_printfile_aggfiles}
\title{Write aggfiles specific to certain Gadget components}
\usage{
write_likelihood_aggfiles(likelihood_file, path = NULL)

write_likelihood_datafiles(likelihood_file, path = NULL)

write_printfile_aggfiles(comp, path = NULL, aggfile_dir, fit_dir = NULL)
}
\arguments{
\item{path}{Character. Path of the Gadget model}

\item{aggfile_dir}{Character. Path of the directory where aggfiles will be printed}
}
\value{
NULL. Writes aggregate files to the appropriate files in
\code{aggfile_dir}
}
\description{
These functions are specific to Gadget likelihood and printfile components as
many of these aggfiles require particular attention to formatting. These
functions are strictly used within \code{\link{write_gadget_file}}
}
\examples{
path <- system.file(gad_mod_dir, package = "gadgetSim")
cod <- list(stocknames = "cod", printfile = "printfiles")
cod_stock <- update_printfile(stock, cod)
cod_stock_agg <- make_aggfiles(cod_stock, "print.aggfiles", path = path)
\dontrun{
write_aggfiles(cod_stock_agg, "print.aggfiles", path = path)
}
}

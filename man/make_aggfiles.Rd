% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/printfile_functions.R
\name{make_aggfiles}
\alias{make_aggfiles}
\alias{make_aggfiles.default}
\alias{make_aggfiles.stock_std}
\alias{make_aggfiles.stock_full}
\alias{make_aggfiles.stock_prey}
\alias{make_aggfiles.likelihood}
\alias{make_aggfiles.likelihood_summary}
\alias{make_aggfiles.stock}
\alias{make_aggfile.predator}
\alias{make_aggfile.predator_over}
\alias{make_aggfile.prey_over}
\alias{make_aggfiles.stock_prey}
\alias{make_aggfiles.predator_prey}
\title{Make aggregation files for Gadget printfile components that require them}
\usage{
make_aggfiles(printfile_comp, aggfile_dir, path = NULL)

\method{make_aggfiles}{default}(printfile_comp, aggfile_dir, path = NULL)

\method{make_aggfiles}{stock_std}(printfile_comp, aggfile_dir, path = NULL)

\method{make_aggfiles}{stock_full}(printfile_comp, aggfile_dir, path = NULL)

\method{make_aggfiles}{stock_prey}(printfile_comp, aggfile_dir, print = NULL)

\method{make_aggfiles}{likelihood}(printfile_comp, aggfile_dir, path = NULL)

\method{make_aggfiles}{likelihood_summary}(printfile_comp, aggfile_dir,
  path = NULL)

\method{make_aggfiles}{stock}(printfile_comp, aggfile_dir, path = NULL)

make_aggfile.predator(printfile_comp, aggfile_dir, path = NULL)

make_aggfile.predator_over(printfile_comp, aggfile_dir, path = NULL)

make_aggfile.prey_over(printfile_comp, aggfile_dir, print = NULL)

\method{make_aggfiles}{stock_prey}(printfile_comp, aggfile_dir, print = NULL)

\method{make_aggfiles}{predator_prey}(printfile_comp, aggfile_dir,
  print = NULL)
}
\arguments{
\item{printfile_comp}{A list of class pertaining to a Gadget printfile component,
see \code{\link{update_printfile}}}

\item{aggfile_dir}{Character. Path of the directory where aggfiles will be printed}

\item{path}{Character. Path of the Gadget model}
}
\value{
A list the same class as \code{printfile_comp}, but updated with the
appropriate aggregate files as attributes
}
\description{
Make aggregation files for Gadget printfile components that require them
}
\examples{
cod <- list(stocknames = "cod")
cod <- update_printfile(stock, cod)
cod <- make_aggfiles(cod, aggfile_dir = "print.aggfiles", path = gad_mod_dir)
}

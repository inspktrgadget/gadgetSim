% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_gadget_files.R
\name{read_time_area}
\alias{read_time_area}
\alias{read_gadget_timefile}
\alias{read_gadget_areafile}
\title{Read Gadget timefile}
\usage{
read_gadget_timefile(timefile, main = NULL, path = NULL)

read_gadget_areafile(areafile, main = NULL, path = NULL)
}
\arguments{
\item{timefile}{Character. The name of the file to read in}

\item{main}{Optional. A list of class \code{gadget_main}}

\item{path}{Optional. Character vector of path to the directory where Gadget files are located}
}
\value{
A list of class \code{gadget_time}
}
\description{
This function reads in a Gadget timefile using either the specified timefile
name or from the main file
}
\examples{
path <- system.file(gad_mod_dir, package = "gadgetSim")
main <- read_gadget_main(path = path)
timefile <- read_gadget_timefile(main = main, path = path)
area <- read_gadget_areafile(main = main, path = path)
}

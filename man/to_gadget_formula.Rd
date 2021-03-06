% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert_formulas.R
\name{to_gadget_formula}
\alias{to_gadget_formula}
\title{Turn R expression into Gadget formula string}
\usage{
to_gadget_formula(ex, stocknames = NULL)
}
\arguments{
\item{ex}{An unevaluated R expression (i.e. enclosed in quotes)}

\item{stocknames}{Optional. Character vector of stocknames to add to any formula variable names}
}
\value{
A character vector that is readable as a Gadget formula
}
\description{
This function is stolen directly from Rgadget::to.gadget.formulae. It takes an unevaluated R
expression (e.g. quote(2 + log(moo - 1))) and converts it into a character string that is readable
by Gadget
}
\details{
Gadget uses reverse Polish notation to read formulas (i.e. the operator comes first,
followed by the items to be operated on; 2 + 2 is read as (+ 2 2)). This function will take
an expression recognizable by R and convert it to one that is recognizable by Gadget
}
\examples{
to_gadget_formula(quote(2 + 2))
to_gadget_formula(quote(2 + log(moo - 1)))
}

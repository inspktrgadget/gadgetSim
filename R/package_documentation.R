#' gadgetSim: A package for creating simulated datasets from Gadget output
#'
#' gadgetSim takes output from a Gadget model and formats it so that the output can be used as
#' an operating model for simulation purposes (e.g. using Gadget as an estimation model). The core of
#' the package uses a handful of functions which can be found at \code{\link{gadget_simulate}}. Mainly,
#' the package takes output from the Gadget StockStdPrinter printfile component, creates a simulated
#' population based on the mean length and standard deviation, and then samples this simulated
#' population based on various selectivity and age-length sampling specifications. The sampled output
#' can then be used as data in an estimation model.
#'
#' @section Gadget Simulation functions:
#' These functions use the output from \code{\link{get_stock_std}} and are meant to be used in
#' conjunction with each other. \code{\link{add_lengthgroups}} takes the output from
#' \code{\link{get_stock_std}} and distributes the number at each year, step, area, and age combination
#' around its mean length using the standard deviation computed by Gadget. The \code{data.frame} value
#' returned by this function is not very useful by itself, but instead is meant to be fed to
#' \code{\link{survey_gadget}}, which samples the data using a selectivity curve and adding
#' multiplicative error to the numbers. This produces a simulated sample of Gadget output. This
#' sample can be further sub-sampled for length and age data using \code{\link{strip_age_length_data}}
#' according to the given length and age proportions. Note that the length and age proportions are
#' hierarchical in this function, which is intentional as an effort to produce more realistic sampling
#' of composition data. The age proportion value is itself a subset of the length proportion value.
#' For example, if 0.2 of the data are sampled for length, and 0.2 of those data are sampled for age,
#' then the proportion of all the data sampled for age is 0.2 * 0.2 = 0.04.
#'
#' @section Retrieving StockStdPrinter output:
#' The main function to retrieve StockStdPrinter output is \code{\link{get_stock_std}}. This produces
#' a printfile given the arguments and calls Gadget using this printfile to produce standard stock
#' output. This output is what is then used to re-create a simulated population using the functions
#' above. gadgetSim is designed to work with and around the
#' \href{https://github.com/hafro/rgadget}{Rgadget} package as much as possible; therefore, this
#' function also checks to see if a user has this package installed, and, if so, and
#' \code{gadget.fit} has been called will take the StockStdPrinter output created by Rgadget
#'
#' @section Functions to call Gadget:
#' There is a single function used to call Gadget on the command line, which is
#' \code{\link{call_gadget}}. This function is very similar to \code{callGadget} in the Rgadget
#' package. The key difference is that an optional path can be specified to \code{call_gadget},
#' which allows the function to be called on a Gadget model from any working directory.
#'
#' @section Read Gadget files:
#' There are several various functions to read different Gadget files. These are mostly used
#' internally, but are exported for convenience to the user if their use is ever desired. These
#' functions also contain an optional path argument to call the functions from any working directory
#' see ?read_gadget_*, where * is the Gadget filetype desired to read (i.e. main). These
#' fucntions also attempt to replicate Rgadget output as much as possible. The structure and
#' classes from this output are similar to that of the synonymous functions in Rgadget.
#'
#'
#' @docType package
#' @name gadgetSim
NULL

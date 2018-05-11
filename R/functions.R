# functions that are needed for this package
# x - write out stock.std - could just use Rgadget
# x - read in stock.std
# x - compute length groups
# x - simulate indices
#   -pre-baked selectivity functions
# functions to write out data files
#   -ages, lengths, indices
#   -should be able to make these within subdirs of the main model file
# functions to strip out age/length data
# function to bootstrap above procedure x number of times
# it would be ideal to have a package that:
#   1. is fed parameters
#   2. spits out gadget model
#   3. performs above procedure
# but for now let's just start with the above

##-------------------------------------------------------
# to do
# add the common selectivity functions to selectivity_functions.R
# set up tests for all current functions
# begin running simulations




#' Call Gadget executable on the command line
#'
#' This function will call the gadget command with the given switches as arguments
#'
#' @param switches List of named switches to supply to Gadget executable.
#' Names correspond to the switch; values correspond to additional arguments.
#' If no argument is needed the value should be logical \code{TRUE}.
#' @param path Optional. Character vector of path to the directory where Gadget files are located
#' @param gadget_exe The Gadget executable to use
#' @param print_out Logical. Should Gadget command line output be printed.
#' @param print_err Logical. Should Gadget command line errors be printed.
#'
#' @return If \code{print_out = TRUE} or \code{print_err = TRUE}, a character vector giving the output
#' of the command, one line per string, else nothing
#' @export
#'
#' @examples
#' path <- system.file(gad_mod_dir, package = "gadgetSim")
#' call_gadget(switches = list(s = TRUE, i = "params.in", log = "logfile"), path = path)
#' call_gadget(switches = list(s = TRUE, i = "WGTS/params.final", main = "WGTS/main.final"),
#'             path = path)
call_gadget <- function(switches = list(s = TRUE, i = "params.in"), path = NULL,
                        gadget_exe = "gadget", print_out = TRUE, print_err = TRUE) {
    switch_names <- paste0("-", names(switches))
    long_form_switch <- grep("version|help", switch_names)
    if (length(long_form_switch > 0)) {
        switch_names[long_form_switch] <- paste0("-", switch_names[long_form_switch])
    }
    switches[switches == TRUE] <- ""
    cmd_line_args <- paste(switch_names, switches)
    if (!is.null(path)) {
        Sys.setenv(GADGET_WORKING_DIR = normalizePath(gad_mod_dir))
        on.exit(Sys.setenv(GADGET_WORKING_DIR = ""))
    }
    system2(gadget_exe, args = cmd_line_args, stdout = print_out, stderr = print_err)
}



#' Make and retrieve output from the StockStdPrinter printfile component of a Gadget model
#'
#' @inheritParams read_gadget_main
#' @inheritParams call_gadget
#' @inheritParams read_gadget_stockfiles
#' @param params_file Character. Path to the params file used to call Gadget
#' @param ... Additional arguments to include
#'
#' @return List of \code{data.frame}s, one for each stock, of output from StockStdPrinter
#' printfile component
#'
#' @export
#'
#' @examples
#' \dontrun{
#' path <- system.file(gad_mod_dir, package = "gadgetSim")
#' stocks_data <- get_stock_std(path = path)
#' }
get_stock_std <- function(main = "main", params_file = "params.in", path = NULL,
                          fit_dir = "FIT", gadget_exe = "gadget", ...) {
    if (requireNamespace("Rgadget", quietly = TRUE)) {
        if (dir.exists(check_path(fit_dir))) {
            if ("WGTS.Rdata" %in% dir(check_path(fit_dir))) {
                load(paste(check_path(fit_dir), "WGTS.Rdata", sep = "/"))
                if (exists(out)) {
                    got_stock_std <- TRUE
                    return(out$stock.std)
                } else {
                    got_stock_std <- FALSE
                }
            } else {
                got_stock_std <- FALSE
            }
        } else {
            got_stock_std <- FALSE
        }
    } else {
        got_stock_std <- FALSE
    }
    if (!got_stock_std) {
        main <- read_gadget_main(main, path = path)
        if (!dir.exists(check_path(fit_dir))) {
            dir.create(check_path(fit_dir))
        }
        stocks <- get_stocknames(read_gadget_stockfiles(main = main, path = path))
        dots <- lapply(stocks, function(x) {
            return(list(stockname = x))
        })
        names(dots) <- rep("stock_std", length(dots))
        output_dir <- "out.fit"
        printfile <- "printfile.fit"
        aggfile_dir <- "print.aggfiles"
        if (!is.null(check_path(fit_dir))) {
            printfile <- paste(fit_dir, printfile, sep = "/")
            output_dir <- paste(fit_dir, output_dir, sep = "/")
            aggfile_dir <- paste(fit_dir, aggfile_dir, sep = "/")
            main_print <-
                modifyList(main, list(printfiles = printfile))
            write_gadget_main(main_print,
                              file = paste(fit_dir, "main.print", sep = "/"),
                              path = path)
            switches <-
                list(s = TRUE, i = params_file,
                     main = paste(fit_dir, "main.print", sep = "/"))
        } else {
            main_print <-
                modifyList(main, list(printfile = printfile))
            write_gadget_main(main_print,
                              file = "main.print",
                              path = path)
            switches <-
                list(s = TRUE, i = params_file,
                     main = "main.print")
        }
        make_gadget_printfile(dots, printfile = printfile, path = path,
                              output_dir = output_dir, aggfile_dir = aggfile_dir)
        call_gadget(switches = switches, path = path, gadget_exe = gadget_exe,
                    print_out = FALSE, print_err = FALSE)
        stock_std <-
            read_gadget_stock_std(output_dir = output_dir, path = path)
        return(stock_std)
    }
}



#' Create length distributions and sample Gadget output
#'
#' These functions create length distributions from Gadget StockStdPrinter output and sample
#' that output to simulate surveys. Error can be added to simulated surveys
#'
#'
#' @param stock_data A \code{data.frame} of stock data retrieved via \code{\link{read_gadget_stock_std}}
#' @param length_groups Numeric vector of length groups to distribute by. Probably should be the
#' same as dl in Gadget model
#' @param keep_zero_counts Logical. Keep year/step/area/age combinations with no individuals
#'
#' @details Length-structured population information from Gadget is output as mean length and standard
#' deviation for each year, step, area, and age combination. \code{add_lengthgroups} takes
#' output from \code{\link{get_stock_std}} and distributes the number for each respective
#' combination into numbers at each length specified by \code{length_groups}.
#' The return value for \code{add_lengthgroups} is a wide \code{data.frame} that can then be
#' fed into \code{survey_select}, which simulates surveys given the selectivity provided in
#' \code{survey_suitability} and error given by \code{survey_sigma}
#'
#'
#' @return \code{add_lengthgroup} returns a wide \code{data.frame} similar to that of
#' \code{stock_data}, but with values distributed across each length group which are represented
#' as each column. The output of \code{add_lengthgroup} is meant to go directly to
#' \code{survey_gadget} which returns a \code{data.frame} similar to \code{stock_data}, but
#' disaggregated by length.
#'
#' @export
#'
#' @name gadget_simulate
#'
#' @examples
#' path <- system.file(gad_mod_dir, package = "gadgetSim")
#' cod_stock_std <- get_stock_std(main = "WGTS/main.final", params_file = "WGTS/params.final",
#'                                path = path, fit_dir = "WGTS")
#' lengrps <- seq(0.5, 50.5, by = 1)
#' cod_lendist <- add_lengthgroups(cod_stock_std$cod0, lengrps)
add_lengthgroups <- function(stock_data, length_groups, keep_zero_counts = FALSE) {
    if (length(length_groups) < 2) {
        stop("Length group should have at least 2 members")
    }
    if (!keep_zero_counts) {
        stock_data <- stock_data[stock_data$number > 0, ]
    }
    lengrp_lower <- length_groups[-length(length_groups)]
    lengrp_upper <- length_groups[-1]
    len_dist <- function(len) {
        pnorm(rep(len, each = nrow(stock_data)), stock_data$length, stock_data$length.sd)
    }
    stock_number <- rep(stock_data$number, times = length(lengrp_upper))
    stock_len_numbers <- stock_number * (len_dist(lengrp_upper) - len_dist(lengrp_lower))
    lengrp_names <- list(c(), paste("length", lengrp_lower, lengrp_upper, sep = "_"))
    lengrp_matrix <-
        as.data.frame(matrix(stock_len_numbers,
                             dimnames = lengrp_names,
                             ncol = length(lengrp_lower)))
    return(cbind(stock_data, lengrp_matrix))
}


#' @rdname gadget_simulate
#' @param survey_suitability Numeric vector the same length as \code{length_groups} representing the
#' selection probability for each length in \code{length_groups}
#' @param survey_sigma Numeric value of multiplicative error to place on samples
survey_gadget <- function(stock_data, length_groups, survey_suitability, survey_sigma) {
    lengrp_lower <- length_groups[-length(length_groups)]
    lengrp_upper <- length_groups[-1]
    base_names <- grep("^length|^weight|^number",
                       names(stock_data), value = TRUE, invert = TRUE)
    base_data <- stock_data[, base_names, drop = FALSE]
    do.call("rbind", lapply(seq_len(length(lengrp_lower)), function(i) {
        length_col <-
            paste("length", lengrp_lower[[i]], lengrp_upper[[i]], sep = "_")
        out <- base_data
        out$length <- mean(c(lengrp_upper[[i]], lengrp_lower[[i]]))
        out$weight <- stock_data$weight
        mult_error <- exp(rnorm(nrow(base_data), 0, survey_sigma) - survey_sigma/2)
        out$number <- round(stock_data[, length_col] * mult_error * survey_suitability[[i]])
        return(out)
    }))
}

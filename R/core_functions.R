## some core functions for this package

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
    # check to make sure all files listed in switches are present in the Gadget model directory
    switches <- Filter(Negate(is.null), switches)
    switch_test <- unlist(Filter(is.character, switches))
    check_files_exist(switch_test, path = path)
    switch_names <- paste0("-", names(switches))
    long_form_switch <- grep("version|help", switch_names)
    if (length(long_form_switch > 0)) {
        switch_names[long_form_switch] <- paste0("-", switch_names[long_form_switch])
    }
    switches[switches == TRUE] <- ""
    cmd_line_args <- paste(switch_names, switches)
    if (!is.null(path)) {
        Sys.setenv(GADGET_WORKING_DIR = normalizePath(path))
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
get_stock_std <- function(main = "main", params_file = NULL, path = NULL,
                          fit_dir = "FIT", gadget_exe = "gadget", ...) {
    if (requireNamespace("Rgadget", quietly = TRUE)) {
        if (dir.exists(check_path(fit_dir))) {
            if ("WGTS.Rdata" %in% dir(check_path(fit_dir))) {
                load(paste(check_path(fit_dir), "WGTS.Rdata", sep = "/"))
                if (exists("out")) {
                    got_stock_std <- TRUE
                    stocks <- unique(out$stock.std$stock)
                    stock_std <-
                        lapply(stocks, function(x) {
                            tmp <- subset(out$stock.std, stock == x)
                            tmp <- subset(tmp, select = year:biomass.consumed)
                            names(tmp) <-
                                c("year", "step", "area", "age", "number", "length",
                                  "weight", "length.sd", "number.consumed", "biomass.consumed")
                            return(tmp)
                        })
                    stock_std <- setNames(stock_std, stocks)
                    return(stock_std)
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
        if (!("gadget_main" %in% class(main))) {
            main <- read_gadget_main(main, path = path)
        }
        check_dir_exists(check_path(fit_dir))
        stocks <- get_stocknames(read_gadget_stockfiles(main = main, path = path))
        dots <- lapply(stocks, function(x) {
            return(list(stockname = x))
        })
        names(dots) <- rep("stock_std", length(dots))
        output_dir <- "out.fit"
        printfile_name <- "printfile.fit"
        aggfile_dir <- "print.aggfiles"
        if (!is.null(check_path(fit_dir))) {
            printfile_name <- paste(fit_dir, printfile_name, sep = "/")
            output_dir <- paste(fit_dir, output_dir, sep = "/")
            aggfile_dir <- paste(fit_dir, aggfile_dir, sep = "/")
            main_print <-
                modifyList(main, list(printfiles = printfile_name))
            write_gadget_file(main_print,
                              file = paste(fit_dir, "main.print", sep = "/"),
                              path = path)
            switches <-
                list(s = TRUE, i = params_file,
                     main = paste(fit_dir, "main.print", sep = "/"))
        } else {
            main_print <-
                modifyList(main, list(printfile = printfile_name))
            write_gadget_file(main_print,
                              file = "main.print",
                              path = path)
            switches <-
                list(s = TRUE, i = params_file,
                     main = "main.print")
        }
        printfile <-
            make_gadget_printfile(dots, main = main, path = path,
                                  output_dir = output_dir, aggfile_dir = aggfile_dir)
        write_gadget_file(printfile, file = printfile_name, path = path,
                          output_dir = output_dir, aggfile_dir = aggfile_dir)
        call_gadget(switches = switches, path = path, gadget_exe = gadget_exe,
                    print_out = FALSE, print_err = FALSE)
        stock_std <-
            read_gadget_stock_std(output_dir = output_dir, path = path)
        return(stock_std)
    }
}


#' Create and get output from Gadget model in one fell swoop
#'
#' This function is really just a wrapper function for \code{\link{make_gadget_model}}
#' and \code{\link{get_stock_std}}. It essentially just combines the functionality
#' of these two functions so that you can skip a line or two of writing code.
#'
#' @inheritParams make_gadget_model
#'
#' @return List of \code{data.frame}s, one for each stock, of output from
#' StockStdPrinter printfile component
#' @export
#'
#' @examples
#' # see ?make_gadget_model. The process is the same for that function, except
#' # here you get output
#' stock_stds <- simualate_gadget(time = time,
#'                                area = area,
#'                                stock = cod,
#'                                fleet = fleets,
#'                                path = gad_mod_dir)
simulate_gadget <- function(..., path = NULL) {
    make_gadget_model(..., path = path)
    return(get_stock_std(path = path))
}

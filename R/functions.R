# functions that are needed for this package
# x - write out stock.std - could just use Rgadget
# read in stock.std
# compute length groups
# simulate indices
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

#' Call Gadget executable on the command line
#'
#' This function will call the gadget command with the given switches as arguments
#'
#' @param switches List of named switches to supply to Gadget executable.
#' Names correspond to the switch; values correspond to additional arguments.
#' If no argument is needed the value should be logical \code{TRUE}.
#' @param path Optional. Character vector of path to the directory where Gadget files are located
#' @param fit_dir Directory where final optimized values are supplied. (e.g. "WGTS")
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
#' call_gadget(switches = list(s = TRUE, i = "params.final", main = "main.final"), path = path,
#'             fit_dir = "WGTS")
call_gadget <- function(switches = list(s = TRUE, i = "params.in"), path = NULL,
                        fit_dir = NULL, gadget_exe = "gadget",
                        print_out = TRUE, print_err = TRUE) {
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
                          fit_dir = NULL, gadget_exe = "gadget", ...) {
    fit_dir <- check_path(fit_dir)
    if (requireNamespace("Rgadget", quietly = TRUE)) {
        if (dir.exists(fit_dir)) {
            if ("WGTS.Rdata" %in% dir(check_path(fit_dir))) {
                load(paste(fit_dir, "WGTS.Rdata", sep = "/"))
                return(out$stock.std)
            }
        }
    } else {
        main <- read_gadget_main(main, path = path)
        if (!dir.exists(fit_dir)) {
            dir.create(fit_dir)
        }
        stocks <- get_stocknames(read_gadget_stockfiles(main = main, path = path))
        dots <- lapply(stocks, function(x) {
            return(stocknames = x)
        })
        names(dots) <- rep("stock_std", length(dots))
        file <- "printfile.fit"
        aggfile_dir <- "print.aggfiles"
        if (!is.null(fit_dir)) {
            file <- paste(fit_dir, file, sep = "/")
            output <- paste(fit_dir, output, sep = "/")
            aggfile_dir <- paste(fit_dir, aggfile_dir, sep = "/")
            main_print <-
                modifyList(main, list(printfile = file))
            write_gadget_main(main_print,
                              file = paste(fit_dir, "main.print", sep = "/"))
            switches <-
                list(s = TRUE, i = params.file,
                     main = paste(fit_dir, "main.print", sep = "/"))
        } else {
            main_print <-
                modifyList(main, list(printfile = file))
            write_gadget_main(main_print,
                              file = "main.print")
            switches <-
                list(s = TRUE, i = params.file,
                     main = "main.print")
        }
        make_gadget_printfile(dots, file = file, path = path,
                              output = output, aggfile_dir = aggfile_dir)
        call_gadget(switches = switches, path = path, fit_dir = fit_dir,
                    gadget_exe = gadget_exe, stdout = FALSE, stderr = FALSE)
        stock_std <-
            read_gadget_stock_std(output_dir = output, path = path)
    }
}

# functions that are needed for this package
# write out stock.std - could just use Rgadget
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
#' @param path A path to the directory where Gadget files are housed
#' @param switches List of named switches to supply to Gadget executable.
#' Names correspond to the switch; values correspond to additional arguments.
#' If no argument is needed the value should be logical \code{TRUE}.
#' @param fit.dir Directory where final optimized values are supplied. e.g. "WGTS"
#' @param gadget.exe The Gadget executable to use
#' @param print.out Logical. Should Gadget command line output be printed.
#' @param print.err Logical. Should Gadget command line errors be printed.
#'
#' @return If \code{print.out = TRUE} or \code{print.err = TRUE}, a character vector giving the output
#' of the command, one line per string, else nothing
#' @export
#'
#' @examples
#' call_gadget("gadget_model", switches = list(s = TRUE, i = "params.in", log = "logfile"))
#' call_gadget("gadget_model", switches = list(s = TRUE, i = "params.in", main = "main.final"),
#'             fit.dir = "WGTS")
call_gadget <- function(path, switches = list(s = TRUE, i = "params.in"),
                        fit.dir = NULL, gadget.exe = "gadget",
                        print.out = TRUE, print.err = TRUE) {
    wd <- getwd()
    on.exit(setwd(wd))
    setwd(path)
    switch_names <- paste0("-", names(switches))
    long_form_switch <- grep("version|help", switch_names)
    if (length(long_form_switch > 0)) {
        switch_names[long_form_switch] <- paste0("-", switch_names[long_form_switch])
    }
    args[args == TRUE] <- ""
    if (!is.null(fit.dir)) {
        if (any(grepl("main|^i$", names(switches)))) {
            imain_ind <- grep("main|^i$", names(switches))
            args[imain_ind] <- paste(fit.dir, args[imain_ind], sep = "/")
        }
    }
    cmd_line_args <- paste(switch_names, args)
    system2(gadget.exe, args = cmd_line_args, stdout = print.out, stderr = print.err)
}



get_stock_std <- function(path, switches = list(s = TRUE, i = "params.final"),
                          fit.dir = "FIT", gadget.exe = "gadget", ...) {
    wd <- getwd()
    on.exit(setwd(wd))
    setwd(path)
    if (requireNamespace("Rgadget", quietly = TRUE)) {
        if (fit.dir %in% dir()) {
            if ("WGTS.Rdata" %in% dir(fit.dir)) {
                load(paste(fit.dir, "WGTS.Rdata", sep = "/"))
                return(out$stock.std)
            }
        }
    } else {
        if ("main" %in% names(switches)) {
            main_file <- switches[[names(switches) == "main"]]
        } else {main_file <- read_gadget_main("main")}
        if (!dir.exists(fit.dir)) {
            dir.create(fit.dir)
            dir.create(paste(fit.dir, "out.fit", sep = "/"))
        }
        call_gadget(path = path, switches = switches, fit.dir = fit.dir,
                    gadget.exe = gadget.exe, stdout = FALSE, stderr = FALSE)
    }
}

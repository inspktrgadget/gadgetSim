# functions to make R versions of various gadget files
# to be used when exporting R objects to gadget-readable files


#' Create a list of class gadget.main
#'
#' @param ... Either a list with named objects pertaining to the main file for
#' Gadget or any number of objects named similarly
#'
#' @return A list of class \code{gadget.main}
#' @export
#'
#' @examples
#' main <- list(timefile = "time", areafile = "area", stockfiles = "cod")
#' make_gadget_mainfile(main)
make_gadget_mainfile <- function(...) {
    dots <- dots2list(...)
    out <- structure(modifyList(gadget_main_default, dots),
                     class = c("gadget.main", "list"))
    if (!isTRUE(all.equal(names(out), names(gadget_main_default)))) {
        stop("You have either removed an argument from mainfile or added one not recognized")
    } else if (is.null(out$stockfiles) | out$stockfiles == "") {
        warning("You have no stock in this mainfile")
    }
    return(out)
}


#' Make Gadget printfile and write out to file
#'
#' @param ... Any number of lists containing details of Gadget printfile components. Arguments to
#' \code{...} must be the same name as one of the default printfile lists,
#' see \code{\link{printfileDefaults}}
#'
#' @param main List of class \code{gadget.main} or character vector of the main printfile to be read
#' @param printfile Character. A path to where the printfile will be written
#' @param path Optional. Path to Gadget model
#' @param output_dir Character. Path to the directory where printfiles will be written
#' @param aggfile_dir Character. Path to the directory where aggfiles will be written
#' @param printatstart A numeric value of 0 or 1. 0 indicates printing at end of timesteps.
#' 1 indicates printing at the start
#' @param steps The steps in which to print the various printfile components.
#' Can be numeric or \code{"all"}
#'
#' @return NULL. Writes the printfiles provided in \code{...} to the directory
#' provided by \code{output_dir}
#' @export
#'
#' @examples
#' make_gadget_printfile(stock_std = list(stockname = "cod"), printfile = "printfile", path = gad_mod_dir)
make_gadget_printfile <- function(..., main = "main", printfile, path = NULL, fit_dir = NULL,
                                  output_dir = "out", aggfile_dir = "print.aggfiles",
                                  printatstart = 1, steps = "all") {
    if (!is.null(path)) {
        if ("gadget.main" %in% class(main)) {
            main <- main
        } else {
            main <- read_gadget_main(file = main, path = path)
        }
        dir_name <- basename(path)
    } else {
        main <- read_gadget_main(main)
        dir_name <- basename(getwd())
    }
    stockfiles <- main$stockfiles
    fleetfiles <- main$fleetfiles
    lik_files <- main$likelihood
    header <- sprintf("; printfile for %s - created by gadgetSim %s on %s",
                      dir_name, packageVersion("gadgetSim"), date())
    # check to see if dots is given as a list or a number of named objects
    dots <- list(...)
    dot_len <- length(dots)
    if (dot_len < 1) {
        stop("You must provide a named object in ...")
    } else if (dot_len == 1) {
        if (!is.null(names(dots))) {
            dots <- dots
        } else {
            dots <- as.list(...)
            if (!any(names(dots) %in% pf_types)) {
                stop(cat("... must have names",
                         paste0("* ", pf_types),
                         "or be a list of objects with names as such",
                         sep = "\n"))
            }
        }
    } else {
        dots <- dots
    }
    # plug the information given in ... into the appropriate printfile component template
    updated_printfiles <-
        lapply(seq_along(dots), function(x) {
                pf_template <- getFromNamespace(names(dots)[x], ns = "gadgetSim")
                new_pf <- update_printfile(pf_template, dots[[x]])
                if (is.null(new_pf$printfile)) {
                    type_label <- gsub("_", "\\.", class(new_pf)[1])
                    stocks2get <- unlist(new_pf[grep("name", names(new_pf))])
                    if (!is.null(output_dir)) {
                        new_pf$printfile <-
                            sprintf("%s/%s.%s",
                                    output_dir,
                                    paste(stocks2get, collapse = "."),
                                    type_label)
                    } else {
                        new_pf$printfile <-
                            sprintf("%s.%s", paste(stocks2get, collapse = "."),
                                    type_label)
                    }
                } else if (!is.null(output_dir)) {
                    new_pf$printfile <- sprintf("%s/%s", output_dir, new_pf$printfile)
                }
                return(new_pf)
            })
    # check directory structure of fit_dir
    if (!is.null(fit_dir)) {
        printfile <- paste(c(fit_dir, printfile), collapse = "/")
        output_dir <- paste(c(fit_dir, output_dir), collapse = "/")
        aggfile_dir <- paste(c(fit_dir, aggfile_dir), collapse = "/")
    }
    # make and add aggregate files for those components that need them
    added_aggfiles <- lapply(updated_printfiles, make_aggfiles,
                             aggfile_dir = aggfile_dir, path = path)
    # check check for aggfiles, create if needed, and write out aggfiles
    null_list <- lapply(added_aggfiles, write_aggfiles,
                        aggfile_dir = aggfile_dir, path = path)
    formatted_printfiles <- lapply(added_aggfiles, format_printfile)
    likfile2print <- paste(c(header, formatted_printfiles), collapse = "\n;\n")
    # create directories for file writing
    if (!dir.exists(check_path(output_dir))) {
        dir.create(check_path(output_dir))
    }
    write(likfile2print, file = check_path(printfile))
}



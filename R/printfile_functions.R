# defaults and methods for updating printfiles

#' Access elements of default printfiles and update them
#'
#' @param printfile_comp A default printfile list, see \link{printfileDefaults}
#' @param new_info A list of named vectors. The names correspond to the arguments to
#' Gadget printfile components, and the vector is the desired value
#'
#' @return A list of class corresponding to the printfile component (\code{update_printfile}
#' for the StockStdPrinter returns a list of class \code{stock_std}).
#'
#' @details \code{update_printfile} is a generic method for the various printfile types.
#' Each printfile class has its own \code{update_printfile} method that returns a list of
#' that class. This family of functions was created to allow for the implementation of any
#' printfiles in functions like \code{\link{make_gadget_printfile}}. Lists used for
#' \code{printfile} must be one of the following classes: \code{stock_std, stock_full,
#' stock, predator, predator_over, prey_over, stock_prey_full, stock_prey, predator_prey,
#' likelihood, likelihood_summary}.
#'
#' @name update_printfile
#'
#' @examples
#' cod_stock_std <- list(stockname = "cod", printfile = "cod.stock.std")
#' update_printfile(stock_std, cod_stock_std)
update_printfile <- function(printfile_comp, new_info) {
    if (!(all(names(new_info) %in% names(printfile_comp)))) {
        stop("Your information to update the printfile component contains some
             incorrect names.\n Please check names and re-try.")
    } else {
        out <- modifyList(printfile_comp, new_info)
    }
    return(out)
}

# you can most likely delete the following methods functions for update_printfile

# update_printfile.stock_std <- function(printfile, new_info) {
#     new_printfile <- update_printfile.default(printfile, new_info)
#     printfile_name_check(new_printfile, stock_std_args)
#     return(new_printfile)
# }
#
# update_printfile.stock_full <- function(printfile, new_info) {
#     new_printfile <- update_printfile.default(printfile, new_info)
#     printfile_name_check(new_printfile, stock_full_args)
#     return(new_printfile)
# }
#
# update_printfile.stock <- function(printfile, new_info) {
#     new_printfile <- update_printfile.default(printfile, new_info)
#     printfile_name_check(new_printfile, stock_args)
#     return(new_printfile)
# }
#
# update_printfile.predator <- function(printfile, new_info) {
#     new_printfile <- update_printfile.default(printfile, new_info)
#     printfile_name_check(new_printfile, predator_args)
#     return(new_printfile)
# }
#
# update_printfile.predator_over <- function(printfile, new_info) {
#     new_printfile <- update_printfile.default(printfile, new_info)
#     printfile_name_check(new_printfile, predator_over_args)
#     return(new_printfile)
# }
#
# update_printfile.prey_over <- function(printfile, new_info) {
#     new_printfile <- update_printfile.default(printfile, new_info)
#     printfile_name_check(new_printfile, prey_over_args)
#     return(new_printfile)
# }
#
# update_printfile.stock_prey_full <- function(printfile, new_info) {
#     new_printfile <- update_printfile.default(printfile, new_info)
#     printfile_name_check(new_printfile, stock_prey_full_args)
#     return(new_printfile)
# }
#
# update_printfile.stock_prey <- function(printfile, new_info) {
#     new_printfile <- update_printfile.default(printfile, new_info)
#     printfile_name_check(new_printfile, stock_prey_args)
#     return(new_printfile)
# }
#
# update_printfile.stock_std <- function(printfile, new_info) {
#     new_printfile <- update_printfile.default(printfile, new_info)
#     printfile_name_check(new_printfile, stock_std_args)
#     return(new_printfile)
# }
#
# update_printfile.predator_prey <- function(printfile, new_info) {
#     new_printfile <- update_printfile.default(printfile, new_info)
#     printfile_name_check(new_printfile, predator_prey_args)
#     return(new_printfile)
# }
#
# update_printfile.likelihood <- function(printfile, new_info) {
#     new_printfile <- update_printfile.default(printfile, new_info)
#     printfile_name_check(new_printfile, likelihood_args)
#     return(new_printfile)
# }
#
# update_printfile.likelihood_summary <- function(printfile, new_info) {
#     new_printfile <- update_printfile.default(printfile, new_info)
#     printfile_name_check(new_printfile, likelihood_summary_args)
#     return(new_printfile)
# }


#' Make aggregation files for Gadget printfile components that require them
#'
#' @param printfile_comp A list of class pertaining to a Gadget printfile component,
#' see \code{\link{update_printfile}}
#' @param aggfile_dir Character. Path of the directory where aggfiles will be printed
#' @param path Character. Path of the Gadget model
#'
#' @return A list the same class as \code{printfile_comp}, but updated with the
#' appropriate aggregate files as attributes
#'
#' @name make_aggfiles
#'
#' @examples
#' cod <- list(stocknames = "cod")
#' cod <- update_printfile(stock, cod)
#' cod <- make_aggfiles(cod, aggfile_dir = "print.aggfiles", path = gad_mod_dir)
make_aggfiles <- function(printfile_comp, aggfile_dir, path = NULL) {
    UseMethod("make_aggfiles")
}

make_aggfiles.default <- function(printfile_comp, aggfile_dir, path = NULL) {
    return(printfile_comp)
}

# printfile components that do not use aggregation files should just return the printfile_comp
make_aggfiles.stock_std <- make_aggfiles.default
make_aggfiles.stock_full <- make_aggfiles.default
make_aggfiles.stock_prey <- make_aggfiles.default
make_aggfiles.likelihood <- make_aggfiles.default
make_aggfiles.likelihood_summary <- make_aggfiles.default

make_aggfiles.stock <- function(printfile_comp, aggfile_dir, path = NULL) {
    if (!any(isNULL_aggfiles(printfile_comp))) {
        invisible()
    } else {
        stocknames <- printfile_comp$stockname
        area_agg <- printfile_comp$areaaggfile
        age_agg <- printfile_comp$ageaggfile
        len_agg <- printfile_comp$lenaggfile
        if (is.null(stocknames)) {
            stop("You are missing the stock for %sPrinter", get_pf_type(printfile_comp))
        }
        stocks <- read_gadget_stockfiles(stocknames, path = path)
        printfile_comp <-
            check_agg_type(printfile_comp, area_agg, stocks, aggfile_dir)
        printfile_comp <-
            check_agg_type(printfile_comp, age_agg, stocks, aggfile_dir)
        printfile_comp <-
            check_agg_type(printfile_comp, len_agg, stocks, aggfile_dir)
    }
    return(printfile_comp)
}

make_aggfile.predator <- function(printfile_comp, aggfile_dir, path = NULL) {
    if (!any(isNULL_aggfiles(printfile_comp))) {
        invisible()
    } else {
        pred_names <- printfile_comp$predatornames
        prey_names <- printfile_comp$preynames
        area_agg <- printfile_comp$areaaggfile
        pred_len_agg <- printfile_comp$predlenaggfile
        prey_len_agg <- printfile_comp$preylenaggfile
        if (is.null(pred_names)) {
            stop("You are missing the predatornames for %sPrinter",
                 get_pf_type(printfile_comp))
        } else if (is.null(prey_names)) {
            stop("You are missing the preynames for %sPrinter",
                 get_pf_type(printfile_comp))
        } else {
            pred <- read_gadget_stockfiles(pred_names, path = path)
            prey <- read_gadget_stockfiles(prey_names, path = path)
            printfile_comp <-
                check_agg_type(printfile_comp, area_agg, c(pred, prey), aggfile_dir)
            printfile_comp <-
                check_agg_type(printfile_comp, pred_len_agg, pred, aggfile_dir)
            printfile_comp <-
                check_agg_type(printfile_comp, prey_len_agg, prey, aggfile_dir)
        }
        return(printfile_comp)
    }
}

make_aggfile.predator_over <- function(printfile_comp, aggfile_dir, path = NULL) {
    if (!any(isNULL_aggfiles(printfile_comp))) {
        invisible()
    } else {
        pred_names <- printfile$predatornames
        area_agg <- printfile_comp$areaaggfile
        len_agg <- printfile_comp$lenaggfile
        if (is.null(pred_names)) {
            stop("You are missing the predatornames for %sPrinter",
                 get_pf_type(printfile_comp))
        } else {
            pred <- read_gadget_stockfiles(pred_names, path = path)
            printfile_comp <-
                check_agg_type(printfile_comp, area_agg, pred, aggfile_dir)
            printfile_comp <-
                check_agg_type(printfile_comp, len_agg, pred, aggfile_dir)
        }
        return(printfile_comp)
    }
}

make_aggfile.prey_over <- function(printfile_comp, aggfile_dir, print = NULL) {
    if (!any(isNULL_aggfiles(printfile_comp))) {
        invisible()
    } else {
        prey_names <- printfile_comp$preynames
        area_agg <- printfile_comp$areaaggfile
        len_agg <- printfile_comp$lenaggfile
        if (is.null(prey_names)) {
            stop("You are missing the preynames for %sPrinter",
                 get_pf_type(printfile_comp))
        } else {
            prey <- read_gadget_stockfiles(prey_names, path = path)
            printfile_comp <-
                check_agg_type(printfile_comp, area_agg, prey, aggfile_dir)
            printfile_comp <-
                check_agg_type(printfile_comp, len_agg, prey, aggfile_dir)
        }
        return(printfile_comp)
    }
}

make_aggfiles.stock_prey <- function(printfile_comp, aggfile_dir, print = NULL) {
    if (!any(isNULL_aggfiles(printfile_comp))) {
        invisible()
    } else {
        prey_names <- printfile_comp$preynames
        area_agg <- printfile_comp$areaaggfile
        age_agg <- printfile_comp$ageaggfile
        len_agg <- printfile_comp$lenaggfile
        if (is.null(prey_names)) {
            stop("You are missing the preynames for %sPrinter",
                 get_pf_type(printfile_comp))
        } else {
            prey <- read_gadget_stockfiles(prey_names, path = path)
            printfile_comp <-
                check_agg_type(printfile_comp, area_agg, prey, aggfile_dir)
            printfile_comp <-
                check_agg_type(printfile_comp, age_agg, prey, aggfile_dir)
            printfile_comp <-
                check_agg_type(printfile_comp, len_agg, prey, aggfile_dir)
        }
        return(printfile_comp)
    }
}

make_aggfiles.predator_prey <- function(printfile_comp, aggfile_dir, print = NULL) {
    if (!any(isNULL_aggfiles(printfile_comp))) {
        invisible()
    } else {
        pred_names <- printfile_comp$predatornames
        prey_names <- printfile_comp$preynames
        area_agg <- printfile_comp$areaaggfile
        age_agg <- printfile_comp$ageaggfile
        len_agg <- printfile_comp$predlenaggfile
        if (is.null(pred_names)) {
            stop("You are missing the predatornames for %sPrinter",
                 get_pf_type(printfile_comp))
        } else if (is.null(prey_names)) {
            stop("You are missing the preynames for %sPrinter",
                 get_pf_type(printfile_comp))
        } else {
            pred <- read_gadget_stockfiles(pred_names, path = path)
            prey <- read_gadget_stockfiles(prey_names, path = path)
            printfile_comp <-
                check_agg_type(printfile_comp, area_agg, c(pred, prey), aggfile_dir)
            printfile_comp <-
                check_agg_type(printfile_comp, age_agg, prey, aggfile_dir)
            printfile_comp <-
                check_agg_type(printfile_comp, len_agg, prey, aggfile_dir)
        }
        return(printfile_comp)
    }
}

#' Functions to make the area, age, length, predator, and prey aggfiles
#'
#' These are helper functions to the function \code{\link{make_aggfiles}}.
#' \code{make_<type>_agg_dir} functions return a path to the printfile,
#' \code{make_<type>_aggfile} produces the content for the aggfile itself,
#' and the \code{check_*} family of functions checks for the presence of an
#' aggfile in the \code{printfile_comp} of \code{\link{make_aggfiles}} and
#' returns the aggfile and path if not present.
#'
#' @inheritParams make_aggfiles
#' @param stocks A list of class \code{gadget.stock} or \code{gadget.stocks}
#'
#' @return \code{make_*_agg_dir} returns a character vector of a path to the
#' printfile location. \code{make_*_aggfile} returns a character vector of the
#' aggregate file itself ready to be written to file. The \code{check_*} functions
#' look to see if the appropriate aggregate file is present, and, if not, will
#' write the aggfile path to the aggfile line in \code{printfile_comp} of
#' \code{\link{make_aggfiles}} and the aggfile as an attribute to \code{printfile_comp}.
#'
#' @name makeAggfileHelpers
#'
#' @examples
#' main <- read_gadget_main(path = gad_mod_dir)
#' stocks <- read_gagdget_stockfiles(main = main, path = gad_mod_dir)
#' make_area_agg_dir(stocks, aggfile_dir = "print.aggfiles")
#' make_area_aggfile(stocks)
make_agg_dir <- function(stocks, aggfile_dir, agg_type) {
    stocknames <- get_stocknames(stocks)
    return(sprintf("%s/%s.%s.agg", aggfile_dir, paste(stocknames, collapse = "."), agg_type))
}

#' @rdname makeAggfileHelpers
make_area_agg_dir <- function(stocks, aggfile_dir) {
    make_agg_dir(stocks = stocks, aggfile_dir = aggfile_dir, agg_type = "area")
}

#' @rdname makeAggfileHelpers
make_age_agg_dir <- function(stocks, aggfile_dir) {
    make_agg_dir(stocks = stocks, aggfile_dir = aggfile_dir, agg_type = "age")
}

#' @rdname makeAggfileHelpers
make_len_agg_dir <- function(stocks, aggfile_dir) {
    make_agg_dir(stocks = stocks, aggfile_dir = aggfile_dir, agg_type = "len")
}

#' @rdname makeAggfileHelpers
make_predlen_agg_dir <- function(stocks, aggfile_dir) {
    make_agg_dir(stocks = stocks, aggfile_dir = aggfile_dir, agg_type = "predlen")
}

#' @rdname makeAggfileHelpers
make_preylen_agg_dir <- function(stocks, aggfile_dir) {
    make_agg_dir(stocks = stocks, aggfile_dir = aggfile_dir, agg_type = "preylen")
}

#' @rdname makeAggfileHelpers
make_area_aggfile <- function(stocks) {
    areas <- get_stock_areas(stocks)
    area_agg <-
        paste(sprintf("%s\t%s", paste0("area", areas), areas), collapse = "\n")
    sprintf("%s\n%s", aggfile_header("area"), area_agg)
}

#' @rdname makeAggfileHelpers
make_age_aggfile <- function(stocks) {
    ages <- get_stock_ages(stocks)
    age_agg <-
        paste(sprintf("%s\t%s", paste0("age", ages), ages), collapse = "\n")
    return(sprintf("%s\n%s", aggfile_header("age"), age_agg))
}

#' @rdname makeAggfileHelpers
make_len_aggfile <- function(stocks, len_type = "length") {
    len <- get_stock_lengths(stocks)
    len_agg <-
        paste(sprintf("%s\t%s\t%s", paste0("len", len[-1]),
                      len[-length(len)], len[-1]), collapse = "\n")
    return(sprintf("%s\n%s", aggfile_header(len_type), len_agg))
}

#' @rdname makeAggfileHelpers
make_predlen_aggfile <- function(stocks) {
    make_len_aggfile(stocks = stocks, len_type = "predator length")
}

#' @rdname makeAggfileHelpers
make_preylen_aggfile <- function(stocks) {
    make_len_aggfile(stocks = stocks, len_type = "prey length")
}

#' @rdname makeAggfileHelpers
check_agg_type <- function(printfile_comp, agg_type, stocks, aggfile_dir) {
    agg_name <- deparse(substitute(agg_type))
    fun_prefix <- paste("make", agg_name, sep = "_")
    dir_call <- paste(fun_prefix, "dir", sep = "_")
    aggfile_call <- paste0(fun_prefix, "file")
    if (is.null(agg_type)) {
        aggfile_name <- paste0(gsub("_", "", agg_name), "file")
        printfile_comp[[aggfile_name]] <-
            do.call(dir_call, list(stocks = stocks, aggfile_dir = aggfile_dir))
        attr(printfile_comp, aggfile_name) <- do.call(aggfile_call, list(stocks = stocks))
    }
    return(printfile_comp)
}

#' @rdname makeAggfileHelpers
isNULL_aggfiles <- function(printfile_comp) {
    vapply(subset(printfile_comp, grepl("aggfile", names(printfile_comp))),
           is.null, logical(1))
}

#' Check the names of a printfile component to ensure all mandatory arguments are present
#'
#'
#' @inheritParams make_aggfiles
#'
#' @return NULL. Only returns an error message if any non-optional arguments are missing
#'
#' @examples
#' printfile_name_check(stock_std)
#'
#' cod <- update_printfile(stock_std, list(stockname = "cod", printfile = "printfile"))
#' printfile_name_check(cod)
printfile_name_check <- function(printfile_comp) {
    pf_nm <- class(printfile_comp)[1]
    pf_args_nm <- paste(pf_nm, "args", sep = "_")
    printfile_args <- getFromNamespace(pf_args_nm, ns = "gadgetSim")
    pf_type <-
        paste0(simpleCap(unlist(strsplit(pf_args_nm,
                                          split = "_"))), collapse = "")
    names_in <- printfile_args %in% names(printfile_comp)
    non_opt_args <- vapply(printfile_comp[printfile_args], is.null, logical(1))
    if (!all(names_in)) {
        names_out <- printfile_args[!names_in]
        stop(sprintf("You provided the wrong name to %s for printer component %sPrinter",
                     names_out, pf_type))
    } else if (any(non_opt_args)) {
        missing_args <- names(printfile_comp[printfile_args])[non_opt_args]
        stop(sprintf("Missing the following non-optional arguments to %s %s",
                     sprintf("%sPrinter", pf_type),
                     paste(c("\n", missing_args), collapse = "\n* ")),
             call. = TRUE)
    }
}

#' Update path for printfile and aggfiles to include directories to write to
#'
#' Checks to make sure that the appropriate printfile and aggfile directories are entered in the
#' printfile_comp
#'
#' @inheritParams make_aggfiles
#' @param print_dir Character. Path of directory to print the printfiles
#'
#' @return A list the same as \code{printfile}, but with updated printfile and
#' aggfile(s) if present
#'
#' @examples
#' cod <- list(stockname = "cod", printfile = "cod.stock.std")
#' cod_std <- update_printfile(stock_std, cod)
#' cod_std <- update_printfile_dirs(stock_std, print_dir = "printfile")
update_printfile_dirs <- function(printfile_comp, print_dir = NULL, aggfile_dir = NULL) {
    pf <- "printfile"
    if (any(names(printfile_comp) == "printfile")) {
        if (!is.null(print_dir)) {
            printfile_comp$printfile <- paste(print_dir, printfile_comp$printfile, sep = "/")
        }
    }
    agg_index <- grep("aggfile", names(printfile_comp))
    if (length(agg_index) > 0) {
        if (!is.null(aggfile_dir)) {
            printfile_comp[[agg_index]] <-
                paste(aggfile_dir, printfile_comp[[agg_index]], sep = "/")
        }
    }
    return(printfile_comp)
}


#' Format printfile component for writing to printfile
#'
#' Converts a list of class pertaining to \code{\link{printfile_defaults}} into a character
#' vector ready to be written to file
#'
#' @inheritParams make_aggfiles
#'
#' @return Character vector of the printfile component ready to be printed to file complete
#' with tabs and newlines
#'
#' @examples
#' cod <- list(stockname = "cod", printfile = "printfile")
#' cod_std <- update_printfile(stock_std, cod)
#' cod_std_pf <- format_printfile(cod_std)
format_printfile <- function(printfile_comp) {
    printfile_name_check(printfile_comp)
    pf_elem <-
        lapply(seq_along(printfile_comp), function(x, nms) {
            if (is.null(printfile_comp[[x]])) {
                return(NULL)
            } else {paste0(c(nms[x], printfile_comp[[x]]), collapse = "\t")}
        }, nms = names(printfile_comp))
    return(paste(c(comp_lab, unlist(pf_elem)), sep = "\n", collapse = "\n"))
}

#' Write aggfiles used in a printfile_component to files in a directory
#'
#' This function searches for aggfiles and prints them to the appropriate directory and filename
#'
#' @inheritParams make_aggfiles
#'
#' @return NULL. Writes aggregate files to the appropriate files in \code{aggfile_dir}
#'
#' @examples
#' path <- system.file(gad_mod_dir, package = "gadgetSim")
#' cod <- list(stocknames = "cod", printfile = "printfiles")
#' cod_stock <- update_printfile(stock, cod)
#' cod_stock_agg <- make_aggfiles(cod_stock, "print.aggfiles", path = path)
#' \dontrun{
#' write_aggfiles(cod_stock_agg, "print.aggfiles", path = path)
#' }
write_aggfiles <- function(printfile_comp, aggfile_dir, path) {
    if (any(isNULL_aggfiles(printfile_comp))) {
        missing_agg <- names(printfile_comp)[isNULL_aggfiles(printfile_comp)]
        stop(sprintf("Required argument %s is missing", missing_agg))
    } else {
        if (!dir.exists(check_path(aggfile_dir))) {
            dir.create(check_path(aggfile_dir))
        } else if (dir.exists(check_path(aggfile_dir))) {
            unlink(paste(c(check_path(aggfile_dir), "*"), collapse = "/"))
        }
        agg_types <- grep("aggfile", names(printfile_comp), value = TRUE)
        null_list <-
            lapply(agg_types, function(x) {
                agg_test <- strsplit(printfile_comp[[x]], split = "/")[[1]][1]
                if (!(all.equal(agg_test, aggfile_dir))) {
                    stop(sprintf("The supplied aggfile_dir and directory prefix for
                                  %s are not the same"), printfile_comp[[x]])
                }
                if (!is.null(path)) {
                    write(attr(printfile_comp, x),
                          file = paste(path, printfile_comp[[x]], sep = "/"))
                } else {
                    write(attr(printfile_comp, x),
                          file = printfile_comp[[x]])
                }
            })
    }
}


pf_types <- c("stock_std", "stock_full", "stock",
              "predator", "predator_over", "prey_over",
              "stock_prey_full", "stock_prey", "predator_prey",
              "likelihood", "likelihood_summary")

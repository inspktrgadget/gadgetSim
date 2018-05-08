# defaults and methods for updating printfiles

#' Gadget print file defaults
#'
#' Pre-baked defaults of printfile types for use in functions
#'
#' @name printfileDefaults
NULL

printfile_years <- "all"
printfile_steps <- "all"
printfile_ys <- c(printfile_years, printfile_steps)

rm_optional_args <- function(printfile, optional_args) {
    return(names(printfile)[-grep(optional_args, names(printfile))])
}

#' @rdname printfileDefaults
stock_std <-
    structure(list(
        type = "stockstdprinter",
        stockname = NULL,
        scale = 1,
        printfile = NULL,
        precision = NULL,
        printatstart = 0,
        yearsandsteps = printfile_ys
    ), class = c("stock_std", "list"))

#' @rdname printfileDefaults
stock_std_args <- rm_optional_args(stock_std, "precision|printatstart")

#' @rdname printfileDefaults
stock_full <-
    structure(list(
        type = "stockfullprinter",
        stocknames = NULL,
        printfile = NULL,
        precision = NULL,
        printatstart = 0,
        yearsandsteps = printfile_ys
    ), class = c("stock_full", "list"))

#' @rdname printfileDefaults
stock_full_args <- rm_optional_args(stock_full, "precision|printatstart")

#' @rdname printfileDefaults
stock <-
    structure(list(
        type = "stockprinter",
        stocknames = NULL,
        areaaggfile = NULL,
        ageaggfile = NULL,
        lenaggfile = NULL,
        printfile = NULL,
        precision = NULL,
        printatstart = 0,
        yearsandsteps = printfile_ys
    ), class = c("stock", "list"))

#' @rdname printfileDefaults
stock_args <- rm_optional_args(stock, "precision|printatstart")

#' @rdname printfileDefaults
predator <-
    structure(list(
        type = "predatorprinter",
        predatornames = NULL,
        preynames = NULL,
        areaaggfile = NULL,
        predlenaggfile = NULL,
        preylenaggfile = NULL,
        biomass = NULL,
        printfile = NULL,
        precision = NULL,
        yearsandsteps = printfile_ys
    ), class = c("predator", "list"))

#' @rdname printfileDefaults
predator_args <- rm_optional_args(predator, "precision|biomass")

#' @rdname printfileDefaults
predator_over <-
    structure(list(
        type = "predatoroverprinter",
        predatornames = NULL,
        areaaggfile = NULL,
        lenaggfile = NULL,
        printfile = NULL,
        precision = NULL,
        yearsandsteps = printfile_ys
    ), class = c("predator_over", "list"))

#' @rdname printfileDefaults
predator_over_args <- rm_optional_args(predator_over, "precision")

#' @rdname printfileDefaults
prey_over <-
    structure(list(
        type = "preyoverprinter",
        preynames = NULL,
        areaaggfile = NULL,
        lenaggfile = NULL,
        printfile = NULL,
        precision = NULL,
        yearsandsteps = printfile_ys
    ), class = c("prey_over", "list"))

#' @rdname printfileDefaults
prey_over_args <- rm_optional_args(prey_over, "precision")

#' @rdname printfileDefaults
stock_prey_full <-
    structure(list(
        type = "stockpreyfullprinter",
        preyname = NULL,
        printfile = NULL,
        precision = NULL,
        yearsandsteps = NULL
    ), class = c("stock_prey_full", "list"))

#' @rdname printfileDefaults
stock_prey_full_args <- rm_optional_args(stock_prey_full, "precision")

#' @rdname printfileDefaults
stock_prey <-
    structure(list(
        type = "stockpreyprinter",
        preynames = NULL,
        printfile = NULL,
        areaaggfile = NULL,
        ageaggfile = NULL,
        lenaggfile = NULL,
        precision = NULL,
        yearsandsteps = printfile_ys
    ), class = c("stock_prey", "list"))

#' @rdname printfileDefaults
stock_prey_args <- rm_optional_args(stock_prey, "precision")

#' @rdname printfileDefaults
predator_prey <-
    structure(list(
        type = "predatorpreyprinter",
        predatornames = NULL,
        preynames = NULL,
        areaaggfile = NULL,
        ageaggfile = NULL,
        lenaggfile = NULL,
        printfile = NULL,
        precision = NULL,
        yearsandsteps = printfile_ys
    ), class = c("predator_prey", "list"))

#' @rdname printfileDefaults
predator_prey_args <- rm_optional_args(predator_prey, "precision")

#' @rdname printfileDefaults
likelihood <-
    structure(list(
        type = "likelihoodprinter",
        likelihood = NULL,
        printfile = NULL
    ), class = c("likelihood", "list"))

#' @rdname printfileDefaults
likelihood_args <- names(likelihood)

#' @rdname printfileDefaults
likelihood_summary <-
    structure(list(
        type = "likelihoodsummaryprinter",
        printfile = NULL
    ), class = c("likelihoodsummary", "list"))

#' @rdname printfileDefaults
likelihood_summary_args <- names(likelihood_summary)


#' Access elements of default printfiles and update them
#'
#' @param printfile A default printfile list, see \link{printfileDefaults}
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
#' cod_stock_std <-
#'     list(stockname = "cod", printfile = "cod.stock.std")
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


make_aggfiles <- function(printfile_comp, aggfiles, path = NULL) UseMethod("make_aggfiles")

make_aggfiles.stock <- function(printfile_comp, aggfiles, path = NULL) {
    stocknames <- printfile_comp$stockname
    area_agg <- printfile_comp$areaaggfile
    age_agg <- printfile_comp$ageaggfile
    len_agg <- printfile_comp$lenaggfile
    if (!(any(c(is.null(area_agg), is.null(age_agg), is.null(len_agg))))) {
        invisible()
    } else {
        if (is.null(stocknames)) {
            stop("You are missing the stock for %sPrinter", get_pf_type(printfile_comp))
        }
        stocks <- read_gadget_stockfiles(stocknames, path = path)
        check_area_agg(printfile_comp, stocks, area_agg, aggfiles)
        check_age_agg(printfile_comp, stocks, age_agg, aggfiles)
        check_len_agg(printfile_comp, stocks, len_agg, aggfiles)
    }
    return(printfile_comp)
}


#' Functions to make the area, age, length, predator, and prey aggfiles
#'
#' @param stocks A list of class \code{gadget.stock} or \code{gadget.stocks}
#' @param aggfiles Character. Path of the directory to print aggfiles
#'
#' @return Character vector ready to be written to file
#'
#' @name makeAggfiles
#'
#' @examples
#' main <- read_gadget_main(path = gad_mod_dir)
#' stocks <- read_gagdget_stockfiles(main = main, path = gad_mod_dir)
#' make_area_agg_dir(stocks, aggfiles = "print.aggfiles")
#' make_area_aggfile(stocks)
make_area_agg_dir <- function(stocks, aggfiles) {
    stocknames <- get_stocknames(stocks)
    return(sprintf("%s/%s.area.agg", aggfiles, paste(stocknames, collapse = ".")))
}

#' @rdname makeAggfiles
make_area_aggfile <- function(stocks) {
    areas <- get_stock_areas(stocks)
    area_agg <-
        paste(sprintf("%s\t%s", paste0("area", areas), areas), collapse = "\n")
    sprintf("%s\n%s", aggfile_header("area"), area_agg)
}

#' @rdname makeAggfiles
make_age_agg_dir <- function(stocks, aggfiles) {
    stocknames <- get_stocknames(stocks)
    return(sprintf("%s/%s.age.agg", aggfiles, paste(stocknames, collapse = ".")))
}

#' @rdname makeAggfiles
make_age_aggfile <- function(stocks) {
    ages <- get_stock_ages(stocks)
    age_agg <-
        paste(sprintf("%s\t%s", paste0("age", ages), ages), collapse = "\n")
    return(sprintf("%s\n%s", aggfile_header("age"), age_agg))
}

#' @rdname makeAggfiles
make_len_agg_dir <- function(stocks, aggfiles) {
    stocknames <- get_stocknames(stocks)
    return(sprintf("%s/%s.len.agg", aggfiles, paste(stocknames, collapse = ".")))
}

#' @rdname makeAggfiles
make_len_aggfile <- function(stocks) {
    len <- get_stock_lengths(stocks)
    len_agg <-
        paste(sprintf("%s\t%s\t%s", paste0("len", len[-1]),
                      len[-length(len)], len[-1]), collapse = "\n")
    return(sprintf("%s\n%s", aggfile_header("length"), len_agg))
}

#' @rdname makeAggfiles
check_area_agg <- function(printfile_comp, area_agg, stocks, aggfile) {
    if (is.null(area_agg)) {
        printfile_comp$areaaggfile <<- make_area_agg_dir(stocks = stocks, aggfile = aggfiles)
        attr(printfile_comp, "areaaggfile") <<- make_area_aggfile(stocks)
    }
}

#' @rdname makeAggfiles
check_age_agg <- function(printfile_comp, age_agg, stocks, aggfile) {
    if (is.null(age_agg)) {
        printfile_comp$ageaggfile <<- make_age_agg_dir(stocks = stocks, aggfile = aggfile)
        attr(printfile_comp, "areaaggfile") <<- make_area_aggfile(stocks)
    }
}

#' @rdname makeAggfiles
check_len_agg <- function(printfile_comp, len_agg, stocks, aggfile) {
    if (is.null(len_agg)) {
        printfile_comp$lenaggfile <<- make_len_agg_dir(stocks = stocks, aggfile = aggfile)
        attr(printfile_comp, "lenaggfile") <<- make_area_aggfile(stocks)
    }
}

#' Check the names of a printfile component to ensure all mandatory arguments are present
#'
#' @param printfile_comp List. The printfile component to be checked

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
        paste0(.simpleCap(unlist(strsplit(pf_args_nm,
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
#' @param printfile_comp A list of class pertaining to a Gadget printfile component,
#' see \code{\link{update_printfile}}
#' @param print_dir Character. Path of directory to print the printfiles
#' @param aggfile Character. Path of directory to print the aggfiles
#'
#' @return A list the same as \code{printfile}, but with updated printfile and
#' aggfile(s) if present
#'
#' @examples
#' cod <- list(stockname = "cod", printfile = "cod.stock.std")
#' cod_std <- update_printfile(stock_std, cod)
#' cod_std <- update_printfile_dirs(stock_std, print_dir = "printfile")
update_printfile_dirs <- function(printfile_comp, print_dir = NULL, aggfiles = NULL) {
    pf <- "printfile"
    if (any(names(printfile_comp) == "printfile")) {
        if (!is.null(print_dir)) {
            printfile_comp$printfile <- paste(print_dir, printfile_comp$printfile, sep = "/")
        }
    }
    agg_index <- grep("aggfile", names(printfile_comp))
    if (length(agg_index) > 0) {
        if (!is.null(aggfiles)) {
            printfile_comp[[agg_index]] <-
                paste(aggfiles, printfile_comp[[agg_index]], sep = "/")
        }
    }
    return(printfile_comp)
}


#' Format printfile component for writing to printfile
#'
#' @param printfile_comp A list of class pertaining to a Gadget printfile component,
#' see \code{\link{update_printfile}}
#'
#' @return
#' @export
#'
#' @examples
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


pf_types <- c("stock_std", "stock_full", "stock",
              "predator", "predator_over", "prey_over",
              "stock_prey_full", "stock_prey", "predator_prey",
              "likelihood", "likelihood_summary")

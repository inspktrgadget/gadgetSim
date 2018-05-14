# functions to read gadget files

#' Read Gadget mainfile
#'
#' @param file Character. Name of the main file to be read
#' @inheritParams call_gadget
#'
#' @return List of class \code{gadget.main} with each name corresponding to a type of file
#' and each object corresponding to a filename as used by Gadget
#' @export
#'
#' @examples
#' read_gadget_main(path = system.file(gad_mod_dir, package = "gadgetSim"))
#' read_gadget_main("WGTS/main.final")
read_gadget_main <- function(file = "main", path = NULL) {
    file <- check_path(file)
    if (!file.exists(file)) {
        stop("Main file not found")
    }
    main <- readLines(file)
    if (length(main) == 0) {
        stop(sprintf("Error in read.gadget.main, file %s is empty", file))
    }
    main <- strip_comments(main)
    keywords <- grep("^\\[", main)
    main <- main[-c(keywords)]
    typeoffile <- sapply(strsplit(main, "\\s+"), function(x) x[1])
    filenames <- sapply(strsplit(main, "\\s+"), function(x) x[-1])
    main <- lapply(filenames, function(x) {
        if (any(grepl("^;", x))) {
            return(x[!grepl("^;", x)])
        } else {return(x)}
    })
    main <- setNames(main, typeoffile)
    class(main) <- c("gadget.main", class(main))
    return(main)
}

#' Read Gadget stockfiles
#'
#' Get stockfiles used in a Gadget model
#'
#' @param stockfiles Character vector of stocknames present in Gadget model
#' @param main Optional. A list of class \code{gadget.main}
#' @inheritParams read_gadget_main
#'
#' @return A list of class \code{gadget.stocks} consisting of \code{gadget.stock}
#' one or more gadget.stock objects
#' @export
#'
#' @examples
#' path <- system.file(gad_mod_dir, package = "gadgetSim")
#' main <- read_gadget_main(path = gad_mod_dir)
#' stocks <- read_gadget_stockfiles(main = main, path = path)
#' head(stocks[[1]])
read_gadget_stockfiles <- function(stockfiles, main = NULL, path = NULL) {
    if (!is.null(main)) {
        if (!("gadget.main" %in% class(main))) {
            stop("If main is specified you must supply a list of class gadget.main")
        }
        stockfiles <- main$stockfiles
    }
    stocks2get <- check_path(stockfiles)
    stocks <-
        lapply(stocks2get, function(x) {
            tmp <- readLines(x)
            tmp <- strsplit(tmp[-(grep("^;", tmp))], split = "\t")
            tmp_names <- vapply(tmp, function(x) return(x[1]), character(1))
            tmp_cont <- lapply(tmp, function(x) return(x[-1]))
            out <- setNames(tmp_cont, tmp_names)
            return(structure(out, class = "gadget.stock"))
        })
    stocks <- setNames(stocks, stockfiles)
    return(structure(stocks, class = "gadget.stocks"))
}


#' Read Gadget fleet file
#'
#' @param fleetfiles Character name of fleet file in Gadget model as specified in main file
#' @inheritParams read_gadget_stockfiles
#'
#' @return A list detailing the fleets included in Gadget model,
#' stocks included, and suitability
#' @export
#'
#' @examples
#' path <- system.file(gad_mod_dir, package = "gadgetSim")
#' main <- read_gadget_main(path = path)
#' read_gadget_fleet(main = main, path = path)
read_gadget_fleet <- function(fleetfiles, main = NULL, path = NULL) {
    if (!is.null(main)) {
        if (!("gadget.main" %in% class(main))) {
            stop("If main is specified you must supply a list of class gadget.main")
        }
        fleetfiles <- main$fleetfiles
    }
    fleets2get <- check_path(fleetfiles)
    tmp <- readLines(fleets2get)
    tmp <- tmp[-(grep("^;", tmp))]
    comp_loc <- grep("^\\[component\\]$|^\\[fleetcomponent\\]$", tmp)
    if (any(grepl("^quotafleet$", split_tab(tmp[comp_loc+1], ind = 1)))) {
        stop("Apologies...read_gadget_fleet is not yet setup to handle quotafleets.")
    }
    suit_loc <- grep("suitability", tmp)
    fleet_info <-
        data.frame(fleet = split_tab(tmp[comp_loc + 1], ind = 2),
                   type = split_tab(tmp[comp_loc + 1], ind = 1),
                   livesonareas = split_tab(tmp[comp_loc + 2], ind = 2),
                   multiplicative = split_tab(tmp[comp_loc + 3], ind = 2),
                   amount = split_tab(tmp[grep("amount", tmp)], ind = 2),
                   stringsAsFactors = FALSE)
    prey_ind <-
        lapply(seq_along(suit_loc), function(y) {
            return((suit_loc[y] + 1):(grep("amount", tmp)[y] - 1))
        })
    prey_ind <- setNames(prey_ind, fleet_info$fleet)
    suit_info <-
        do.call("rbind", lapply(seq_along(prey_ind), function(y, n) {
            dat <-
                as.data.frame(do.call("rbind", lapply(prey_ind[[y]], function(z) {
                    split_tab(tmp[z])
                })), stringsAsFactors = FALSE)
            dat <- setNames(dat,
                            c("stock", "func", "suitability", "params"))
            dat <- cbind(data.frame(fleet = n[y]),
                         subset(dat, select = -func))
            return(dat)
        }, n = names(prey_ind)))
    return(list(fleet = fleet_info, prey = suit_info))
}


#' Read Gadget likelihood file
#'
#' @param likelihoodfiles Character. The name of the likelihood file
#' @inheritParams read_gadget_stockfiles
#'
#' @details There are a number of different likelihood types in a Gadget model. This
#' function will retrieve the likelihood file, sort and organize each component into its
#' respective likelihood type, and return a list with each item of the list containing a
#' \code{data.frame} of all the components within a respective likelihood type.
#'
#' @return A list of \code{data.frame}s; one for each likelihood type.
#' The returned list also has class \code{gadget.likelihood}
#' @export
#'
#' @examples
#' lik <- read_gadget_likelihood(system.file("gadget_model/likelihood", package = "gadgetSim"))
#'
#' ## Using the main and path arguments instead
#' path <- system.file(gad_mod_dir, package = "gadgetSim")
#' main <- read_gadget_main(path = path)
#' lik <- read_gadget_likelihood(main = main, path = path)
read_gadget_likelihood <- function(likelihoodfiles, main = NULL, path = NULL) {
    if (!is.null(main)) {
        if (!("gadget.main" %in% class(main))) {
            stop("If main is specified you must supply a list of class gadget.main")
        }
        likelihoodfiles <- main$likelihood
    }
    likelihoodfiles <- check_path(likelihoodfiles)
    lik <- readLines(likelihoodfiles)
    lik <- gf2list(strip_comments(lik), list_names = TRUE)
    get_lik_ <- function(lik_comp, object) {
            return(split_tab(lik_comp[grep(object, lik_comp)], ind = 2))
    }
    get_lik_name <- function(lik_comp, object = "^name") {
        return(get_lik_(lik_comp, object))
    }
    get_lik_type <- function(lik_comp, object = "^type") {
        return(get_lik_(lik_comp, object))
    }
    get_lik_weight <- function(lik_comp, object = "^weight") {
        return(as.numeric(get_lik_(lik_comp, object)))
    }
    organize_comp <- function(comp) {
        comp_names <- split_tab(comp, ind = 1)
        dat <- data.frame(split_tab_list(comp, ind = -1),
                             stringsAsFactors = FALSE)
        dat <- setNames(dat, comp_names)
        attr(dat, "class") <- c(get_lik_type(comp), "data.frame")
        return(dat)
    }
    temp <-lapply(lik, organize_comp)
    lik_types <- unique(vapply(temp, function(x) class(x)[1], character(1)))
    out <-
        lapply(lik_types, function(x) {
            do.call("rbind", c(lapply(temp, function(y) {
                if (x %in% class(y)) {
                    class(y) <- "data.frame"
                    return(y)
                } else {return(NULL)}
            }), make.row.names = FALSE))
        })
    out <- setNames(out, lik_types)
    class(out) <- c("gadget.likelihood", class(out))
    return(out)
}


#' Read the output from a StockStdPrinter printfile component
#'
#' This function reads specifically the output from a StockStdPrinter printfile component from Gadget
#' and formats the output into a useable \code{data.frame} in R
#'
#' @param output_dir Character. Path to the directory where output is housed
#' @inheritParams read_gadget_stockfiles
#'
#' @return \code{data.frame} of the output from a StockStdPrinter printfile component
#' @export
#'
#' @examples
#' \dontrun{
#' path <- system.file(gad_mod_dir, package = "gadgetSim")
#' make_gadget_printfile(stock_std = list(stockname = "cod", printfile = "printfile"),
#'                       main = "WGTS/main.final", file = "WGTS/printfile", path = path,
#'                       aggfile_dir = "WGTS/aggfiles")
#' call_gadget(switches = list(s = TRUE, i = "WGTS/params.final", main = "WGTS/main.final"),
#'             path = path)
#' read_gadget_stock_std("out", path = paste("WGTS", path, sep = "/"))
#' }
read_gadget_stock_std <- function(output_dir, files = NULL, path = NULL) {
    output_dir <- check_path(output_dir)
    if (!is.null(file)) {
        files2read <- files
    } else {
        files2read <- grep("stock.std", dir(output_dir), value = TRUE)
    }
    stock_std_names <- c("year", "step", "area", "age", "number",
                         "length", "weight", "length.sd",
                         "consumed", "biomass")
    stock_std <-
        lapply(files2read, function(x) {
            tmp <- read.table(paste(c(output_dir, x), collapse = "/"),
                              sep = "", comment.char = ";", stringsAsFactors = FALSE)
            tmp <- setNames(tmp, stock_std_names)
        })
    return(setNames(stock_std, gsub(".stock.std", "", files2read)))
}

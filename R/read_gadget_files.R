# functions to read gadget files

#' Read Gadget mainfile
#'
#' @param file Character. Name of the main file to be read
#'
#' @return List of class \code{gadget.main} with each name corresponding to a type of file
#' and each object corresponding to a filename as used by Gadget
#' @export
#'
#' @examples
#' read_gadget_main(path = system.file(gad_mod_dir, package = "gadgetSim"))
#' read_gadget_main("WGTS/main.final")
read_gadget_main <- function(file = "main", path = NULL) {
    if (!is.null(path)) {
        file <- paste(path, file, sep = "/")
    }
    if (!file.exists(file)) {
        stop("Main file not found")
    }
    main <- sub(" +$", "", readLines(file))
    if (length(main) == 0) {
        stop(sprintf("Error in read.gadget.main, file %s is empty", file))
    }
    comments <- grep("^;", main)
    keywords <- grep("^\\[", main)
    main <- main[-c(comments, keywords)]
    typeoffile <- sapply(strsplit(main, "\t"), function(x) x[1])
    filenames <- sapply(strsplit(main, "\t"), function(x) x[-1])
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
#' @param main List of class \code{gadget.main}
#' @param path Optional. Path to Gadget model
#'
#' @return A list of class \code{gadget.stocks} consisting of \code{gadget.stock}
#' one or more gadget.stock objects
#' @export
#'
#' @examples
#' main <- read_gadget_main("main", path = system.file(gad_mod_dir, package = "gadgetSim"))
#' stocks <- read_gadget_stockfiles(main = main)
#' head(stocks[[1]])
read_gadget_stockfiles <- function(stockfiles, main = NULL, path = NULL) {
    if (!is.null(main)) {
        if (!("gadget.main" %in% class(main))) {
            stop("If main is specified you must supply a list of class gadget.main")
        }
        stockfiles <- main$stockfiles
    }
    if (!is.null(path)) {
        stocks2get <- paste(path, stockfiles, sep = "/")
    } else {stocks2get <- stockfiles}
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
#' @param main Optional. Object of class \code{gadget.main} used to search for fleet file
#' @param path Character name of path to Gadget model
#'
#' @return A list detailing the fleets included in Gadget model,
#' stocks included, and suitability
#' @export
#'
#' @examples
#' main <- read_gadget_main(path = system.file(gad_mod_dir, package = "gadgetSim"))
#' read_gadget_fleet(main = main, path = system.file(gad_mod_dir, package = "gadgetSim"))
read_gadget_fleet <- function(fleetfiles, main = NULL, path = NULL) {
    if (!is.null(main)) {
        if (!("gadget.main" %in% class(main))) {
            stop("If main is specified you must supply a list of class gadget.main")
        }
        fleetfiles <- main$fleetfiles
    }
    if (!is.null(path)) {
        fleets2get <- paste(path, fleetfiles, sep = "/")
    } else {fleets2get <- fleetfiles}
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
                   amount = split_tab(tmp[get_index("amount", tmp)], ind = 2),
                   stringsAsFactors = FALSE)
    prey_ind <-
        lapply(seq_along(suit_loc), function(y) {
            return((suit_loc[y] + 1):(get_index("amount", tmp)[y] - 1))
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

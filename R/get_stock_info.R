# functions to retrieve basic stock information (e.g. age, areas, etc.)

#' Functions to retrieve basic information about Gadget stocks
#'
#' @param stocks A list of class \code{gadget.stocks} containing one (or typically more)
#' lists of class \code{gadget.stock}
#'
#' @return A vector of values found in the Gadget stock file
#'
#' @name getStockInfo
#'
#' @examples
#' cod <- read_gadget_stockfiles("cod", path = gad_mod_dir)
#' get_stock_ages(cod)
#'
get_stock_ages <- function(stocks) {
    if (class(stocks) == "gadget.stock") {
        return(as.numeric(stocks$minage):(as.numeric(stocks$maxage)))
    } else if (class(stocks) == "gadget.stocks") {
        return(unique(unlist(lapply(stocks, get_stock_ages))))
    } else {
        stop("You must supply a list of class gadget.stock or gadget.stocks")
    }
}

#' @rdname getStockInfo
get_stock_areas <- function(stocks) {
    if (class(stocks) == "gadget.stock") {
        return(stocks$livesonareas)
    } else if (class(stocks) == "gadget.stocks") {
        return(unique(unlist(lapply(stocks, get_stock_areas))))
    } else {
        stop("You must supply a list of class gadget.stock or gadget.stocks")
    }
}

#' @rdname getStockInfo
get_stockname <- function(stocks) {
    if (class(stocks) == "gadget.stock") {
        return(stocks$stockname)
    } else if (class(stocks) == "gadget.stocks") {
        return(unique(unlist(lapply(stocks, get_stockname))))
    } else {
        stop("You must supply a list of class gadget.stock or gadget.stocks")
    }
}

#' @rdname getStockInfo
get_stock_lengths <- function(stocks) {
    if (class(stocks) == "gadget.stock") {
        return(seq(as.numeric(stocks$minlength),
                   as.numeric(stocks$maxlength),
                   by = as.numeric(stocks$dl)))
    } else if (class(stocks) == "gadget.stocks") {
        minlength <-
            min(as.numeric(unique(unlist(lapply(stocks, function(x) x$minlength)))))
        maxlength <-
            max(as.numeric(unique(unlist(lapply(stocks, function(x) x$maxlength)))))
        dl <-
            min(as.numeric(unique(unlist(lapply(stocks, function(x) x$dl)))))
        return(seq(minlength, maxlength, by = dl))
    } else {
        stop("You must supply a list of class gadget.stock or gadget.stocks")
    }
}

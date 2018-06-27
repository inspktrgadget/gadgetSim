# functions to retrieve basic stock information (e.g. age, areas, etc.)

#' Functions to retrieve basic information about Gadget stocks
#'
#' @param stocks A list of class \code{gadget_stock} or a list of class \code{gadget_stocks} containing one (or typically more)
#' lists of class \code{gadget_stock}
#' @param what2get Character vector of which component of a stockfile to get.
#'
#' @return A vector of values found in the Gadget stock file possibly coerced to numeric if appropriate
#'
#' @name get_stock_info
#'
#' @examples
#' cod <- read_gadget_stockfiles("cod", path = gad_mod_dir)
#' get_stock_ages(cod)
#' get_stock_areas(cod)
#' get_stocknames(cod)
#' get_stock_lengths(cod)
#' get_stock_anything(cod, "normalparamfile")
#' get_stock_anything(cod, "dl")
get_stock_anything <- function(stocks, what2get) {
    if ("gadget_stock" %in% class(stocks)) {
		tmp <- tryCatch(as.numeric(stocks[[what2get]]),
					warning = function(w) return(stocks[[what2get]]),
					error = function(e) return(stocks[[what2get]])
				)
		return(tmp)
	} else if ("gadget_stocks" %in% class(stocks)) {
	    return(unique(unlist(lapply(stocks, get_stock_anything, what2get = what2get))))
	} else {
	    stop("You must supply a list of class gadget_stock or gadget_stocks and something to fetch")
	}
}

#' @rdname get_stock_info
get_stock_ages <- function(stocks) {
    if ("gadget_stock" %in% class(stocks)) {
        return(as.numeric(stocks$minage):(as.numeric(stocks$maxage)))
	} else if ("gadget_stocks" %in% class(stocks)) {
        return(unique(unlist(lapply(stocks, get_stock_ages))))
    } else {
        stop("You must supply a list of class gadget_stock or gadget_stocks")
    }
}

#' @rdname get_stock_info
get_stock_areas <- function(stocks) {
    if ("gadget_stock" %in% class(stocks)) {
        return(as.numeric(stocks$livesonareas))
	} else if ("gadget_stocks" %in% class(stocks)) {
        return(unique(unlist(lapply(stocks, get_stock_areas))))
    } else {
        stop("You must supply a list of class gadget_stock or gadget_stocks")
    }
}

#' @rdname get_stock_info
get_stocknames <- function(stocks) {
    if ("gadget_stock" %in% class(stocks)) {
        return(stocks$stockname)
	} else if ("gadget_stocks" %in% class(stocks)) {
        return(unique(unlist(lapply(stocks, get_stocknames))))
    } else {
        stop("You must supply a list of class gadget_stock or gadget_stocks")
    }
}

#' @rdname get_stock_info
get_stock_lengths <- function(stocks) {
    if ("gadget_stock" %in% class(stocks)) {
        return(seq(as.numeric(stocks$minlength),
                   as.numeric(stocks$maxlength),
                   by = as.numeric(stocks$dl)))
	} else if ("gadget_stocks" %in% class(stocks)) {
        minlength <-
            min(as.numeric(unique(unlist(lapply(stocks, function(x) x$minlength)))))
        maxlength <-
            max(as.numeric(unique(unlist(lapply(stocks, function(x) x$maxlength)))))
        dl <-
            min(as.numeric(unique(unlist(lapply(stocks, function(x) x$dl)))))
        return(seq(minlength, maxlength, by = dl))
    } else {
        stop("You must supply a list of class gadget_stock or gadget_stocks")
    }
}

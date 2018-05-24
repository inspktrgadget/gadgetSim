# growth functions in Gadget

vb <- function(linf, k, t0, age) {
    linf * (1 - exp(-k * (age - t0)))
}

#' Produce growth functions in a gadget readable formula
#'
#' @param stockname Character. The stockname desired for variables in the von Bertalanffy function
#' @param age Numeric vector of ages
#'
#' @return A character vector of the von Bertalanffy function the same length as \code{age}.
#' Format is as a Gadget formula, see \code{\link{to_gadget_formula}}
#'
#' @export
#'
#' @examples
#' vb_formula("cod", 1:2)
vb_formula <- function(stockname, age) {
    add_stockname <- function(arg) {
        return(sprintf("%s.%s", stockname, arg))
    }
    linf <- add_stockname("linf")
    k <- add_stockname("k")
    t0 <- add_stockname("t0")
    vonb <-
        as_quoted_(
            paste0(linf,
                   " * (1 - exp((-1 *",
                   k,
                   ") * (",
                   age,
                   " - ",
                   t0,
                   ")))"))
    sapply(vonb, to_gadget_formula)
}

vb_recl_formula <- function(stockname, age) {
    add_stockname <- function(arg) {
        return(sprintf("%s.%s", stockname, arg))
    }
    linf <- add_stockname("linf")
    k <- add_stockname("k")
    recl <- add_stockname("recl")
    vonb <-
        as_quoted_(
            paste0(linf,
                   "* (1 - exp((-1 *",
                   k,
                   ") * (",
                   age,
                   "- (1 + (log(1 - (",
                   recl,
                   "/",
                   linf,
                   ")))/",
                   k,
                   "))))"))
    sapply(vonb, to_gadget_formula)
}

#' Produce recruit functions in a Gadget readable formula
#'
#' These functions will produce recruitment curves such as Beverton-Holt or Ricker in a format that
#' is readable to Gadget. These are for use in creating spawnfiles.
#'
#' @param stockname A character vector of a stockname that will precede the switches for \eqn{\mu} and
#' \eqn{\lambda} arguments, or, optionally, a numberic vector of length 1 if SimpleSSB is desired or
#' of length 2 that will be used as \eqn{\mu} and \eqn{\lambda} for either the Ricker or
#' Beverton-Holt recruitment functions to fix those parameter values
#'
#' @return A character vector to be written to a Gadget file
#' @export
#'
#' @name recruit_formulas
#'
#' @examples
#' bev_holt_formula("cod")
#' bev_holt_formula(c(4e9, 1e8))
#' ricker_formula("cod")
simple_ssb_formula <- function(stockname, params = NULL) {
    if (!is.null(params)) {
        if (length(params) != 1) {
            stop("You must enter a single parameter (mu) for SimpleSSB recruitment")
        }
        mu <- params
    } else {
        mu <- c(paste0("#", stockname, "simplessb.mu"))
    }
    paste("simplessb", mu, sep = "\t")
}

#' @rdname recruit_formulas
ricker_formula <- function(stockname, params = NULL) {
    if (!is.null(params)) {
        if (length(params) != 2) {
            stop("You must enter mu and lambda for Ricker Recruitment")
        }
        mu <- params[1]
        lam <- params[2]
    } else {
        mu <- c(paste0("#", stockname, ".ricker.mu"))
        lam <- c(paste0("#", stockname, ".ricker.lam"))
    }
    paste("ricker", mu, lam, sep = "\t")
}

#' @rdname recruit_formulas
bev_holt_formula <- function(stockname, params = NULL) {
    if (!is.null(params)) {
        if (length(params) != 2) {
            stop("You must enter mu and lambda for Beverton-Holt Recruitment")
        }
        mu <- params[1]
        lam <- params[2]
    } else {
        mu <- c(paste0("#", stockname, ".bh.mu"))
        lam <- c(paste0("#", stockname, ".bh.lam"))
    }
    paste("bevertonholt", mu, lam, sep = "\t")
}

# growth functions in Gadget

vb <- function(linf, k, t0, age) {
    linf * (1 - exp(-k * (age - t0)))
}

#' Produce Gadget formula for von Bertalanffy function
#'
#' @param stockname Character. The stockname desired for variables in the von Bertalanffy function
#' @param age Numeric vector of ages
#'
#' @return A character vector of the von Bertalanffy function the same length as \code{age}. Format is
#' as a Gadget formula, see \code{\link{to_gadget_formula}}
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

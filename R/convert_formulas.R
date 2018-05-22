# functions to convert functions and math to gadget-readable functions (i.e. reverse polish notation)

#' Turn R expression into Gadget formula string
#'
#' This function is stolen directly from Rgadget::to.gadget.formulae. It takes an unevaluated R
#' expression (e.g. quote(2 + log(moo - 1))) and converts it into a character string that is readable
#' by Gadget
#'
#' @param ex An unevaluated R expression (i.e. enclosed in quotes)
#' @param stocknames Optional. Character vector of stocknames to add to any formula variable names
#'
#' @details Gadget uses reverse Polish notation to read formulas (i.e. the operator comes first,
#' followed by the items to be operated on; 2 + 2 is read as (+ 2 2)). This function will take
#' an expression recognizable by R and convert it to one that is recognizable by Gadget
#'
#' @return A character vector that is readable as a Gadget formula
#' @export
#'
#' @examples
#' to_gadget_formula(quote(2 + 2))
#' to_gadget_formula(quote(2 + log(moo - 1)))
to_gadget_formula <- function(ex, stocknames = NULL) {
    if (is.name(ex)) {
        return(paste0("#", ex))
    }
    if (is.numeric(ex) || is.character(ex)) {
        return(as.character(ex))
    }
    if (is.call(ex)) {
        ex_fn <- as.character(ex[[1]])
        ex_args <- if (length(ex) > 1)
            as.list(ex[2:length(ex)])
        else list()
        if (ex_fn == "(" && length(ex_args) == 1) {
            return(to_gadget_formula(ex_args[[1]]))
        }
        return(paste0("(", ex[[1]], " ", paste(lapply(ex_args,
                                                      to_gadget_formula), collapse = " "), ")"))
    }
    stop("Don't know what to do with: ", capture.output(str(ex)))
}

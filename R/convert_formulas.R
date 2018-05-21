# functions to convert functions and math to gadget-readable functions (i.e. reverse polish notation)

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

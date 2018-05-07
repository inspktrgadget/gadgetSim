# useful functions to use in package. None are exported

#' Split and unlist a character vector
#'
#' @param vec Character
#' @param split Character vector or object which can be coerced to such
#'
#' @return A character vector of vec split by \code{split}
#' @name split
#'
#'
#' @examples
split_ <- function(vec, split, ind = NULL) {
    if (is.null(ind)) {
        out <-
            lapply(seq_along(vec), function(x) {
                unlist(strsplit(vec[x], split = split))
            })
    } else {
        out <-
            lapply(seq_along(vec), function(x) {
                unlist(strsplit(vec[x], split = split))[ind]
            })
    }
    return(unlist(out))
}

#' @rdname split
split_tab <- function(vec, split = "\t", ind = NULL) {
    split_(vec, split = split, ind = ind)
}


get_index <- function(x, vec) {
    return(grep(x, vec))
}

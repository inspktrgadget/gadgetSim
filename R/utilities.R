# useful functions to use in package. None are exported

#' Split and unlist a character vector
#'
#' @param vec Character
#' @param split Character vector or object which can be coerced to such
#'
#' @return A character vector of vec split by \code{split}
#' @name split_
#'
#' @examples
#' split_("may the force be with you", split = " ")
#' split_tab("may\tthe\tforce\tbe\twith\tyou")
split_ <- function(vec, split, ind = NULL) {
    return(unlist(split_list_(vec = vec, split = split, ind = ind)))
}

#' @rdname split_
split_tab <- function(vec, ind = NULL) {
    split_(vec, split = "\t", ind = ind)
}

#' @rdname split_
split_list_ <- function(vec, split, ind = NULL) {
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
    return(out)
}

#' @rdname split_
split_tab_list <- function(vec, ind = NULL) {
    split_list_(vec, split = "\t", ind = ind)
}


#' Get the index of obect(s) in a vector
#'
#' @param x Object for which the index will be retrieved
#' @param vec Vector against which x will be indexed
#'
#' @return
#'
#' @name getIndices
#'
#' @examples
#' get_index(2, 2:10)
#' get_index("q", letters)
get_index <- function(x, vec) {
    if (length(x) == 1) {
        out <- grep(x, vec)
    } else if (length(x) > 1) {
        out <-
            vapply(x, function(y) {
                grep(y, vec)
            }, numeric(1))
    }
    return(as.vector(out))
}

#' Paste path variable and file name together if path exists
#'
#' @param x Character. Name of file
#'
#' @return Character. Either the file name or the \code{path} and file name pasted if
#' \code{path} exists
#'
#' @examples
#' check_path("main")
check_path <- function(x, env = parent.frame()) {
    if (exists("path", envir = env)) {
        if (!is.null(env$path)) {
            x <- paste(env$path, x, sep = "/")
        }
    }
    return(x)
}


#' Convert a Gadget file to a list of components based on a regular expression
#'
#' Gadget files often contain information about a number of components that are
#' divided by a regular expression (often \code{[component]}). \code{gf2list} converts
#' the direct output from, say, a \code{readLines(x)} call into a list that can be named
#'
#' @param x Vector of character vectors of a raw Gadget file
#' @param split Regular expression to split by
#' @param list_names Optional. If \code{TRUE}, \code{gf2list} will name the components of
#' \code{x} by the first character vector in each component. Otherwise a character vector of
#' names the length of \code{x} can be supplied
#'
#' @return A list of components from a Gadget file
#'
#' @examples
#' path <- system.file(gad_mod_dir, package = "gadgetSim")
#' raw_lie <- readLines(paste(path, "likelihood", sep = "/"))
#' gf2list(raw_lik)
#' gf2list(raw_lik, list_names = TRUE)
gf2list <- function(x, split = "^\\[component\\]$", list_names = FALSE) {
    split_ind <- grep(split, x)
    out <-
        lapply(seq_along(split_ind[-length(split_ind)]), function(y) {
            tmp <- x[(split_ind[y] + 1):(split_ind[y+1] - 1)]
        })
    if (list_names) {
        list_names <-
            vapply(out, function(y) {
                split_tab(y[1], ind = 2)
            }, character(1))
        out <- setNames(out, list_names)
    } else if (!is.logical(list_names)) {
        out <- setNames(out, list_names)
    }
    return(out)
}


#' Strip comments from a character vector
#'
#' @param x Character vector
#'
#' @return A character vector with comments removed
#'
#' @examples
#' x <- c("; this is a comment in Gadget", "main\tmainfile")
#' strip_comments(x)
strip_comments <- function(x, comment = ";") {
    comments_ind <- grep(paste0("^", comment), x)
    return(x[-comments_ind])
}


#' Simple function to capitalize all words of a character vector
#'
#' @param x Character vector
#'
#' @return A character vector with each word capitalized
#'
#' @examples
#' .simpleCap("may the force be with you")
.simpleCap <- function(x) {
    s <- vapply(x, function(x) strsplit(x, " ")[[1]], character(1))
    paste(toupper(substring(s, 1, 1)), substring(s, 2),
          sep = "", collapse = "")
}


#' Replace an underscore with a period
#'
#' @param x Character vector containing an underscore
#'
#' @return Character vector with underscores replaced by period
#'
#' @examples
#' us2dot("what's up with www_google_com")
us2dot <- function(x) {
    return(gsub("_", ".", x))
}


#' Format the class of a printfile component to look like that given in Gadget manual
#'
#' @param printfile_comp A list of class pertaining to a Gadget printfile component,
#' see \code{\link{update_printfile}}
#'
#' @return Character vector of the class of \code{printfile_comp}
#'
#' @examples
#' get_pf_type(stock_std)
get_pf_type <- function(printfile_comp) {
   paste(.simpleCap(unlist(strsplit(class(printfile_comp)[1], split = "_"))), collapse = "")
}

#' Some default labels
#'
#' These are just basic default labels for some things that are used frequently
#' throughout functions in this package.
comp_lab <- "[component]"
comp_regex <- "^\\[component\\]$"
aggfile_header <- function(af_type) {
    return(sprintf("; %s aggfile created by gadgetSim %s on %s",
                   af_type, packageVersion("gadgetSim"), date()))
}

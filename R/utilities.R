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
split_ws <- function(vec, ind = NULL) {
    split_(vec, split = "\\s+", ind = ind)
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

#' @rdname split_
split_ws_list <- function(vec, ind = NULL) {
    split_list_(vec, split = "\\s+", ind = ind)
}


#' Get the index of obect(s) in a vector
#'
#' @param x Object for which the index will be retrieved
#' @param vec Vector against which x will be indexed
#'
#' @return
#'
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


#' Split a vector into a list based on indices
#'
#' This function takes a vector and indices from that vector and splits the vector into a
#' list at the assigned indices. Values at the indices can either be kept or discarded.
#'
#' @param vec A vector of any length or class
#' @param ind Numeric vector depicting the indices at which to split the list
#' @param keep_indices Logical. Return the indices in each element of the list or not
#'
#' @return A list with length equal to that of the lenght of \code{vec}. Each element
#' contains the values in between the indices including the values at the indices if
#' \code{keep_indices = TRUE}, the default.
#' @export
#'
#' @examples
#' a <- letters
#' ind <- grep("a|e|i|o|u", a)
#' consonants <- make_list_at_index(a, ind, keep_indices = FALSE)
make_list_at_index <- function(vec, ind, keep_indices = TRUE) {
    if (keep_indices) {
        factors <-
            lapply(seq_along(ind), function(x) {
                if (x == length(ind)) {
                    rep(ind[x], (length(vec) - ind[x] + 1))
                } else {
                    rep(ind[x], (ind[x+1] - ind[x]))
                }
            })
    } else {
        factors <-
            lapply(seq_along(ind), function(x) {
                if (x == length(ind)) {
                    rep(ind[x] + 1, (length(vec) - ind[x]))
                } else {
                    rep(ind[x] + 1, (ind[x+1] - ind[x] - 1))
                }
            })
        vec <- vec[-ind]
    }
    return(unname(split(vec, unlist(factors))))
}


#' Checking path and directory status
#'
#' These functions complete various tasks related to directory structure and
#' inclusion of a path variable in many of the functions throughout the package.
#'
#' \code{check_path} looks for an object named \code{path} in the environment
#' specified by \code{env} and attaches it in front of \code{x} separated by
#' a "/". \code{check_dir_exists} checks to see if a directory exists, and, if
#' not, makes one in directory specified by \code{path}.
#'
#' Both \code{check_path} and \code{check_dir_exists} will recognize either a
#' character vector as path or an object of class \code{gadget_sub_dir}. If path
#' is the latter, then some special characters can also be utilized. These are
#' of the most use when writing likelihood and data files to sub-directories of
#' a Gadget model (for instance, when running multiple replications of a
#' simulation model are desired for testing purposes).
#'
#' @param x Character. Name of file
#' @param env An environment to search for the object named \code{path}
#' @param sub_dir_only Logical. Only paste the sub-directory path when
#' \code{path} is an object of class \code{\link{gadget_sub_dir}}
#' @param ignore_subdir Logical. Only paste the path without the sub-directory
#' when \code{path} is an object of class \code{\link{gadget_sub_dir}}
#'
#' @return \code{check_path} returns a character of either the \code{x} or the
#' \code{path} and \code{x} pasted if \code{path} exists in the environment
#' \code{env}. If \code{path} is an object of class \code{\link{gadget_sub_dir}}
#' then a character of either the path and sub-dir (default), or \code{x} pasted
#' with sub-directory (\code{sub_dir_only = TRUE}), or \code{x} pasted with
#' only the path and sub-directory not included (\code{ignore_subdir = TRUE}).
#' These are of use when writing likelihood aggfiles and data files in sub-
#' directories. \code{check_dir_exists} returns nothing. It searches for the
#' directory \code{dir} within \code{path} if it exists and creates a
#' directory named \code{dir} if it is not found
#'
#' @name path_dir_funs
#'
#' @examples
#' check_path("main")
#' path <- "test_dir"
#' check_path("main")
#'
#' \dontrun{
#' check_dir_exists("foo")
#' check_dir_exists("bar")
#' }
check_path <- function(x, env = parent.frame(),
                       sub_dir_only = FALSE, ignore_subdir = FALSE) {
    if (is.null(x)) {
        return(x)
    } else if (exists("path", envir = env)) {
        if ("gadget_sub_dir" %in% class(env$path)) {
            if (sub_dir_only) {
                return(paste(c(env$path$sub_dir, x), collapse = "/"))
            } else if (ignore_subdir) {
                return(paste(c(env$path$path, x), collapse = "/"))
            } else {
                return(paste(c(env$path$path, env$path$sub_dir, x),
                             collapse = "/"))
            }
        } else if (!is.null(env$path)) {
            x <- paste(env$path, x, sep = "/")
        }
    }
    return(x)
}

#' @rdname path_dir_funs
#' @export
#' @param dir Character. The name of a directory to look for (and create if
#' not found)
check_dir_exists <- function(dir = NULL, recursive = FALSE) {
    if ("gadget_sub_dir" %in% class(dir)) {
        dir <- paste(c(dir$path, dir$sub_dir), collapse = "/")
        recursive <- TRUE
    }
    if (is.null(dir)) {
        invisible()
    } else if (!dir.exists(dir)) {
        dir.create(dir, recursive = recursive)
    }
}


check_files_exist <- function(files, path = NULL) {
    if (length(files) == 0) {
        invisible()
    } else {
        if (!is.null(path)) {
            files_present <- dir(path, recursive = TRUE)
        } else {
            files_present <- dir(recursive = TRUE)
            path <- "working"
        }
        if (!all(files %in% files_present)) {
            missing_files <- !(files %in% files_present)
            stop("The following files are missing from ", path, " directory.",
                 paste(paste0(" * ", files[missing_files]), collapse = "\n"))
        }
    }
}


#' Logical test to see if name(s) are in an object
#'
#' This function tests to see if names \code{x} are found in an object and
#' returns TRUE if they are
#'
#' @param x Character. Regular expression of name
#' @param obj Named object. Can be a vector, list, data.frame, or any other
#' object that has a names attribute
#'
#' @return Logical. TRUE if names \code{x} exist in \code{obj}
#'
#' @examples
#' dat <- data.frame(a = 1, b = 2)
#' check_names("a", dat)
#' check_names("c", dat)
check_names <- function(x, obj) {
    return(any(grepl(x, names(obj))))
}


#' Convert a Gadget file to a list of components based on a regular expression
#'
#' Gadget files often contain information about a number of components that are
#' divided by a regular expression (often \code{[component]}). \code{gf2list}
#' converts the direct output from, say, a \code{readLines(x)} call into a list
#' that can be named
#'
#' @param x Vector of character vectors of a raw Gadget file
#' @param split Regular expression to split by
#' @param list_names Optional. If \code{TRUE}, \code{gf2list} will name the
#' components of \code{x} by the first character vector in each component.
#' Otherwise a character vector of names the length of \code{x} can be supplied
#'
#' @return A list of components from a Gadget file
#'
#' @examples
#' path <- system.file(gad_mod_dir, package = "gadgetSim")
#' raw_lik <- readLines(paste(path, "likelihood", sep = "/"))
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
#' @name strip_comments
#'
#' @examples
#' x <- c("; this is a comment in Gadget", "main\tmainfile")
#' strip_comments(x)
strip_comments <- function(x, comment, keep_with = NULL) {
    UseMethod("strip_comments")
}

#' @rdname strip_comments
strip_comments.character <- function(x, comment = ";", keep_with = NULL) {
    comments_ind <- grep(paste0("^", comment), x)
    if (length(comments_ind) > 0) {
        out <- x[-comments_ind]
    } else {out <- x}
    split_char <- function(y, comment, keep_with)
        vapply(y, function(z) {
            if (!is.null(keep_with)) {
                if (grepl(keep_with, z)) {
                    return(z)
                }
            }
            dissect <- unlist(strsplit(z, split = "\\s+"))
            comments_ind <- grep(paste0("^", comment), dissect)
            if (length(comments_ind) == 0) {
                tmp <- dissect
                if (any(grepl(comment, tmp))) {
                    comment_ind <- grep(comment, tmp)
                    comment_culprit <-
                        unlist(strsplit(tmp[comment_ind], split = ""))
                    comment_char_ind <- grep(comment, comment_culprit)
                    tmp <- c(tmp[1:(comment_ind - 1)],
                             paste0(comment_culprit[1:(comment_char_ind - 1)]))
                }
                return(paste(tmp, collapse = "\t"))
            } else {
                tmp <- dissect[-(comments_ind[1]:length(dissect))]
                return(paste(tmp, collapse = "\t"))
            }
        }, character(1))
    if (length(x) > 1) {
        out <-
            unlist(lapply(out, split_char,
                          comment = comment,
                          keep_with = keep_with))
    } else {out <- split_char(out, comment = comment, keep_with = keep_with)}
    return(as.vector(out))
}

#' @rdname strip_comments
strip_comments.list <- function(x, comment = ";", keep_with = NULL) {
    out <-
        lapply(x, function(y) {
            strip_comments(y, keep_with = keep_with)
        })
    return(out)
}


#' Convert input to quoted variables
#'
#' This function is stolen from plyr::as.quoted. Converts characters to quoted
#' variables
#'
#' @param x input to quote
#' @param env environment in which unbounded symbols in expression should be
#' evaluated. Defaults to the environment in which \code{as.quoted} was executed
#'
#' @return a list of quoted variables
#'
#' @examples
#' as.quoted(c("a", "b", "log(d)"))
as_quoted_ <- function(x, env = parent.frame()) {
    structure(lapply(x, function(x) parse(text = x)[[1]]), env = env,
              class = "quoted")
}


#' Simple function to capitalize all words of a character vector
#'
#' @param x Character vector
#'
#' @return A character vector with each word capitalized
#'
#' @examples
#' simpleCap("may the force be with you")
simpleCap <- function(x) {
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


#' Format the class of a printfile component to look like that given in Gadget
#' manual
#'
#' @param printfile_comp A list of class pertaining to a Gadget printfile
#' component, see \code{\link{update_printfile}}
#'
#' @return Character vector of the class of \code{printfile_comp}
#'
#' @examples
#' get_pf_type(stock_std)
get_pf_type <- function(printfile_comp) {
   paste(simpleCap(unlist(strsplit(class(printfile_comp)[1], split = "_"))),
         collapse = "")
}

#' Check if \code{...} is a list or a number of named objects and export to list
#' appropriately
#'
#' @description Depending on how arguments of \code{...} are supplied to a
#' function \code{as.list()} will fail with an error or \code{list()} will
#' over-concatenate the argument(s). These functions check to see how \code{...}
#' is entered and exports
#' to the appropriate type to be used in functions
#'
#' @param ... Either a list or any number of named objects
#'
#' @details \code{is_list_dots} will return a logical value. \code{dots2list}
#' will return a list of the values of \code{...}
#'
#' @name dots2list
#'
#' @examples
#' is_list_dots(list(a = 1, b = 2))
#' is_list_dots(a = 1, b = 2)
#'
#' dots2list(list(a = 1, b = 2))
#' dots2list(a = 1, b = 2)
is_list_dots <- function(...) {
    tryCatch({is.list(...)},
             warnings = function(w) {
                 return(FALSE)
             },
             error = function(e) {
                 return(FALSE)
             })
}

#' @rdname dots2list
dots2list <- function(...) {
    if (is_list_dots(...)) {
        dots <- as.list(...)
    } else {
        dots <- list(...)
    }
    return(dots)
}



#' Check to see if elements of a list are null
#'
#' @param lst Any list
#'
#' @details \code{is.null} does not work to check individual elements of a list.
#' This function does that an returns a logical vector the length of \code{lst}
#' stating whether each element of \code{lst} is null (TRUE) or not (FALSE)
#'
#' @return A logical vector of length \code{lst}
#'
#' @examples
#' is_list_element_null(list(a = 1, b = 2, c = NULL, d = NULL))
is_list_element_null <- function(lst, keep_names = FALSE) {
    tmp <- unlist(lapply(lst, is.null))
	if (keep_names) {
		return(tmp)
	} else {
		return(as.vector(tmp))
	}
}

rm_null_elements <- function(lst, keep_attributes = TRUE) {
    if (keep_attributes) {
        if (!is.null(names(lst))) {
            lst_attr <- attributes(lst)
            lst_attr <- lst_attr[-1]
        } else {
            lst_attr <- NULL
        }
        out <- Filter(Negate(is.null), lst)
        lst_nms <- names(out)
        attributes(out) <- c(list(names = lst_nms), lst_attr)
    } else {
        out <- Filter(Negate(is.null), lst)
    }
    return(out)
}

#' Some default labels
#'
#' These are just basic default labels for some things that are used frequently
#' throughout functions in this package.
#'
#' @param af_type Character. The aggfile type to be included in header label
#' (e.g. area, age, len)
#'
#' @return \code{aggfile_header} returns a character to be used as a header for
#' printing text files
#'
#'
#' @name def_labs
#'
#' @examples
#' aggfile_header("area")
aggfile_header <- function(af_type = "") {
    return(sprintf("; %s aggfile created by gadgetSim %s on %s",
                   af_type, packageVersion("gadgetSim"), date()))
}

#' @rdname def_labs
#' @param gf Character. The type of Gadget file being written
gadgetfile_header <- function(gf = "") {
    return(sprintf("; Gadget %s file created by gadgetSim %s on %s",
                   gf, packageVersion("gadgetSim"), date()))
}

#' @rdname def_labs
#' @param df Character. The type of data file being written
datafile_header <- function(df = "") {
    return(sprintf("; %s data file created by gadgetSim %s on %s",
                   df, packageVersion("gadgetSim"), date()))
}
#' @rdname def_labs
comp_lab <- "[component]"
#' @rdname def_labs
comp_regex <- "^\\[component\\]$"



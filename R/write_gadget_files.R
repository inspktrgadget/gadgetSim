# functions to write gadget files

#' Write Gadget components to file
#'
#' \code{write_gadget_file} is a generic function for the various Gadget filetypes produced by
#' gadgetSim. There are a number of S3 methods corresponding to each Gadget filetype (see below).
#' These functions write the various classes of Gadget filetypes into a text file that can be
#' understood by Gadget. In addition, any attributes containing data will be written to the
#' corresponding filename for that attribute.
#'
#' @param gf A list of class \code{gadget_main}, \code{gadget_time}, \code{gadget_area},
#' \code{gadget_stock}, \code{gadget_fleet}, \code{gadget_printfile}, or \code{gadget_params}
#' @param file Character. The name of the file to be written
#' @param path Optional. Character of the path of the directory to write files to
#'
#' @return Nothing. Writes the respective Gadget filetype to a text file
#' @export
#'
#' @name write_gadget_file
#'
#' @examples
#' \dontrun{
#' main <- list(timefile = "time", areafile = "area", stockfiles = "cod")
#' main <- make_gadget_mainfile(main)
#' write_gadget_file(main)
#'
#' # optional use of a path
#' write_gadget_file(main, path = "model_dir")
#' }
write_gadget_file <- function(gf, file = NULL, path = NULL, output_dir = NULL,
                              aggfile_dir = NULL, fit_dir = NULL) {
    UseMethod("write_gadget_file")
}

#' @rdname write_gadget_file
write_gadget_file.gadget_main <- function(gf, file = "main", path = NULL) {
    if (is.null(gf)) {
        gf <- gadget_main_default
        warning(paste("No Gadget main file found. Writing the default to file",
                      file))
    } else if (!("gadget_main" %in% class(gf))) {
        stop("Class of file should contain gadget_main.")
    }
    if (length(gf$printfiles) == 0) {
        gf$printfiles <- "; Required comment"
    }
    filenames <-
        vapply(gf, function(x) {
            return(paste0("\t", x, collapse = "\t"))
        }, character(1))
    mainfile <- paste0(names(filenames), filenames)
    tap_names <- c("timefile", "areafile", "printfiles")
    match_main <- match(tap_names, names(gf))
    if (any(is.na(match_main))) {
        stop("One of timefile, areafile, or printfiles missing.")
    }
    tap_files <- mainfile[match_main]
    keywords <-
        list(stock = "stockfiles",
             tagging = "tagfiles",
             otherfood = "otherfoodfiles",
             fleet = "fleetfiles",
             likelihood = "likelihoodfiles")
    matchkey <- match(names(filenames), keywords, nomatch = 0)
    keywords[matchkey] <- mainfile[4:length(mainfile)]
    main_template <-
        c(gadgetfile_header("main"),
          tap_files,
          keywords)
    main <-
        lapply(seq_along(main_template), function(x, n) {
            if (n[x] == "") {
                return(c(main_template[[x]]))
            } else {
                return(c(paste0("[", n[x], "]"),
                         main_template[[x]]))
            }
        }, n = names(main_template))
    main <- unlist(main)
    check_dir_exists(path)
    write(main, file = check_path(file), sep = "\n")
}

#' @rdname write_gadget_file
write_gadget_file.gadget_time <- function(gf, file = "time", path = NULL) {
    time_args <- paste(names(gf), collapse_entries(gf), sep = "\t")
    header <- gadgetfile_header("time")
    timefile <- paste(c(header, time_args), collapse = "\n")
    check_dir_exists(path)
    write(timefile, file = check_path(file))
}

#' @rdname write_gadget_file
write_gadget_file.gadget_area <- function(gf, file = "area", path = NULL) {
    gf <- collapse_entries(gf)
    areasize <- gf[c("areas", "size")]
    areasize_args <- paste(names(areasize), areasize, sep = "\t")
    temperature <- gf$temperature
    temperature_args <- paste(c("temperature", "; -- Data --",
                                paste(c(";", "year", "step", "area", "mean.temp"), collapse = "\t"),
                                temperature), collapse = "\n")
    header <- gadgetfile_header("area")
    areafile <- paste(c(header, areasize_args, temperature_args), collapse = "\n")
    check_dir_exists(path)
    write(areafile, file = check_path(file))
}

#' @rdname write_gadget_file
write_gadget_file.gadget_fleets <- function(gf, file = "fleet", path = NULL) {
    check_dir_exists(path)
    fleets <-
        lapply(gf, function(x) {
            write_gadget_attributes(x, path = path)
            fleet <-
                lapply(seq_along(x), function(y, nms) {
                    if (nms[y] == "suitability") {
                        return(paste(c(nms[y], x[[y]]), collapse = "\n"))
                    } else {
                        return(paste(c(nms[y], x[[y]]), collapse = "\t"))
                    }
                }, nms = names(x))
            return(paste(fleet, collapse = "\n"))
        })
    header <- gadgetfile_header("fleet")
    comp_comment <- paste(c(";", comp_lab, ""), collapse = "\n")
    out <- paste(paste0(comp_comment, fleets), collapse = "\n")
    write_out <- paste(c(header, out), collapse = "\n")
    check_dir_exists(check_path("Modelfiles"))
    write(write_out, check_path(sprintf("Modelfiles/%s", file)))
}

#' @rdname write_gadget_file
write_gadget_file.gadget_stock <- function(gf, file = gf$stockname, path = NULL) {
    stock_args <- collapse_entries(gf)
    stockfile <- paste(names(gf), stock_args, sep = "\t", collapse = "\n")
    header <- gadgetfile_header("stock")
    out <- paste(c(header, stockfile), collapse = "\n")
    check_dir_exists(path)
    write_gadget_attributes(gf, path = path)
    write(out, check_path(file))
}

#' @rdname write_gadget_file
write_gadget_file.gadget_stocks <- function(gf, path = NULL) {
    lapply(gf, function(x) {
        write_gadget_file(x, file = check_path(x$stockname), path = path)
    })
}

#' @rdname write_gadget_file
write_gadget_file.gadget_params <- function(gf, file = "params.in", path = NULL) {
    df <- collapse_df(gf)
    header <- gadgetfile_header("params")
    col_names <- paste(c("switch", "value", "lower", "upper", "optimize"), collapse = "\t")
    out <- paste(c(header, col_names, df), collapse = "\n")
    write(out, file = check_path(file))
}

#' @rdname write_gadget_file
#' @inheritParams make_gadget_printfile
write_gadget_file.gadget_printfile <- function(gf, file = "printfile.fit", path = NULL,
                                               output_dir = "out", aggfile_dir = "print.aggfiles",
                                               fit_dir = NULL) {
    # check directory structure of fit_dir
    if (!is.null(fit_dir)) {
        check_dir_exists(check_path(fit_dir))
        printfile <- paste(c(fit_dir, file), collapse = "/")
        output_dir <- paste(c(fit_dir, output_dir), collapse = "/")
        aggfile_dir <- paste(c(fit_dir, aggfile_dir), collapse = "/")
    }
    null_list <- lapply(gf, write_aggfiles,
                        aggfile_dir = aggfile_dir, path = path, fit_dir = fit_dir)
    formatted_printfiles <- lapply(gf, format_printfile)
    header <- gadgetfile_header("printfile")
    likfile2print <- paste(c(header, formatted_printfiles), collapse = "\n;\n")
    check_dir_exists(check_path(output_dir))
    write(likfile2print, file = check_path(printfile))
}

#' Functions to write attributes of Gadget components to file
#'
#' In the make_gadget_* functions it is currently implemented that files written as arguments
#' to various Gadget components are saved as attributes with a meta-attribute of filename
#' corresponding to the appropriate path given in the argument (e.g. In stockfiles, if doesspawn = 1,
#' there is a spawnfile argument that specifies the path to the spawning information.
#' \code{\link{make_gadget_stockfile}} currently saves this spawning information as an attribute
#' named \code{spawning}. This attribute houses the necessary spawning information and has a
#' meta-attribute of filename that will determine where to write the file).
#'
#' @param gf The Gadget file component to be written. Should be of class \code{gadget_fleet},
#' \code{gadget_stocks}, but can alternately be a \code{data.frame}
#' @param env An environment to search for the object named \code{path}
#' @param path Optional. Character of the path of the directory to look in
#'
#' @return Nothing. Writes the appropriate Gadget file, aggfile or data file to a text file
#'
#' @name write_attr
write_gadget_attributes <- function(gf, path = NULL, env = parent.frame()) {
    UseMethod("write_gadget_attributes", gf)
}

#' @rdname write_attr
write_gadget_attributes.gadget_fleet <- function(gf, path = NULL, env = parent.frame()) {
    amount <- attr(gf, "amount")
    file_info <- get_attr_filename(amount)
    filename <- file_info$filename
    check_dir_exists(check_path(file_info$attr_dir))
    header <- datafile_header("Fleet amount")
    out <- paste(c(header,
                   "; -- Data --",
                   paste(c(";", names(amount)), collapse = "\t"),
                   collapse_df(amount)),
                 collapse = "\n")
    write(out, file = check_path(filename))
}

#' @rdname write_attr
write_gadget_attributes.gadget_stock <- function(gf, path = NULL, env = parent.frame()) {
    gf_attr <- attributes(gf)[-grep("^names|^class", names(attributes(gf)))]
    lapply(gf_attr, write_gadget_attributes, path = path, env = env)
}

#' @rdname write_attr
write_gadget_attributes.data.frame <- function(df, path = NULL, env = parent.frame()) {
    file_info <- get_attr_filename(df)
    filename <- file_info$filename
    check_dir_exists(check_path(file_info$attr_dir))
    header <- ifelse(grepl("Aggfiles", filename), aggfile_header(), datafile_header())
    out <- paste(c(header,
                   "; -- Data --",
                   paste(";", paste(names(df), collapse = "\t")),
                   collapse_df(df)),
                 collapse = "\n")
    write(out, file = check_path(filename))
}

#' @rdname write_attr
write_gadget_attributes.gadget_spawnfile <- function(gf, path = NULL, env = parent.frame()) {
    file_info <- get_attr_filename(gf)
    filename <- file_info$filename
    check_dir_exists(check_path(file_info$attr_dir))
    header <- gadgetfile_header("spawning")
    spawn_args <- paste(names(gf), collapse_entries(gf), sep = "\t", collapse = "\n")
    out <- paste(c(header,
                   spawn_args),
                 collapse = "\n")
    write(out, file = check_path(filename))
}

#' @rdname write_attr
#' @param obj The attribute to be checked for filename
get_attr_filename <- function(obj) {
    filename <- attr(obj, "filename")
    attr_dir <- split_(filename, "/")
    if (length(attr_dir) > 1) {
        attr_dir <- attr_dir[1]
    } else {attr_dir <- NULL}
    return(list(filename = filename, attr_dir = attr_dir))
}

#' Write aggfiles used in a printfile_component to files in a directory
#'
#' This function searches for aggfiles and prints them to the appropriate directory and filename
#'
#' @inheritParams make_aggfiles
#'
#' @return NULL. Writes aggregate files to the appropriate files in \code{aggfile_dir}
#'
#' @examples
#' path <- system.file(gad_mod_dir, package = "gadgetSim")
#' cod <- list(stocknames = "cod", printfile = "printfiles")
#' cod_stock <- update_printfile(stock, cod)
#' cod_stock_agg <- make_aggfiles(cod_stock, "print.aggfiles", path = path)
#' \dontrun{
#' write_aggfiles(cod_stock_agg, "print.aggfiles", path = path)
#' }
write_aggfiles <- function(printfile_comp, aggfile_dir, path = NULL, fit_dir = NULL) {
    if (any(isNULL_aggfiles(printfile_comp))) {
        missing_agg <- names(printfile_comp)[isNULL_aggfiles(printfile_comp)]
        stop(sprintf("Required argument %s is missing", missing_agg))
    } else if (!any(grepl("aggfile", names(printfile_comp)))) {
        return(printfile_comp)
    } else {
        check_dir_exists(check_path(aggfile_dir))
        if (!is.null(fit_dir)) {
            aggfile_dir_name <- split_(aggfile_dir, "/", ind = 2)
        } else {
            aggfile_dir_name <- aggfile_dir
        }
        agg_types <- grep("aggfile", names(printfile_comp), value = TRUE)
        null_list <-
            lapply(agg_types, function(x, path) {
                agg_test <- strsplit(printfile_comp[[x]], split = "/")[[1]][1]
                if (!(all.equal(agg_test, aggfile_dir_name))) {
                    stop(sprintf("The supplied aggfile_dir and directory prefix for
                                  %s are not the same"), printfile_comp[[x]])
                }
                if (!is.null(fit_dir)) {
                    write(attr(printfile_comp, x),
                          file = check_path(paste(fit_dir, printfile_comp[[x]], sep = "/")))
                } else {
                    write(attr(printfile_comp, x),
                          file = check_path(printfile_comp[[x]]))
                }
            }, path = path)
    }
}


#' Collapse objects and vectors so they are ready to be written to file
#'
#' These functions take lists, vectors or data.frames and collapse them using "\\t" or "\\n" depending
#' on if the object is a vector, list, or data.frame
#'
#' @param df The data.frame or Gadget file to collapse attributes on for \code{collapse_df}
#'
#' @return If \code{df} is a data.frame returns a character vector of the \code{df} pasted together
#' by "\\t" for rows and "\\n" for columns. If a Gadget file it will return the same Gadget file
#' with each element collapsed by "\\t" if the length is >1
#'
#' @name collapse
#'
#' @examples
#' time <- make_gadget_timefile(1985, 2015, "quarterly")
#' collapse_entries(time)
collapse_df <- function(df) {
    if (!is.data.frame(df)) {
        stop("This attribute must be a data.frame")
    }
    pasted_rows <-
        apply(df, 1, function(y) {
            paste(y, collapse = "\t")
        })
    return(paste(pasted_rows, collapse = "\n"))
}

#' @rdname collapse
#' @param gf The Gadget file to collapse entries on for \code{collapse_entries}
collapse_entries <- function(gf) {
    tmp <-
        lapply(gf, function(x) {
            if (is.data.frame(x)) {
                return(collapse_df(x))
            }
            if (length(x) > 1) {
                return(paste(x, collapse = "\t"))
            } else {
                return(x)
            }
        })
    return(tmp)
}

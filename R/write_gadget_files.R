# functions to write gadget files

#' Write Gadget components to file
#'
#' \code{write_gadget_file} is a generic function for the various Gadget
#' filetypes produced by gadgetSim. There are a number of S3 methods
#' corresponding to each Gadget filetype (see below). These functions write the
#' various classes of Gadget filetypes into a text file that can be understood
#' by Gadget. The appropriate components of each respective gadget file type
#' is also written to data files if that is appropriate, and the data in the
#' gadget file type is replaced by the path to that file.
#'
#' @param gf A list of class \code{gadget_main}, \code{gadget_time},
#' \code{gadget_area}, \code{gadget_stock}, \code{gadget_fleet},
#' \code{gadget_printfile}, or \code{gadget_params}
#' @param file Character. The name of the file to be written
#' @param path Optional. Character of the path of the directory to write
#' files to
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
#' @export
write_gadget_file.gadget_main <- function(gf, file = "main", path = NULL) {
    if (is.null(gf)) {
        gf <- gadget_main_default
        warning("No Gadget main file found. Writing the default to file ",
                file)
    } else if (!("gadget_main" %in% class(gf))) {
        stop("Class of file must contain gadget_main.")
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
#' @export
write_gadget_file.gadget_time <- function(gf, file = "time", path = NULL) {
    time_args <- paste(names(gf), collapse_entries(gf), sep = "\t")
    header <- gadgetfile_header("time")
    timefile <- paste(c(header, time_args), collapse = "\n")
    check_dir_exists(path)
    write(timefile, file = check_path(file))
}

#' @rdname write_gadget_file
#' @export
write_gadget_file.gadget_area <- function(gf, file = "Modelfiles/area",
                                          path = NULL) {
    gf <- collapse_entries(gf)
    areasize <- gf[c("areas", "size")]
    areasize_args <- paste(names(areasize), areasize, sep = "\t")
    temperature <- gf$temperature
    temperature_args <- paste(c("temperature", "; -- Data --",
                                paste(c(";", "year", "step",
                                        "area", "mean.temp"),
                                      collapse = "\t"),
                                temperature), collapse = "\n")
    header <- gadgetfile_header("area")
    areafile <- paste(c(header, areasize_args, temperature_args),
                      collapse = "\n")
    check_dir_exists(path, recursive = TRUE)
    check_dir_exists(check_path("Modelfiles"), recursive = TRUE)
    write(areafile, file = check_path(file))
}

#' @rdname write_gadget_file
#' @export
write_gadget_file.gadget_fleets <- function(gf, file = "Modelfiles/fleet",
                                            path = NULL) {
    check_dir_exists(path)
    fleets <-
        lapply(gf, function(x) {
            x <- write_gadget_attributes(x, path = path)
            fleet <-
                lapply(seq_along(x), function(y, nms) {
                    if (nms[y] == "suitability") {
                        suit <- x[[y]]
                        if (is.data.frame(suit)) {
                            if (!any(grepl("function", suit))) {
                                suit$fun_col <- "function"
                                suit <-
                                    suit[, c(1, ncol(suit),
                                             (2:(ncol(suit) - 1)))]
                            }
                            return(paste(c(nms[y], collapse_df(suit)),
                                         collapse = "\n"))
                        } else {
                            stop("Suitability in gadget_fleet should be",
                                 "entered as a data.frame")
                        }
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
    write(write_out, check_path(file))
}

#' @rdname write_gadget_file
#' @export
write_gadget_file.gadget_stock <- function(gf, file = gf$stockname,
                                           path = NULL) {
    gf <- format_stockfile(gf)
    gf <- write_gadget_attributes(gf, path = path)
    stock_args <- collapse_entries(gf)
    stockfile <- paste(names(gf), stock_args, sep = "\t", collapse = "\n")
    header <- gadgetfile_header("stock")
    out <- paste(c(header, stockfile), collapse = "\n")
    check_dir_exists(path)
    write(out, check_path(file))
}

#' @rdname write_gadget_file
#' @export
write_gadget_file.gadget_stocks <- function(gf, path = NULL) {
    lapply(gf, function(x) {
        write_gadget_file(x, file = check_path(x$stockname), path = path)
    })
}

#' @rdname write_gadget_file
#' @export
write_gadget_file.gadget_params <- function(gf, file = "params.in",
                                            path = NULL) {
    df <- collapse_df(gf)
    header <- gadgetfile_header("params")
    col_names <- paste(c("switch", "value", "lower", "upper", "optimize"),
                       collapse = "\t")
    out <- paste(c(header, col_names, df), collapse = "\n")
    write(out, file = check_path(file))
}

#' @rdname write_gadget_file
#' @export
#' @inheritParams make_gadget_printfile
write_gadget_file.gadget_printfile <- function(gf, file = "printfile.fit",
                                               path = NULL,
                                               output_dir = "out",
                                               aggfile_dir = "print.aggfiles",
                                               fit_dir = NULL) {
    # check directory structure of fit_dir
    if (!is.null(fit_dir)) {
        check_dir_exists(check_path(fit_dir))
        file <- paste(c(fit_dir, file), collapse = "/")
        output_dir <- paste(c(fit_dir, output_dir), collapse = "/")
        aggfile_dir <- paste(c(fit_dir, aggfile_dir), collapse = "/")
    }
    null_list <- lapply(gf, write_printfile_aggfiles,
                        aggfile_dir = aggfile_dir, path = path,
                        fit_dir = fit_dir)
    formatted_printfiles <- lapply(gf, format_printfile)
    header <- gadgetfile_header("printfile")
    printfile2print <- paste(c(header, formatted_printfiles),
                             collapse = "\n;\n")
    check_dir_exists(check_path(output_dir))
    write(printfile2print, file = check_path(file))
}

#' @rdname write_gadget_file
#' @export
write_gadget_file.gadget_likelihood <- function(gf, file = "likelihood",
                                                path = NULL) {
    gf <- write_likelihood_aggfiles(gf, path = path)
    gf <- write_likelihood_datafiles(gf, path = path)
    lik_comps <-
        lapply(gf, function(x) {
            names(x)[grep("^data$", names(x))] <- "datafile"
            tmp <- collapse_entries(x)
            out <- paste(paste(names(tmp), tmp, sep = "\t"), collapse = "\n")
            return(out)
        })
    header <- gadgetfile_header("likelihood")
    likfile <- paste(c(header, lik_comps),
                     collapse = paste0("\n;\n", comp_lab, "\n"))
    check_dir_exists(path)
    write(likfile, file = check_path(file))
}

#' Functions to write attributes of Gadget components to file
#'
#' These functions are used within the \code{\link{write_gadget_file}} methods
#' to write data from the gadget file type in R to an appropriate file in the
#' Gadget model. \code{write_gadget_attributes} is a generic function with
#' methods specific for various different types of gadget files.
#'
#' @param gf The Gadget file component to be written. Should be of class
#' \code{gadget_fleet},
#' \code{gadget_stocks}, but can alternately be a \code{data.frame}
#' @param env An environment to search for the object named \code{path}
#' @param path Optional. Character of the path of the directory to look in
#'
#' @return Nothing. Writes the appropriate Gadget file, aggfile or data file to
#' a text file
#' @export
#'
#' @name write_attr
write_gadget_attributes <- function(gf, path = NULL, env = parent.frame()) {
    UseMethod("write_gadget_attributes", gf)
}

#' @rdname write_attr
#' @export
write_gadget_attributes.gadget_fleet <- function(gf, path = NULL,
                                                 env = parent.frame()) {
    if (is.data.frame(gf$amount) | is.matrix(gf$amount)) {
        amount <- gf$amount
        fleet_name <- gf[[1]]
        filename <- sprintf("Data/fleet.%s.data", fleet_name)
    } else {
        stop("Amount in fleetfile must be of class data.frame")
    }
    file_info <- get_attr_filename(filename)
    filename <- file_info$filename
    check_dir_exists(check_path(file_info$attr_dir), recursive = TRUE)
    header <- datafile_header("Fleet amount")
    out <- paste(c(header,
                   "; -- Data --",
                   paste(c(";", names(amount)), collapse = "\t"),
                   collapse_df(amount)),
                 collapse = "\n")
    write(out, file = check_path(filename))
    gf$amount <- filename
    return(gf)
}

#' @rdname write_attr
#' @export
write_gadget_attributes.gadget_stock <- function(gf, path = NULL,
                                                 env = parent.frame()) {
    chk_comp <- function(comp_name) {
        if (gf[[comp_name]] == 1) {
            return(TRUE)
        } else {
            return(FALSE)
        }
    }
    if (check_names("^refweightfile$", gf)) {
        refweight_filename <-
            sprintf("Modelfiles/%s.refweightfile", gf$stockname)
        file_info <- get_attr_filename(refweight_filename)
        check_dir_exists(check_path(file_info$attr_dir), recursive = TRUE)
        write_gadget_attributes(gf$refweightfile,
                                path = check_path(refweight_filename))
        gf$refweightfile <- refweight_filename
    }
    if (check_names("^growthandeatlengths$", gf)) {
        grw_eat_len_filename <-
            sprintf("Aggfiles/%s.stock.len.agg", gf$stockname)
        file_info <- get_attr_filename(grw_eat_len_filename)
        check_dir_exists(check_path(file_info$attr_dir), recursive = TRUE)
        write_gadget_attributes(gf$growthandeatlengths,
                                path = check_path(grw_eat_len_filename))
        gf$growthandeatlengths <- grw_eat_len_filename
    }
    if (chk_comp("iseaten")) {
        if (!check_names("^preylengths$", gf)) {
            stop("If iseaten == 1, then you must specify data for preylengths")
        } else {
            if (is.data.frame(gf$preylengths)) {
                preylengths_filename <-
                    sprintf("Aggfiles/%s.stock.len.agg", gf$stockname)
                file_info <- get_attr_filename(preylengths_filename)
                check_dir_exists(check_path(file_info$attr_dir),
                                 recursive = TRUE)
                write_gadget_attributes(gf$preylengths,
                                        path = check_path(preylengths_filename))
                gf$preylengths <- preylengths_filename
            } else {
                stop("Stock preylengths should be a data.frame or a matrix")
            }
        }
    }
    if (check_names("^initialconditions$", gf)) {
        init_data_ind <- get_index("^doesmigrate$", names(gf)) - 1
        init_data <- gf[[init_data_ind]]
        init_data_type <- names(gf)[init_data_ind]
        filename <- sprintf("Modelfiles/%s.init.%s",
                            gf$stockname,
                            init_data_type)
        file_info <- get_attr_filename(filename)
        check_dir_exists(check_path(file_info$attr_dir), recursive = TRUE)
        write_gadget_attributes(gf[[init_data_ind]],
                                path = check_path(filename))
        gf[[init_data_ind]] <- filename
    }
    if (chk_comp("doesmigrate")) {
        next_comp_ind <- get_index("^doesmature$", names(gf))
        migration_type <- gsub("define", "", names(gf)[(next_comp_ind - 1)])
        yearstep_data <- gf$yearstepfile
        migration_data <- gf[paste0("define", migration_type)]
        yearstep_filename <- sprintf("Modelfiles/%s.migration.yearsteps",
                                     gf$stockname)
        migration_filename <- sprintf("Modelfiles/$s.migration.$s",
                                      gf$stockname, migration_type)
        write_gadget_attributes(migration_data,
                                path = check_path(migration_filename))
        write_gadget_attributes(yearstep_data,
                                path = check_path(yearstep_filename))
        gf$yearstepfile <- yearstep_filename
        gf[paste0("define", migration_type)] <- migration_filename
    }
    if (chk_comp("doesmature")) {
        maturity_filename <- sprintf("Modelfiles/%s.maturity", gf$stockname)
        maturity_data <- gf$maturityfile
        file_info <- get_attr_filename(maturity_filename)
        check_dir_exists(check_path(file_info$attr_dir), recursive = TRUE)
        write_gadget_attributes(maturity_data,
                                path = check_path(maturity_filename))
        gf$maturityfile <- gf$filename
    }
    if (chk_comp("doesrenew")) {
        data_ind <- get_index("^doesspawn$", names(gf)) - 1
        data_type <- names(gf)[data_ind]
        filename <- sprintf("Modelfiles/%s.renewal.%s",
                            gf$stockname,
                            data_type)
        file_info <- get_attr_filename(filename)
        check_dir_exists(check_path(file_info$attr_dir), recursive = TRUE)
        write_gadget_attributes(gf[[data_ind]],
                                path = check_path(filename))
        gf[[data_ind]] <- filename
    }
    if (chk_comp("doesspawn")) {
        spawn_info <- gf$spawnfile
        filename <- sprintf("Modelfiles/%s.spawnfile", gf$stockname)
        file_info <- get_attr_filename(filename)
        check_dir_exists(check_path(file_info$attr_dir), recursive = TRUE)
        write_gadget_attributes(spawn_info,
                                path = check_path(filename))
        gf$spawnfile <- filename
    }
    if (chk_comp("doesstray")) {
        straying_info <- gf$strayfile
        filename <- sprintf("Modelfiles/%s.strayfile", gf$stockname)
        file_info <- get_attr_filename(filename)
        check_dir_exists(check_path(file_info, recursive = TRUE))
        write_gadget_attributes(straying_info,
                                path = check_path(filename))
        gf$strayfile <- filename
    }
    return(gf)
}

#' @rdname write_attr
#' @export
write_gadget_attributes.data.frame <- function(gf, path = NULL) {
    header <- ifelse(grepl("Aggfiles", path),
                     aggfile_header(),
                     datafile_header())
    out <- paste(c(header,
                   "; -- Data --",
                   paste(";", paste(names(gf), collapse = "\t")),
                   collapse_df(gf)),
                 collapse = "\n")
    write(out, file = path)
}

#' @rdname write_attr
#' @export
write_gadget_attributes.list <- function(gf, path = NULL) {
    header <- aggfile_header()
    out <- paste(c(header, paste(c(names(gf), collapse_entries(gf)),
                                 collapse= "\t")),
                   collapse = "\n")
    write(out, file = path)
}


#' @rdname write_attr
#' @export
write_gadget_attributes.gadget_spawnfile <- function(gf, path = NULL) {
    header <- gadgetfile_header("spawning")
    spawn_args <- paste(names(gf), collapse_entries(gf), sep = "\t",
                        collapse = "\n")
    out <- paste(c(header,
                   spawn_args),
                 collapse = "\n")
    write(out, file = path)
}

#' @rdname write_attr
#' @export
write_gadget_attributes.gadget_strayfile <- function(gf, path = NULL) {
    header <- gadgetfile_header("straying")
    stray_args <- paste(names(gf), collapse_entries(gf), sep = "\t",
                        collapse = "\n")
    out <- paste(c(header, stray_args), collapse = "\n")
    write(out, file = path)
}

#' @rdname write_attr
#' @param obj The attribute to be checked for filename
get_attr_filename <- function(filename) {
    attr_dir <- split_(filename, "/")
    if (length(attr_dir) > 1) {
        attr_dir <- paste(attr_dir[-length(attr_dir)], collapse = "/")
    } else {attr_dir <- NULL}
    return(list(filename = filename, attr_dir = attr_dir))
}

#' Write aggfiles specific to certain Gadget components
#'
#' These functions are specific to Gadget likelihood and printfile components as
#' many of these aggfiles require particular attention to formatting. These
#' functions are strictly used within \code{\link{write_gadget_file}}
#'
#' @inheritParams make_aggfiles
#'
#' @return NULL. Writes aggregate files to the appropriate files in
#' \code{aggfile_dir}
#'
#' @name write_aggfiles
#'
#' @examples
#' path <- system.file(gad_mod_dir, package = "gadgetSim")
#' cod <- list(stocknames = "cod", printfile = "printfiles")
#' cod_stock <- update_printfile(stock, cod)
#' cod_stock_agg <- make_aggfiles(cod_stock, "print.aggfiles", path = path)
#' \dontrun{
#' write_aggfiles(cod_stock_agg, "print.aggfiles", path = path)
#' }
write_likelihood_aggfiles <- function(likelihood_file, path = NULL) {
    comps <-
        lapply(likelihood_file, function(x) {
            if (check_names("aggfile", x)) {
                aggfile_ind <- get_index("aggfile", names(x))
                null_list <-
                    lapply(aggfile_ind, function(y, nms, path) {
                        lik_name <- x$name
                        lik_type <- x$type
                        agg_data <- x[[y]]
                        agg_type <- gsub("aggfile", "", nms[y])
                        filename <-
                            lik_agg_filename(lik_type, lik_name,
                                             agg_type, path = path)
                        file_info <- get_attr_filename(filename)
                        check_dir_exists(check_path(file_info$attr_dir,
                                                    ignore_subdir = TRUE),
                                         recursive = TRUE)
                        write_gadget_attributes(agg_data,
                            path = check_path(filename, ignore_subdir = TRUE))
                        x[[y]] <<- filename
                    }, nms = names(x), path = path)
                return(x)
            } else {
                return(x)
            }
        })
    return(comps)
}

#' @rdname write_aggfiles
write_likelihood_datafiles <- function(likelihood_file, path = NULL) {
    comps <-
        lapply(likelihood_file, function(x, path) {
            if (check_names("data", x)) {
                datafile_ind <- get_index("data", names(x))
                lik_data <- x[[datafile_ind]]
                lik_name <- x$name
                lik_type <- x$type
                filename <-
                    switch(lik_type,
                           penalty = lik_data_filename("bounds.penaltyfile",
                                                       path = path),
                           catchdistribution = lik_data_filename(lik_type,
                                                                 lik_name,
                                                                 x$`function`,
                                                                 path = path),
                           surveyindices = lik_data_filename(lik_type,
                                                             lik_name,
                                                             x$sitype,
                                                             path = path),
                           lik_data_filename(lik_name, lik_type, x$`function`,
                                             path = path))
                file_info <- get_attr_filename(filename)
                check_dir_exists(check_path(file_info$attr_dir,
                                            ignore_subdir = TRUE),
                                 recursive = TRUE)
                write_gadget_attributes(lik_data,
                    path = check_path(filename, ignore_subdir = TRUE))
                x[[datafile_ind]] <- filename
                return(x)
            } else {
                return(x)
            }
        }, path = path)
    return(comps)
}

#' @rdname write_aggfiles
write_printfile_aggfiles <- function(comp, path = NULL,
                                     aggfile_dir, fit_dir = NULL) {
    if (any(isNULL_aggfiles(comp))) {
        missing_agg <- names(comp)[isNULL_aggfiles(comp)]
        stop(sprintf("Required argument %s is missing", missing_agg))
    } else if (!any(grepl("aggfile", names(comp)))) {
        return(comp)
    } else {
        check_dir_exists(check_path(aggfile_dir))
        if (!is.null(fit_dir)) {
            aggfile_dir_name <- split_(aggfile_dir, "/", ind = 2)
        } else {
            aggfile_dir_name <- aggfile_dir
        }
        agg_types <- grep("aggfile", names(comp), value = TRUE)
        null_list <-
            lapply(agg_types, function(x, path) {
                agg_test <- strsplit(comp[[x]], split = "/")[[1]][1]
                if (!(all.equal(agg_test, aggfile_dir_name))) {
                    stop("The supplied aggfile_dir and directory prefix for ",
                         comp[[x]], "are not the same")
                }
                if (!is.null(fit_dir)) {
                    write(attr(comp, x),
                          file = check_path(paste(fit_dir, comp[[x]],
                                                  sep = "/")))
                } else {
                    write(attr(comp, x),
                          file = check_path(comp[[x]]))
                }
            }, path = path)
    }
}

#' @rdname likelihood_helper_funs
lik_agg_filename <- function(..., aggfile_dir = "Aggfiles", path = NULL) {
    dots <- dots2list(...)
    if (is_gadget_sub_dir(path)) {
        return(paste(check_path(aggfile_dir, sub_dir_only = TRUE),
                     paste(c(dots, c(list("agg"))), collapse = "."),
                     sep = "/"))
    } else {
        return(paste(aggfile_dir,
                     paste(c(dots, c(list("agg"))), collapse = "."),
                     sep = "/"))
    }
}

#' @rdname likelihood_helper_funs
lik_data_filename <- function(..., datafile_dir = "Data", path = NULL) {
    dots <- dots2list(...)
    if (is_gadget_sub_dir(path)) {
        return(paste(check_path(datafile_dir, sub_dir_only = TRUE),
                     paste(dots, collapse = "."),
                     sep = "/"))
    } else {
        return(paste(datafile_dir,
               paste(dots, collapse = "."), sep = "/"))
    }
}


#' Collapse objects and vectors so they are ready to be written to file
#'
#' These functions take lists, vectors or data.frames and collapse them using
#' "\\t" or "\\n" depending on if the object is a vector, list, or data.frame
#'
#' @param df The data.frame or Gadget file to collapse attributes on for
#' \code{collapse_df}
#'
#' @return If \code{df} is a data.frame returns a character vector of the
#' \code{df} pasted together by "\\t" for rows and "\\n" for columns. If a
#' Gadget file it will return the same Gadget file with each element collapsed
#' by "\\t" if the length is >1
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
    return(paste(do.call("paste", c(as.list(df), list(sep = "\t"))),
                 collapse = "\n"))
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

#' Write an object of class \code{gadget_model} to the respective files
#'
#' \code{write_gadget_model} takes an object of class "gadget_model" as returned
#' by \code{\link{read_gadget_model}}
#'
#' @param mod_obj A list of class \code{gadget_model}
#' @inheritParams write_gadget_file
#'
#' @return Nothing. Writes the respective Gadget filetype to a text file
#' @export
#'
#' @examples
#' \dontrun{
#' path <- system.file(gad_mod_dir, package = "gadgetSim")
#' gad_mod <- read_gadget_model(path = path)
#' write_gadget_model(gad_mod, path = "different_model")
#' }
write_gadget_model <- function(mod_obj, path = NULL) {
    if (!("gadget_model" %in% class(mod_obj))) {
        stop("The model object provided must be a list of class gadget_model")
    }
    if (!check_names("main", mod_obj)) {
        stop("You have not provided a main file for this model")
    } else {
        null_list <-
            lapply(mod_obj, function(x) {
                write_gadget_file(x, path = path)
            })
    }
}

#' Write out a Gadget sub-directory and return the path to it
#'
#' This function will create a new directory by the name of \code{sub_dir}
#' within the directory specified by \code{path}. The address to the new
#' sub-directory is returned.
#'
#' \code{gadget_sub_dir} is essentially just an easy way to create a
#' sub-directory of the chosen Gadget model for writing model files to.
#' It could be used for writing replicant models files.
#'
#' @param sub_dir Character. Path to the sub-directory you wish to create
#' @param path Optional. Character string of the path of the Gadget model you
#' are working in
#'
#' @return A character string of the path to the sub-directory specified in
#' \code{sub_dir}
#' @export
#'
#' @name gadget_sub_dir
#'
#' @examples
#' \dontrun{
#' path <- system.file(gad_mod_dir, package = "gadgetSim")
#' gadget_sub_dir("test_model", path = path)
#' }
#' gadget_sub_dir()
gadget_sub_dir <- function(sub_dir, path = NULL) {
    check_dir_exists(check_path(sub_dir), recursive = TRUE)
    return(structure(list(
        path = path,
        sub_dir = sub_dir
    ), class = c("gadget_sub_dir", "list")))
}

#' @rdname gadget_sub_dir
#' @export
is_gadget_sub_dir <- function(obj) {
    if ("gadget_sub_dir" %in% class(obj)) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}

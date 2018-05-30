# functions to check different parts of gadget stockfile and plug in appropriate values
# these functions are solely meant for use within make_gadget_stockfile

#' Check various entries when making a stockfile using make_gadget_stockfile()
#'
#' These are non-exported functions that are only used within \code{\link{make_gadget_stockfile}}.
#' Each checks a different aspect of the required arguments to a Gadget stockfile and either
#' returns a default value if appropriate or stops function execution if a necessary piece is
#' missing and a straightforward default does not exists
#'
#' @param stockfile A list that contains stockfile information such as
#' \code{gadget_stockfile_default}, see source code for \code{\link{make_gadget_stockfile}}
#'
#' @return A list that is a subset of \code{stockfile} containing the appropriate names for each
#' category being checked
#'
#' @name check_stockfile_funs
#'
#' @examples
#' see source code for \code{\link{make_gadget_stockfile}}
check_stock_info <- function(dots) {
    stock <- dots$stock
    if (!check_names("^stockname", stock)) {
        stop("You must specify a stockname")
    }
    if (any(is_list_element_null(list(stock$minlength, stock$maxlength, stock$dl)))) {
        stop("You must specify length information (i.e. minlength, maxlength, dl)")
    } else if (any(is_list_element_null(list(stock$minage, stock$maxage)))) {
        stop("You must specify age information (i.e. minage, maxage")
    }
    return(stock)
}

#' @rdname check_stockfile_funs
check_stock_refweightfile <- function(dots) {
    if (check_names("^refweightfile", dots)) {
        if (class(dots$refweightfile) == "data.frame") {
            filename <- sprintf("Modelfiles/%s.refweightfile", dots$stock$stockname)
            refwgt <-
                structure(list(
                    refweightfile = filename),
                    refweightfile = structure(dots$refweightfile, filename = filename))
        } else {
            stop("You must either provide a data.frame for refweightfile or rely on the default")
        }
    } else {
        if (check_names("^growth", dots)) {
            if (dots$growth$growthfunction == "lengthvbsimple") {
                stock <- dots$stock
                reflength <- seq(stock$minage, stock$maxlength, stock$dl)
                rfwt_p <- as.numeric(dots$growth$growthparameters[3:4])
                ref_wts <- rfwt_p[1] * reflength ^ rfwt_p[2]
                filename <- sprintf("Modelfiles/%s.refweightfile", stock$stockname)
                refwgt <-
                    structure(list(
                        refweightfile = filename),
                        refweightfile =
                            structure(data.frame(length = reflength, weight = ref_wts),
                                      filename = filename))
            } else {
                refwgt <- list(refweightfile = "")
                warning("Sorry, gadgetSim is not smart enough to creat a refweightfile for
                         growth functions other than lenghtvbsimple yet.
                         You'll have to specify it manually")
            }
        } else {
            refwgt <- list(refweightfile = "")
        }
    }
    return(refwgt)
}

#' @rdname check_stockfile_funs
check_stock_grw_eat_len <- function(dots,
                                    lenaggfile = get("lenaggfile", env = parent.frame())) {
    if (!check_names("^growthandeatlengths", dots)) {
        filename <- lenaggfile[1]
        grw_eat_len <-
            structure(list(
                growthandeatlengths = filename),
                lenaggfile = structure(attr(lenaggfile, "lenaggfile"), filename = filename))
    } else {
        if (class(dots$growthandeatlengths) != "data.frame") {
            stop("You must either provide a data.frame specifying the aggfile for
                  growth and eat lengths or rely on the default")
        } else {
            filename <- sprintf("Aggfiles/%s.stock.len.agg", dots$stock$stockname)
            grw_eat_len <-
                structure(list(
                    growthandeatlengths = filename),
                    lenaggfile = structure(dots$growthandeatlengths, filename = filename))
        }
    }
    return(grw_eat_len)
}

#' @rdname check_stockfile_funs
check_stock_growth <- function(dots) {
    if (!check_names("^growth", dots)) {
        growth <- list(doesgrow = 0)
    } else {
        growth <-
            c(list(doesgrow = 1), dots$growth)
        if (!check_names("^beta", dots$growth)) {
            growth$beta <- sprintf("(* #%1$s.bbin.mult #%1$s.bbin)", dots$stock$stockname)
        }
        if (!check_names("^maxlengthgroupgrowth", dots$growth)) {
            growth$maxlengthgroupgrowth <- 15
        }
    }
    return(growth)
}

#' @rdname check_stockfile_funs
check_stock_m <- function(dots) {
    if (!check_names("^naturalmortality", dots)) {
        m <- list(naturalmortality = rep(0.2, times = ((dots$stock$maxage - dots$stock$minage) + 1)))
        message("You did not specify a natural mortality...setting all ages to 0.2")
        return(m)
    } else {
        m <- list(naturalmortality = dots$naturalmortality)
    }
    return(m)
}


#' @rdname check_stockfile_funs
check_stock_iseaten <- function(dots,
                                lenaggfile = get("lenaggfile", env = parent.frame())) {
    if (check_names("^iseaten", dots)) {
        if (all(c("preylengths", "energycontent") %in% names(dots$iseaten))) {
            filename <- sprintf("Aggfiles/%s.preylengths", dots$stock$stockname)
            iseaten <-
                structure(list(
                    iseaten = 1,
                    preylengths = filename,
                    energycontent = dots$iseaten$energycontent),
                    preylengths = structure(dots$iseaten$preylengths, filename = filename))
        } else if (check_names("^energycontent", dots$iseaten) &
                   !check_names("^preylengths", dots$iseaten)) {
            filename <- lenaggfile[1]
            iseaten <-
                structure(list(
                    iseaten = 1,
                    preylengths = filename,
                    energycontent = dots$iseaten$energycontent),
                    lenaggfile = structure(attr(lenaggfile, "lenaggfile"), filename = filename))
        } else if (check_names("^preylengths", dots$iseaten) &
                   !check_names("^energycontent", dots$iseaten)) {
            filename <- sprintf("Aggfiles/%s.preylengths", dots$stock$stockname)
            iseaten <-
                structure(list(
                    iseaten = 1,
                    preylengths = filename,
                    energycontent = 1),
                    preylengths = structure(dots$iseaten$preylengths, filename = filename))
        } else {
            filename <- lenaggfile[1]
            iseaten <-
                structure(list(
                    iseaten = 1,
                    preylengths = filename,
                    energycontent = 1),
                    lenaggfile = structure(attr(lenaggfile, "lenaggfile"), filename = filename))
        }
    } else {
        iseaten <- list(iseaten = 0)
    }
    return(iseaten)
}

#' @rdname check_stockfile_funs
check_stock_predator <- function(dots) {
    if (check_names("^doeseat", dots)) {
        req_names <- c("suitability", "preference", "maxconsumption", "halffeedingvalue")
        if (!all(req_names %in%
                 names(dots$doeseat))) {
            stop("You missed the following required items in doeseat", "\n",
                  paste(paste(" * ",
                              req_names[!(req_names %in% names(dots$doeseat))]), collapse = "\n"))
        }
        predator <-
            c(list(doeseat = 1), dots$doeseat)
    } else {
        predator <- list(doeseat = 0)
    }
    return(predator)
}

#' @rdname check_stockfile_funs
check_stock_initcond <- function(dots) {
    if (check_names("^initialconditions|^initcond", dots)) {
        ic_data <- dots[[grep("^initialconditions|^initcond", names(dots))]]
        ic_file_types <- c("normalparamfile", "normalcondfile", "numberfile")
        if ((is.list(ic_data) & length(ic_data) == 1) | (is.data.frame(ic_data))) {
            if (is.data.frame(ic_data)) {
                ic_file_type <- class(ic_data)[1]
                dat <- ic_data
            } else {
                ic_file_type <- class(ic_data[[1]])[1]
                dat <- ic_data[[1]]
            }
            if (!(ic_file_type %in% ic_file_types)) {
                stop("Initial conditions must of one of the following classes", "\n",
                     paste(paste(" * ", ic_file_types), collapse = "\n"), "\n",
                     "see ?stock_distribution_funs")
            }
            initcond <-
                list(initialconditions = "", minage = dots$stock$minage, maxage = dots$stock$maxage,
                     minlength = dots$stock$minlength, maxlength = dots$stock$maxlength,
                     dl = dots$stock$dl)
            filename <- sprintf("Modelfiles/%s.init.%s", dots$stock$stockname, ic_file_type)
            initcond[[ic_file_type]] <- filename
            attr(initcond, ic_file_type) <- structure(dat, filename = filename)
        } else {
            req_args <- c("minage", "maxage", "minlength", "maxlength", "dl")
            if (!all(req_args %in% names(ic_data))) {
                stop("You entered some, but not all initialconditions information")
            } else {
                initcond <-
                    list(initialconditions = "", minage = ic_data$minage, maxage = ic_data$maxage,
                         minlength = ic_data$minlength, maxlength = ic_data$maxlength,
                         dl = ic_data$dl,
                         sdev = ifelse(check_names("sdev", ic_data),
                                       ic_data$sdev, NULL))
                ic_data <- ic_data[!(names(ic_data) %in% c("initialconditions", req_args, "sdev"))]
                ic_file_type <- class(ic_data[[1]])[1]
                filename <- sprintf("Modelfiles/%s.init.%s", dots$stock$stockname, ic_file_type)
                initcond[[ic_file_type]] <- filename
                attr(initcond, ic_file_type) <- structure(ic_data[[1]], filename = filename)
            }
        }
        return(initcond)
    } else {
        stop("You have not specified initial conditions for the stock")
    }
}

#' @rdname check_stockfile_funs
check_stock_migration <- function(dots) {
    if (check_names("^migration", dots)) {
        if ("migration_matrix" %in% class(dots$migration)) {
            migration <-
                list(doesmigrate = 1,
                     yearstepfile = sprintf("Modelfiles/%s.migration.yearstepfile",
                                            dots$stock$stockname),
                     definematrices = sprintf("Modelfiles/%s.migration.matrices",
                                              dots$stock$stockname))
            attr(migration, "migrationmatrix") <- format_migration(dots$migration)
        } else if ("migration_ratios" %in% class(dots$migration)) {
            migration <-
                list(doesmigrate = 1,
                     yearstepfile = sprintf("Modelfiles/%s.migration.yearstepfile",
                                            dots$stock$stockname),
                     defineratios = sprintf("Modelfiles/%s.migration.ratios",
                                            dots$stock$stockname))
            attr(migration, "migrationratios") <- format_migration(dots$migration)
        } else {
            stop("Migration information must be a data.frame of one of the following classes", "\n",
                 paste(paste(" * ", c("migration_matrix", "migration_ratios"), collapse = "\n")),
                 "\n", "see ?migration_funs")
        }
    } else {
        migration <- list(doesmigrate = 0)
    }
    return(migration)
}

#' @rdname check_stockfile_funs
check_stock_maturity <- function(dots) {
    if (check_names("^maturity", dots)) {
        if (!check_names("^maturityfunction", dots$maturity)) {
            stop("You must enter a maturity function with appropriate parameters for the stock")
        }
        filename <- sprintf("Modelfiles/%s.maturityfile", dots$stock$stockname)
        maturity <-
            list(doesmature = 1,
                 maturityfunction = dots$maturity$maturityfunction,
                 maturityfile = filename)
        attr(maturity, "maturityfile") <-
            structrure(dots$maturity[grep("^maturityfunction", names(dots$maturity), invert = TRUE)],
                       filename = filename)
    } else {
        maturity <- list(doesmature = 0)
    }
    return(maturity)
}

#' @rdname check_stockfile_funs
check_stock_movement <- function(dots) {
    if (check_names("^movement|^transition", dots)) {
        movement <-
            c(list(doesmove = 1),
                   dots[grep("^movement|^transition", names(dots))])
    } else {
        movement <- list(doesmove = 0)
    }
    return(movement)
}

#' @rdname check_stockfile_funs
check_stock_renewal <- function(dots) {
    if (check_names("^renewal|^recruitment", dots)) {
        rec_info <- dots[grep("^renewal|^recruitment", names(dots))]
        if (length(rec_info) == 1) {
            rec_class <- class(rec_info[[1]])[1]
            filename <- sprintf("Modelfiles/%s.rec.%s", dots$stock$stockname, rec_class)
            renewal <-
                list(doesrenew = 1,
                     minlength = dots$stock$minlength,
                     maxlength = dots$stock$maxlength,
                     dl = dots$stock$dl)
            renewal[rec_class] <- filename
            attr(renewal, rec_class) <- structure(rec_info[[1]], filename = filename)
        } else {
            rec_data <- rec_info[grep("^normalparamfile|^normalcondfile|^numberfile",
                                      names(rec_info))]
            rec_class <- class(rec_data[[1]])[1]
            renewal <-
                list(doesrenew = 1,
                     minlength = rec_info$minlength,
                     maxlength = rec_info$maxlength,
                     dl = rec_info$dl)
            filename <- sprintf("Modelfiles/%s.rec.%s", dots$stock$stockname, rec_class)
            renewal[rec_class] <- filename
            attr(renewal, rec_class) <- structure(rec_data[[1]], filename = filename)
        }
    } else {
        renewal <- list(doesrenew = 0)
    }
    return(renewal)
}

#' @rdname check_stockfile_funs
check_stock_spawning <- function(dots) {
    if (check_names("^spawning", dots)) {
        if (!("gadget_spawnfile" %in% class(dots$spawning))) {
            stop("You must enter a spawnfile of class gadget_spawnfile", "\n",
                 "see ?make_gadget_spawnfile")
        } else {
            filename <- sprintf("Modelfiles/%s.spawnfile", dots$stock$stockname)
            spawning <-
                structure(list(
                    doesspawn = 1,
                    spawnfile = filename
                ), spawnfile = structure(dots$spawning, filename = filename))
        }
    } else {
        spawning <- list(doesspawn = 0)
    }
    return(spawning)
}

#' @rdname check_stockfile_funs
check_stock_straying <- function(dots) {
    if (check_names("^straying", dots)) {
        filename <- sprintf("Modelfiles/%s.strayfile", dots$stock$stockname)
        straying <-
            structure(list(
                doesstray = 1,
                strayfile = filename
            ))
        attr(straying, "strayfile") <- structure(dots$straying, filename = filename)
    } else {
        straying <- list(doesstray = 0)
    }
    return(straying)
}


#' @rdname check_stockfile_funs
make_stock_lenaggfile <- function(dots) {
    stock <- dots$stock
    reflength <- seq(stock$minlength, stock$maxlength, by = stock$dl)
    lenaggfile <- sprintf("Aggfiles/%s.stock.len.agg", stock$stockname)
    attr(lenaggfile, "lenaggfile") <-
        data.frame(name = sprintf("len%s", reflength[2:length(reflength)]),
                   lower = reflength[-length(reflength)],
                   upper = reflength[-1],
                   stringsAsFactors = FALSE)
    return(lenaggfile)
}

#' Negates a logical test of the presence of a name in stockfile
#'
#' This function checks to see if a named element exists in a list and returns TRUE if it does not.
#'
#' @param name Character. The name to be checked
#' @param sf Named object. Defaults to stockfile as this is used throughout the
#' \code{\link{check_stockfile_funs}}
#'
#' @return Logical. TRUE if name is not present. FALSE if it is
#'
#' @examples
#' stockfile <- list(stockname = "cod", minage = 1, maxage = 10, minlength = 1, maxlength = 100)
#' check_sf_names("^livesonareas")
#' check_sf_names("^stockname")
check_sf_names <- function(name, sf = get("stockfile", env = parent.frame())) {
    return(!any(grepl(name, names(sf))))
}

#' Functions to produce initial conditions or renewal distribution data
#'
#' These functions will produce a data.frame of class normalcondfile, normalparamfile, or numberfile
#'
#' @param ... Arguments necessary for a normalcondfile, a normalparamfile, or a numberfile. See
#' initial conditions and renewal in the Gadget User Guide
#'
#' @return A data.frame of class normalcondfile, normalparamfile, or numberfile
#' @export
#'
#' @name stock_distribution_funs
#'
#' @examples
#' normalparamfile(year = 1:10,
#'                 area = 1,
#'                 age.factor = 10,
#'                 area.factor = 10,
#'                 mean = vb_formula("cod", 1:10),
#'                 stddev = 1:10,
#'                 alpha = 0.0001,
#'                 beta = 3)
normalcondfile <- function(...) {
    df <- as.data.frame(dots2list(...))
    class(df) <- c("normalcondfile", "data.frame")
    return(df)
}

#' @rdname stock_distribution_funs
normalparamfile <- function(...) {
    df <- as.data.frame(dots2list(...))
    class(df) <- c("normalparamfile", "data.frame")
    return(df)
}

#' @rdname stock_distribution_funs
numberfile <- function(...) {
    df <- as.data.frame(dots2list(...))
    class(df) <- c("numberfile", "data.frame")
    return(df)
}

migration_df <- function(years, steps, areas, prop) {
    if (!is.numeric(prop)) {
        prop <- tryCatch(to_gadget_formula(prop),
                         error = function(e) {
                             return(prop)
                         })
    }
    mig_df <- expand.grid(year = years, step = steps,
                          from = areas, to = rev(areas),
                          stringsAsFactors = FALSE)
    mig_df <- mig_df[order(mig_df$year, mig_df$step, mig_df$from, mig_df$to), ]
    rownames(mig_df) <- 1:nrow(mig_df)
    mig_df$prop <- prop
    return(mig_df)
}


#' Helper functions to create and format migration matrices
#'
#' These functions create and format migration information for a Gadget stockfile. The
#' \code{migration_*} functions create a data.frame with class of the same name as the function
#' for use in \code{\link{check_stockfile_migration}}. The returned data.frame can then be read into
#' \code{format_migration} which will return a list of the correct structure to be written to file.
#' These functions will be most useful when migration proportions are not known and are specified as
#' a switch in the Gadget model
#'
#' @param years Numeric vector of years to specify migration proportions for
#' @param steps Numeric vector of steps to specify migration proportions for
#' @param areas Numeric vector of areas in the Gadget model
#' @param prop Vector of proportions to use. Can be numeric or character.
#' Length must be the same as \eqn{years * steps * areas^2} or a factor of such (or 1)
#'
#' @return \code{migration_*} functions return a data.frame with class the same as the function name.
#' \code{format_migration_matrix} returns a list with each element a matrix or data.frame of the
#' migration ratios to be used
#'
#' @export
#'
#' @name migration_funs
#'
#' @examples
#' years <- 1:2
#' steps <- 1:2
#' areas <- 1:2
#' ysa_combo <- c(paste(years, steps, sep = "."), paste(rev(years), steps, sep = "."))
#' prop <- sort(rep(paste("#cod.migration", ysa_combo, sep = "."), 4))
#' migration_matrix(years, steps, areas, prop)
migration_matrix <- function(years, steps, areas, prop) {
    mig_df <- migration_df(years, steps, areas, prop)
    class(mig_df) <- c("migration_matrix", "data.frame")
    return(mig_df)
}

#' @rdname migration_funs
migration_ratios <- function(years, steps, areas, prop) {
    mig_df <- migration_df(years, steps, areas, prop)
    class(mig_df) <- c("migration_ratios", "data.frame")
    return(mig_df)
}

#' @rdname migration_funs
format_migration <- function(mig_matrix) {
    UseMethod("format_migration")
}

#' @rdname migration_funs
#' @param mig_matrix A \code{data.frame} of class "migration_matrix" as produced by
#' \code{migration_matrix}
format_migration.migration_matrix <- function(mig_matrix) {
    if (!("migration_matrix" %in% class(mig_matrix))) {
        stop("You must use a data.frame of class migration_matrix, see ?migration_matrix")
    }
    years <- unique(mig_matrix$year)
    steps <- unique(mig_matrix$step)
    mm_list <-
        lapply(years, function(x) {
            lapply(steps, function(y) {
                tmp <- subset(mig_matrix, year == x & step == y)
                mm <- matrix(tmp$prop, nrow = sqrt(nrow(tmp)))
                if (!(all(colSums(mm) == 1))) {
                    stop(sprintf("Migration ratios at year %s and step %s do not equal 1", x, y))
                }
                attr(mm, "name") <- sprintf("matrix%s.%s", x, y)
                return(mm)
            })
        })
    return(mm_list)
}

#' @rdname migration_funs
#' @param A \code{data.frame} of class "migration_ratios" as produced by \code{migration_ratios}
format_migration.migration_ratios <- function(mig_ratios) {
    if (!("migration_ratios" %in% class(mig_ratios))) {
        stop("You must use a data.frame of class migration_ratios, see ?migration_ratios")
    }
    years <- unique(mig_ratios$year)
    steps <- unique(mig_ratios$step)
    mm_list <-
        lapply(years, function(x) {
            lapply(steps, function(y) {
                tmp <- subset(mig_ratios, year == x & step == y)
                mm <- matrix(tmp$prop, nrow = sqrt(nrow(tmp)))
                if (!(all(colSums(mm) == 1))) {
                    stop(sprintf("Migration ratios at year %s and step %s do not equal 1", x, y))
                }
                mm <- as.matrix(subset(mm, from != to, select = from:prop))
                attr(mm, "name") <- sprintf("matrix%s.%s", x, y)
                return(mm)
            })
        })
    return(mm_list)
}

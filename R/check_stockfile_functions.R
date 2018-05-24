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
            refwgt <-
                structure(list(
                    refweightfile = sprintf("Modelfiles/%s.refweightfile", dots$stock$stockname)),
                    refweightfile = dots$refweightfile)
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
                refwgt <-
                    structure(list(
                        refweightfile = sprintf("Modelfiles/%s.refweightfile", stock$stockname)),
                        refweightfile = data.frame(length = reflength, weight = ref_wts))
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
        grw_eat_len <-
            structure(list(
                growthandeatlengths = lenaggfile[1]),
                lenaggfile = attr(lenaggfile, "lenaggfile"))
    } else {
        if (class(dots$growthandeatlengths) != "data.frame") {
            stop("You must either provide a data.frame specifying the aggfile for
                  growth and eat lengths or rely on the default")
        } else {
            grw_eat_len <-
                structure(list(
                    growthandeatlengths = sprintf("Aggfiles/%s.stock.len.agg", dots$stock$stockname)),
                    lenaggfile = dots$growthandeatlengths)
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
            growth$beta <- sprintf("(* %1$s.bbin.mult %1$s.bbin)", dots$stock$stockname)
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
    } else {dots$naturalmortality}
}


#' @rdname check_stockfile_funs
check_stock_iseaten <- function(dots,
                                lenaggfile = get("lenaggfile", env = parent.frame())) {
    if (check_names("^iseaten", dots)) {
        if (all(c("preylengths", "energycontent") %in% names(dots$iseaten))) {
            iseaten <-
                structure(list(
                    iseaten = 1,
                    preylengths = sprintf("Aggfiles/%s.preylengths", dots$stock$stockname),
                    energycontent = dots$iseaten$energycontent),
                    preylengths = dots$iseaten$preylengths)
        } else if (check_names("^energycontent", dots$iseaten) &
                   !check_names("^preylengths", dots$iseaten)) {
            iseaten <-
                structure(list(
                    iseaten = 1,
                    preylengths = lenaggfile[1],
                    energycontent = dots$iseaten$energycontent),
                    lenaggfile = attr(lenaggfile, "lenaggfile"))
        } else if (check_names("^preylengths", dots$iseaten) &
                   !check_names("^energycontent", dots$iseaten)) {
            iseaten <-
                structure(list(
                    iseaten = 1,
                    preylengths = sprintf("Aggfiles/%s.preylengths", dots$stock$stockname),
                    energycontent = 1),
                    preylengths = dots$iseaten$preylengths)
        } else {
            iseaten <-
                structure(list(
                    iseaten = 1,
                    preylengths = lenaggfile[1],
                    energycontent = 1),
                    lenaggfile = attr(lenaggfile, "lenaggfile"))
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
            stop(cat("You missed the following required items in doeseat",
                     paste(paste(" * ",
                                 req_names[!(req_names %in% names(dots$doeseat))]), collapse = "\n"),
                     sep = "\n"))
        }
        predator <-
            c(list(doeseat = 1), dots$doeseat)
    } else {
        predator <- list(doeseat = 0)
    }
    return(predator)
}

#' @rdname check_stockfile_funs
check_stock_initcond <- function(stockfile) {
    if (!is.null(stockfile)) {
        initcond_ind <- grep("^initialconditions", names(stockfile))
        doesmigrate_ind <- grep("^doesmigrate", names(stockfile))
        if ((doesmigrate_ind - initcond_ind) == 1) {
            ic_class <- class(stockfile$initialconditions)
            if ("data.frame" %in% ic_class) {
                initcond <-
                    list(minage = stockfile$minage, maxage = stockfile$maxage,
                         minlength = stockfile$minlength, maxlength = stockfile$maxlength,
                         dl = stockfile$dl)
                if (check_sf_names("^sdev")) {
                    initcond$sdev <- stockfile$sdev
                }
                if (length(ic_class) > 1) {
                    initcond[ic_class[1]] <-
                        sprintf("Modelfiles/%s.init.%s", stockfile$stockname, ic_class[1])
                    attr(initcond, paste("init", ic_class[1], sep = ".")) <-
                        stockfile$initialconditions
                } else {
                    stop(cat("Initial conditions data must be of one of the following classes",
                             paste(paste0(" * ",
                                          c("normalcondfile", "normalparamfile", "numberfile")),
                                   sep = "\n"),
                             "see ?stock_distribution_funs",
                             sep = "\n"))
                }
            } else {
                stop("You have not entered useful information for initial conditions")
            }
        } else {
            initcond <- as.list(stockfile[initcond_ind:(doesmigrate_ind) - 1])
        }
    } else {
        stop("You have not entered any information for stockfile initial conditions")
    }
    return(initcond)
}

#' @rdname check_stockfile_funs
check_stock_migration <- function(stockfile) {
    if (stockfile$doesmigrate == 1) {
        if ("definematrices" %in% names(stockfile)) {
            migration <-
                list(doesmigrate = 1,
                     yearstepfile = sprintf("Modelfiles/%s.migration.yearstepfile",
                                            stockfile$stockname))
            if ("migration_matrix" %in% class(stockfile$definematrices)) {
                migration$definematrices <-
                    sprintf("Modelfiles/%s.migration.matrices", stockfile$stockname)
                attr(migration, "migrationmatrix") <- format_migration(stockfile$definematrices)
            } else if ("migration_ratios" %in% class(stockfile$definematrices)) {
                migration$defineratios <-
                    sprintf("Modelfiles/%s.migration.ratios", stockfile$stockname)
                attr(migration, "migrationratios") <- format_migration(stockfile$definematrices)
            }
        } else {
            stop("You set doesmigrate to 1, but didn't any migration information")
        }
    }
    return(migration)
}

#' @rdname check_stockfile_funs
check_stock_maturity <- function(stockfile) {
    if (stockfile$doesmature == 1) {
        maturity <-
            list(doesmature = 1)
        if (check_sf_names("^maturityfunction")) {
            stop("You must specify a maturity function")
        } else if (check_sf_names("^maturityfile")) {
            stop("You must specify appropriate parameters for the maturity function")
        } else {
            maturity$maturityfunction <- stockfile$maturityfunction
            maturity$maturityfile <- sprintf("%s.maturityfile", stockfile$stockname)
            attr(maturity, "maturityfile") <-
                stockfile$maturityfile
        }
    } else {
        maturity <- as.list(stockfile$doesmature)
    }
    return(maturity)
}

#' @rdname check_stockfile_funs
check_stock_movement <- function(stockfile) {
    if (stockfile$doesmove == 1) {
        if (check_sf_names("^transitionstocksandratios")) {
            stop("You must specify transition stocks and ratios")
        } else if (check_sf_names("transitionstep")) {
            stop("You must specify the transition step")
        } else {
            movement <-
                stockfile[grep("^doesmove|^transitionstocksandratios|^transitionstep",
                               names(stockfile))]
        }
    } else {
        movement <- list(doesmove == 0)
    }
    return(movement)
}

#' @rdname check_stockfile_funs
check_stock_renewal <- function(stockfile) {
    if (stockfile$doesrenew == 1) {
        doesspawn_ind <- grep("^doesspawn", names(stockfile))
        renewal_ind <- grep("^doesrenew", names(stockfile))
        if ((doesspawn_ind - renewal_ind) == 1) {
            stop("You have not entered any renewal distribution data")
        } else {
            renewal <- stockfile[renewal_ind:(doesspawn_ind - 1)]
            if (!grepl("^minlength", names(renewal))) {
                renewal$minlength <- stockfile$minlength
            }
            if (!grepl("^maxlength", names(renewal))) {
                renewal$maxlength <- stockfile$maxlength
            }
            if (!grepl("dl", names(renewal))) {
                renewal$dl <- stockfile$dl
            }
            if (grepl("normalcondfile", names(renewal))) {
                renewal$normalcondfile <- sprintf("Modelfiles/%s.rec.normalcond", stockfile$stockname)
                attr(renewal, "rec.normalcondfile") <-
                    stockfile$normalcondfile
            } else if (grepl("normalparamfile", names(renewal))) {
                renewal$normalcondfile <- sprintf("Modelfiles/%s.rec.normalparam", stockfile$stockname)
                attr(renewal, "rec.normalparamfile") <-
                    stockfile$normalparamfile
            } else if (grepl("numberfile" %in% names(renewal))) {
                renewal$numberfile <- sprintf("Modelfiles/%s.rec.numberfile", stockfile$stockname)
                attr(renewal, "rec.numberfile") <-
                    stockfile$numberfile
            } else {
                stop("You have not entered any renewal distribution data")
            }
        }
    } else {
        renewal <- list(doesrenew = 1)
    }
    return(renewal)
}

#' @rdname check_stockfile_funs
check_stock_spawning <- function(stockfile) {
    if (stockfile$doesspawn == 1) {
        if (check_sf_names("^spawnfile")) {
            stop("You have not entered any spawning information")
        } else {
            spawning <-    if (check_sf_names("^stockname", sf = dots$stock)) {
                stop("You must specify a stockname")
            }
            if (any(is_list_element_null(list(tmp$minlength, tmp$maxlength, tmp$dl)))) {
                stop("You must specify length information (i.e. minlength, maxlength, dl)")
            } else if (any(is_list_element_null(list(tmp$minage, tmp$maxage)))) {
                stop("You must specify age information (i.e. minage, maxage")
            }
                list(doesspawn = 1,
                     spawnfile = sprintf("Modelfiles/%s.spawnfile", stockfile$stockname))
            attr(spawning, "spawnfile") <- stockfile$spawnfile
        }
    } else {
        spawning <- list(doesspawn = 0)
    }
    return(spawning)
}

#' @rdname check_stockfile_funs
check_stock_straying <- function(stockfile) {
    if (stockfile$doesstray == 1) {
        stopifnot(grepl("strayfile", names(stockfile)))
        straying <-
            list(doesstray = 1,
                 strayfile = sprintf("Modelfiles/%s.strayfile", stockfile$stockname))
        attr(straying, "strayfile") <- stockfile$strayfile
    } else {
        straying <- list(doesstray = 0)
    }
    return(straying)
}

#' @rdname check_stockfile_funs
make_lenaggfile <- function(dots) {
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

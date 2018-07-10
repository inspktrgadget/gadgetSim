# functions to check different parts of gadget stockfile and plug in appropriate
# values these functions are solely meant for use within make_gadget_stockfile

#' Check various entries when making a stockfile using make_gadget_stockfile()
#'
#' These are non-exported functions that are only used within
#' \code{\link{make_gadget_stockfile}}.
#' Each checks a different aspect of the required arguments to a Gadget
#' stockfile and either returns a default value if appropriate or stops
#' function execution if a necessary piece is missing and a straightforward
#' default does not exists
#'
#' @param stockfile A list that contains stockfile information such as
#' \code{gadget_stockfile_default}, see source code for
#' \code{\link{make_gadget_stockfile}}
#'
#' @return A list that is a subset of \code{stockfile} containing the
#' appropriate names for each category being checked
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
    if (any(is_list_element_null(list(stock$minlength,
                                      stock$maxlength,
                                      stock$dl)))) {
        stop("You must specify length information ",
             "(i.e. minlength, maxlength, dl)")
    } else if (any(is_list_element_null(list(stock$minage, stock$maxage)))) {
        stop("You must specify age information (i.e. minage, maxage")
    }
    return(stock)
}

#' @rdname check_stockfile_funs
check_stock_refweightfile <- function(dots) {
    if (check_names("^refweightfile", dots)) {
        if (class(dots$refweightfile) != "data.frame") {
            stop("You must either provide a data.frame for refweightfile or",
                 "rely on the default")
        } else {
            refwgt <- list(refweightfile = dots$refweightfile)
        }
    } else {
        if (check_names("^growth", dots)) {
            if (dots$growth$growthfunction == "lengthvbsimple") {
                stock <- dots$stock
                reflength <- seq(stock$minage, stock$maxlength, stock$dl)
                rfwt_p <- as.numeric(dots$growth$growthparameters[3:4])
                ref_wts <- rfwt_p[1] * reflength ^ rfwt_p[2]
                refwgt <-
                    list(data.frame(length = reflength, weight = ref_wts))
            } else {
                refwgt <- list(refweightfile = "")
                warning("Sorry, gadgetSim is not smart enough to create a ",
                         "refweightfile for growth functions other than ",
                         "lenghtvbsimple yet. You'll have to specify it ",
                         "manually")
            }
        } else {
            refwgt <- list(refweightfile = "")
        }
    }
    return(refwgt)
}

#' @rdname check_stockfile_funs
check_stock_grw_eat_len <- function(dots,
                                    lenaggfile = get("lenaggfile",
                                                     env = parent.frame())) {
    if (!check_names("^growthandeatlengths", dots)) {
        grw_eat_len <- list(growthandeatlengths = lenaggfile)
    } else {
        if (class(dots$growthandeatlengths) != "data.frame") {
            stop("You must either provide a data.frame specifying the aggfile ",
                 "for growth and eat lengths or rely on the default")
        } else {
            grw_eat_len <- list(growthandeatlengths = dots$growthandeatlengths)
        }
    }
    return(grw_eat_len)
}

#' @rdname check_stockfile_funs
check_stock_growth <- function(dots) {
    if (!check_names("^growth", dots)) {
        growth <- NULL
    } else {
        growth <- dots$growth
        if (!check_names("^beta", dots$growth)) {
            growth$beta <-
                sprintf("(* #%1$s.bbin.mult #%1$s.bbin)", dots$stock$stockname)
        }
        if (!check_names("^maxlengthgroupgrowth", dots$growth)) {
            growth$maxlengthgroupgrowth <- 15
        }
        growth <- list(growth = growth)
    }
    return(growth)
}

#' @rdname check_stockfile_funs
check_stock_m <- function(dots) {
    if (!check_names("^naturalmortality", dots)) {
        m_len <- ((dots$stock$maxage - dots$stock$minage) + 1)
        m <- list(naturalmortality = rep(0.2, times = m_len))
        message("You did not specify natural mortality...",
                "setting all ages to 0.2")
        return(m)
    } else {
        m <- list(naturalmortality = dots$naturalmortality)
    }
    return(m)
}


#' @rdname check_stockfile_funs
check_stock_prey <-
    function(dots, lenaggfile = get("lenaggfile", env = parent.frame())) {
    if (check_names("^prey", dots)) {
        if (all(c("preylengths", "energycontent") %in% names(dots$prey))) {
            filename <- sprintf("Aggfiles/%s.preylengths", dots$stock$stockname)
            prey <-
                list(preylengths = dots$prey$preylengths,
                     energycontent = dots$prey$energycontent)
        } else if (check_names("^energycontent", dots$prey) &
                   !check_names("^preylengths", dots$prey)) {
            prey <-
                list(preylengths = lenaggfile,
                     energycontent = dots$prey$energycontent)
        } else if (check_names("^preylengths", dots$prey) &
                   !check_names("^energycontent", dots$prey)) {
            prey <-
                list(preylengths = dots$prey$preylengths,
                     energycontent = 1)
        } else {
            prey <-
                list(preylengths = lenaggfile,
                     energycontent = 1)
        }
        prey <- list(prey = prey)
    } else {
        prey <- NULL
    }
    return(prey)
}

#' @rdname check_stockfile_funs
check_stock_predator <- function(dots) {
    if (check_names("^doeseat", dots)) {
        req_names <- c("suitability", "preference",
                       "maxconsumption", "halffeedingvalue")
        if (!all(req_names %in%
                 names(dots$doeseat))) {
            stop("\n", "You missed the following required items in doeseat",
                 "\n",
                  paste(paste(" * ",
                              req_names[!(req_names %in% names(dots$doeseat))]),
                        collapse = "\n"))
        }
        predator <- dots$doeseat
    } else {
        predator <- NULL
    }
    return(predator)
}

#' @rdname check_stockfile_funs
check_stock_initcond <- function(dots) {
    if (check_names("^initialconditions|^initcond", dots)) {
        ic_data <- dots[[grep("^initialconditions|^initcond", names(dots))]]
        ic_file_types <- c("normalparamfile", "normalcondfile", "numberfile")
        if ((is.list(ic_data) & length(ic_data) == 1) |
            (is.data.frame(ic_data))) {
            if (is.data.frame(ic_data)) {
                ic_file_type <- class(ic_data)[1]
                dat <- ic_data
            } else {
                ic_file_type <- class(ic_data[[1]])[1]
                dat <- ic_data[[1]]
            }
            if (!(ic_file_type %in% ic_file_types)) {
                stop("Initial conditions must of one of the following classes",
                     "\n",
                     paste(paste(" * ", ic_file_types), collapse = "\n"), "\n",
                     "see ?stock_distribution_funs")
            }
            initcond <-
                list(minage = dots$stock$minage,
                     maxage = dots$stock$maxage,
                     minlength = dots$stock$minlength,
                     maxlength = dots$stock$maxlength,
                     dl = dots$stock$dl)
            initcond[[ic_file_type]] <- dat
        } else {
            req_args <- c("minage", "maxage", "minlength", "maxlength", "dl")
            if (!all(req_args %in% names(ic_data))) {
                stop("You entered some, but not all information ",
                     "for initial conditions")
            } else {
                initcond <-
                    list(initialconditions = "",
                         minage = ic_data$minage,
                         maxage = ic_data$maxage,
                         minlength = ic_data$minlength,
                         maxlength = ic_data$maxlength,
                         dl = ic_data$dl,
                         sdev = ifelse(check_names("sdev", ic_data),
                                       ic_data$sdev, NULL))
                ic_data <- ic_data[!(names(ic_data) %in%
                                         c("initialconditions",
                                           req_args,
                                           "sdev"))]
                ic_file_type <- class(ic_data[[1]])[1]
                initcond[[ic_file_type]] <- dat
            }
        }
        return(list(initialconditions = initcond))
    } else {
        stop("You have not specified initial conditions for the stock")
    }
}

#' @rdname check_stockfile_funs
check_stock_migration <- function(dots) {
    if (check_names("^migration", dots)) {
        if ("migration_matrix" %in% class(dots$migration)) {
            mig_matrices <- format_migration(dots$migration)
            matrices <- lapply(mig_matrics, function(x) x$matrix)
            yearstepfile <-
                do.call("rbind", lapply(mig_matrices, function(x) {
                    return(x$yearstepfile)
                }))
            migration <-
                list(yearstepfile = yearstepfile,
                     definematrices = mig_matrices)
        } else if ("migration_ratios" %in% class(dots$migration)) {
            mig_matrices <- format_migration(dots$migration)
            matrices <- lapply(mig_matrics, function(x) x$matrix)
            yearstepfile <-
                do.call("rbind", lapply(mig_matrices, function(x) {
                    return(x$yearstepfile)
                }))
            migration <-
                list(yearstepfile = yearstepfile,
                     defineratios = mig_matrices)
        } else {
            stop("Migration information must be a data.frame of one of the ",
                 "following classes", "\n",
                 paste(paste(" * ", c("migration_matrix", "migration_ratios"),
                             collapse = "\n")),
                 "\n", "see ?migration_funs")
        }
    } else {
        migration <- NULL
    }
    return(migration)
}

#' @rdname check_stockfile_funs
check_stock_maturity <- function(dots) {
    if (check_names("^maturity", dots)) {
        if (!check_names("^maturityfunction", dots$maturity)) {
            stop("You must enter a maturity function with appropriate ",
                 "parameters for the stock")
        }
        maturity <-
            list(maturityfunction = dots$maturity$maturityfunction,
                 maturityfile = dots$maturity[grep("^maturityfunction",
                                                   names(dots$maturity),
                                                   invert = TRUE)])
    } else {
        maturity <- NULL
    }
    return(maturity)
}

#' @rdname check_stockfile_funs
check_stock_movement <- function(dots) {
    if (check_names("^movement|^transition", dots)) {
        movement <-dots[grep("^movement|^transition", names(dots))]
    } else {
        movement <- NULL
    }
    return(movement)
}

#' @rdname check_stockfile_funs
check_stock_renewal <- function(dots) {
    if (check_names("^renewal|^recruitment", dots)) {
        rec_info <- dots[grep("^renewal|^recruitment", names(dots))]
        if (length(rec_info) == 1) {
            rec_class <- class(rec_info[[1]])[1]
            renewal <-
                list(minlength = dots$stock$minlength,
                     maxlength = dots$stock$maxlength,
                     dl = dots$stock$dl)
            renewal[[rec_class]] <- rec_info[[1]]
        } else {
            rec_data <-
                rec_info[grep("^normalparamfile|^normalcondfile|^numberfile",
                              names(rec_info))]
            rec_class <- class(rec_data[[1]])[1]
            renewal <-
                list(minlength = rec_info$minlength,
                     maxlength = rec_info$maxlength,
                     dl = rec_info$dl)
            renewal[[rec_class]] <- rec_data[[1]]
        }
        renewal <- list(renewal = renewal)
    } else {
        renewal <- NULL
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
            spawning <- list(spawning = dots$spawning)
        }
    } else {
        spawning <- NULL
    }
    return(spawning)
}

#' @rdname check_stockfile_funs
check_stock_straying <- function(dots) {
    if (check_names("^straying", dots)) {
        straying <- list(straying = dots$straying)
    } else {
        straying <- NULL
    }
    return(straying)
}


#' @rdname check_stockfile_funs
make_stock_lenaggfile <- function(dots) {
    stock <- dots$stock
    reflength <- seq(stock$minlength, stock$maxlength, by = stock$dl)
    lenaggfile <-
        data.frame(name = sprintf("len%s", reflength[2:length(reflength)]),
                   lower = reflength[-length(reflength)],
                   upper = reflength[-1],
                   stringsAsFactors = FALSE)
    return(lenaggfile)
}

#' Functions to produce initial conditions or renewal distribution data
#'
#' These functions will produce a data.frame of class normalcondfile,
#' normalparamfile, or numberfile
#'
#' @param ... Arguments necessary for a normalcondfile, a normalparamfile, or a
#' numberfile. See initial conditions and renewal in the Gadget User Guide
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
#' @export
normalparamfile <- function(...) {
    df <- as.data.frame(dots2list(...))
    class(df) <- c("normalparamfile", "data.frame")
    return(df)
}

#' @rdname stock_distribution_funs
#' @export
numberfile <- function(...) {
    df <- as.data.frame(dots2list(...))
    class(df) <- c("numberfile", "data.frame")
    return(df)
}


#' Helper functions to create and format migration matrices
#'
#' These functions create and format migration information for a Gadget
#' stockfile. The \code{migration_*} functions create a data.frame with class of
#' the same name as the function for use in
#' \code{\link{check_stockfile_migration}}. The returned data.frame can then be
#' read into \code{format_migration} which will return a list of the correct
#' structure to be written to file. These functions will be most useful when
#' migration proportions are not known and are specified as a switch in the
#' Gadget model
#'
#' @param years Numeric vector of years to specify migration proportions for
#' @param steps Numeric vector of steps to specify migration proportions for
#' @param areas Numeric vector of areas in the Gadget model
#' @param prop Vector of proportions to use. Can be numeric or character.
#' Length must be the same as \eqn{years * steps * areas^2} or a factor of such
#' (or 1)
#'
#' @return \code{migration_*} functions return a data.frame with class the same
#' as the function name. \code{format_migration_matrix} returns a list with each
#' element a matrix or data.frame of the migration ratios to be used
#'
#' @export
#'
#' @name migration_funs
#'
#' @examples
#' years <- 1:2
#' steps <- 1:2
#' areas <- 1:2
#' ysa_combo <- c(paste(years, steps, sep = "."),
#'                paste(rev(years),
#'                steps, sep = "."))
#' prop <- sort(rep(paste("#cod.migration", ysa_combo, sep = "."), 4))
#' migration_matrix(years, steps, areas, prop)
migration_matrix <- function(years, steps, areas, prop) {
    mig_df <- migration_df(years, steps, areas, prop)
    class(mig_df) <- c("migration_matrix", "data.frame")
    return(mig_df)
}

#' @rdname migration_funs
#' @export
migration_ratios <- function(years, steps, areas, prop) {
    mig_df <- migration_df(years, steps, areas, prop)
    class(mig_df) <- c("migration_ratios", "data.frame")
    return(mig_df)
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

#' @rdname migration_funs
#' @export
format_migration <- function(mig_matrix) {
    UseMethod("format_migration")
}

#' @rdname migration_funs
#' @param mig_matrix A \code{data.frame} of class "migration_matrix" as produced
#' by \code{migration_matrix}
format_migration.migration_matrix <- function(mig_matrix) {
    if (!("migration_matrix" %in% class(mig_matrix))) {
        stop("You must use a data.frame of class migration_matrix", "\n",
             "see ?migration_matrix")
    }
    years <- unique(mig_matrix$year)
    steps <- unique(mig_matrix$step)
    mm_list <-
        lapply(years, function(x) {
            lapply(steps, function(y) {
                tmp <- subset(mig_matrix, year == x & step == y)
                mm <- matrix(tmp$prop, nrow = sqrt(nrow(tmp)))
                if (is.numeric(mm)) {
                    if (!(all(colSums(mm) == 1))) {
                        stop("Migration ratios at year ", x,  " and step",
                             "do not equal 1", x, y)
                    }
                }
                yearstepfile = data.frame(year = x,
                                          step = y,
                                          matrix = sprintf("matrix%s.%s", x, y))
                return(list(matrix = mm,
                            yearstepfile = yearstepfile))
            })
        })
    return(unlist(mm_list, recursive = FALSE))
}

#' @rdname migration_funs
#' @param A \code{data.frame} of class "migration_ratios" as produced by
#' \code{migration_ratios}
format_migration.migration_ratios <- function(mig_ratios) {
    if (!("migration_ratios" %in% class(mig_ratios))) {
        stop("You must use a data.frame of class migration_ratios", "\n",
             "see ?migration_ratios")
    }
    years <- unique(mig_ratios$year)
    steps <- unique(mig_ratios$step)
    mm_list <-
        lapply(years, function(x) {
            lapply(steps, function(y) {
                tmp <- subset(mig_ratios, year == x & step == y)
                mm <- matrix(tmp$prop, nrow = sqrt(nrow(tmp)))
                if (!is.numeric(mm)) {
                    if (!(all(colSums(mm) == 1))) {
                        stop("Migration ratios at year ", x,  " and step",
                             "do not equal 1", x, y)
                    }
                }
                mm <- as.matrix(subset(mm, from != to, select = from:prop))
                yearstepfile <-
                    data.frame(year = x, step = y,
                               matrix = sprintf("matrix%s.%s", x, y))
                return(list(matrices = mm,
                            yearstepfile = yearstepfile))
            })
        })
    return(unlist(mm_list, recursive = FALSE))
}

#' Check components of a  Gadget stockfile that is being read
#'
#' This function will check to see if the various components to a stockfile are
#' present during reading and will format those components if present. If a
#' filename is present in a various component then this function will read that
#' filename and place the appropriate data into an attribute of the stockfile.
#' This function is for use in \code{\link{read_gadget_stockfile}}
#'
#' @param stockfile The stockfile being read in by
#' \code{\link{read_gadget_stockfile}}
#' @inheritParams call_gadget
#'
#' @return A list the same as the stockfile, but with attributes added for data.
#'
#' @examples
#' stockfile <- readLines("gadget_model/cod")
#' check_stockfile(stockfile)
check_stockfile <- function(stockfile, path = NULL) {
    chk_cmp <- function(comp) {
        if (stockfile[[comp]] == 1) {
            return(TRUE)
        } else {return(FALSE)}
    }
    chk_file <- function(comp) {
        comp <- stockfile[[comp]]
        if (all(is.null(comp) | length(comp) == 0 | comp == "")) {
            return(FALSE)
        } else {
            return(TRUE)
        }
    }
    stockname <- stockfile["stockname"]
    stock_info <- c("livesonareas", "minage", "maxage",
                    "minlength", "maxlength", "dl")
    stock_info <- stockfile[stock_info]
    stock_info <- lapply(stock_info, as.numeric)
    if (chk_file("refweightfile")) {
        refweightfile <-
            list(refweightfile =
                     read_gadget_refweightfile(stockfile$refweightfile,
                                               path = path))
    } else {
        refweightfile <- NULL
    }
    if (chk_file("growthandeatlengths")) {
        growthandeatlengths <-
            list(growthandeatlengths =
                  read_gadget_stock_len_aggfile(stockfile$growthandeatlengths,
                                                path = path))
    } else {
        growthandeatlengths <- NULL
    }
    if (chk_cmp("doesgrow")) {
        growth_st <- get_index("doesgrow", names(stockfile)) + 1
        growth_end <- get_index("naturalmortality", names(stockfile)) - 1
        growth <- list(growth = stockfile[growth_st:growth_end])
    } else {
        growth <- NULL
    }
    if (chk_file("naturalmortality")) {
        nat_mort <-
            list(naturalmortality = stockfile[get_index("naturalmortality",
                                                         names(stockfile))])
    } else {
        nat_mort <- NULL
    }
    if (chk_cmp("iseaten")) {
        prey <-
            list(prey = list(
                preylengths = read_gadget_preylengths(stockfile$preylengths,
                                                      path = path),
                energycontent = ifelse(check_names("^energycontent$",
                                                   stockfile),
                                       stockfile$energycontent,
                                       1)
            ))
    } else {
        prey <- NULL
    }
    if (chk_cmp("doeseat")) {
        predator_st <- get_index("doeseat", names(stockfile)) + 1
        predator_end <- get_index("initialconditions", names(stockfile)) - 1
        predator <- list(predator = stockfile[predator_st:predator_end])
    } else {
        predator <- NULL
    }
    if (check_names("^initialconditions$", stockfile)) {
        init_cond_st <- get_index("^initialconditions$", names(stockfile))
        init_cond_end <- get_index("^doesmigrate", names(stockfile)) - 1
        init_cond <-stockfile[init_cond_st:init_cond_end]
        init_cond <- init_cond[-get_index("^initialconditions$",
                                          names(init_cond))]
        init_cond <-
            within(init_cond, {
                minage <- as.numeric(minage)
                maxage <- as.numeric(maxage)
                minlength <- as.numeric(minlength)
                maxlength <- as.numeric(maxlength)
                dl <- as.numeric(dl)})
        filename <- init_cond[[length(init_cond)]]
        data_dist_type <- names(init_cond)[length(init_cond)]
        init_cond[[data_dist_type]] <-
            read_gadget_init_cond(init_cond[[length(init_cond)]],
                                  data_dist_type,
                                  path = path)
        init_cond <- list(initialconditions = init_cond)
    } else {
        init_cond <- NULL
    }
    if (chk_cmp("doesmigrate")) {
        migration <-
            stockfile[get_index("^doesmigrate$", names(stockfile)):
                      (get_index("^doesmature$", names(stockfile)) - 1)]
        yearstep_filename <- migration$yearstepfile
        migration$yearstepfile <-
            read_gadget_yearstepfile(stockfile$yearstepfile,
                                     path = path)
        if (check_names("definematrices", migration)) {
            migration$definematrices <-
                read_gadget_migration_file(stockfile$definematrices,
                                           "matrices", path = path)
        } else if (check_names("defineratios", migration)) {
            migration$defineratios <-
                read_gadget_migration_file(stockfile$defineratios,
                                           "ratios", path = path)
        }
        migration <- list(migration = migration)
    } else {
        migration <- NULL
    }
    if (chk_cmp("doesmature")) {
        maturity <-
            list(maturity =
                     read_gadget_maturity_file(stockfile$maturityfile,
                                               path = path))
    } else {
        maturity <- NULL
    }
    if (chk_cmp("doesmove")) {
        movement_st <- get_index("doesmove", names(stockfile)) + 1
        movement_end <- get_index("doesrenew", names(stockfile)) - 1
        movement <- list(movement = stockfile[movement_st:movement_end])
    } else {
        movement <- NULL
    }
    if (chk_cmp("doesrenew")) {
        renewal_st <- get_index("^doesrenew$", names(stockfile)) + 1
        renewal_end <- get_index("^doesspawn$", names(stockfile)) - 1
        renewal <- stockfile[renewal_st:renewal_end]
        filename <- renewal[[length(renewal)]]
        data_dist_type <- names(renewal)[length(renewal)]
        renewal[[data_dist_type]] <-
            read_gadget_renewal(filename,
                                data_dist_type,
                                path = path)
        renewal <- list(renewal = renewal)
    } else {
        renewal <- NULL
    }
    if (chk_cmp("doesspawn")) {
        spawning <-
            list(spawning =
                     read_gadget_spawnfile(stockfile$spawnfile,
                                           path = path))
    } else {
        spawning <- NULL
    }
    if (chk_cmp("doesstray")) {
        straying <-
            list(straying = read_gadget_strayfile(stockfile$strayfile,
                                                  path = path))
    } else {
        straying <- NULL
    }
    return(c(stockname, stock_info, refweightfile, growthandeatlengths, growth,
             nat_mort, prey, predator, init_cond, migration, maturity,
             movement, renewal, spawning, straying))
}


#' Format an object of class \code{gadget_stock} to prepare for writing to file
#'
#' This function is used internally by \code{\link{write_gadget_file}},
#' specifically the method for class \code{gadget_stock}. It formats the various
#' components of a \code{gadget_stock} stockfile so that it is ready to be
#' written to file
#'
#' @param stockfile
#'
#' @return A list with all components appropriately formatted for writing
#'
#' @examples
#' # see created object from ?make_gadget_stockfile
#' format_stockfile(cod)
format_stockfile <- function(stockfile) {
    chk_comp <- function(comp, gad_name, keep_str = FALSE) {
        if (check_names(comp, stockfile)) {
            if (comp == "initialconditions") {
                interrogative <- ""
            } else {
                interrogative <- list(1)
            }
            if (keep_str) {
                comp <- c(setNames(interrogative, gad_name), stockfile[comp])
            } else if (any(grepl("gadget_", class(stockfile[[comp]])))) {
                comp <- c(setNames(interrogative, gad_name), stockfile[comp])
            } else if (is.data.frame(stockfile[[comp]])) {
                comp <- c(setNames(interrogative, gad_name), stockfile[comp])
            } else {
                comp <- c(setNames(interrogative, gad_name), stockfile[[comp]])
            }
            if ("spawning" %in% names(comp)) {
                names(comp)[names(comp) == "spawning"] <- "spawnfile"
            } else if ("straying" %in% names(comp)) {
                names(comp)[names(comp) == "straying"] <- "strayfile"
            }
            return(comp)
        } else {
            return(setNames(list(0), gad_name))
        }
    }
    growth <- chk_comp("growth", "doesgrow")
    nat_mort <- stockfile[["naturalmortality"]]
    prey <- chk_comp("prey", "iseaten")
    predator <- chk_comp("predator", "doeseat")
    init_cond <- chk_comp("initialconditions", "initialconditions")
    migration <- chk_comp("migration", "doesmigrate")
    maturity <- chk_comp("maturity", "doesmature")
    movement <- chk_comp("movement", "doesmove")
    renewal <- chk_comp("renewal", "doesrenew")
    spawning <- chk_comp("spawning", "doesspawn", keep_str = TRUE)
    straying <- chk_comp("straying", "doesstray", keep_str = TRUE)
    required_args <- stockfile[c("stockname", "livesonareas", "minage",
                                 "maxage", "minlength", "maxlength", "dl",
                                 "refweightfile", "growthandeatlengths")]
    return(structure(c(required_args, growth, nat_mort, prey, predator,
                       init_cond, migration, maturity, movement, renewal,
                       spawning, straying),
                     class = c("gadget_stock", "list")))
}

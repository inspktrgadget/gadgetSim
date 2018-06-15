# functions to make gadget likelihood components and likelihood file



#' Create various Gadget likelihood components
#'
#' These functions will create the various likelihood components used by Gadget
#' in an optimizing run. There is a function for each specific likelihood
#' component. These then get fed into \code{\link{make_gadget_likelihood}},
#' which will assemble them altogether into the Gadget likelihood file.
#'
#' @param ... Named elements corresponding to the arguments needed for each
#' respective likelihood component (see
#' \href{http://www.hafro.is/gadget/files/userguide.pdf}{Gadget User Guide})
#'
#' @return A list of class \code{gadget_*_likelihood}, with * corresponding to
#' the approprate likelihood type
#' @export
#'
#' @name gadget_likelihood_comps
#'
#' @examples
#' # read in the data
#' path <- system.file(gad_mod_dir, package = "gadgetSim")
#' stock_std <- get_stock_std(path = path)
#'
#' # sample the data
#' lengrps <- seq(0.5, 150.5, 1)
#' suit <- logistic_selectivity(lengrps, alpha = 0.25, l50 = 30)
#' cod_lengrps <- add_lengthgroups(stock_std$cod, lengrps)
#' cod_survey <- survey_gadget(cod_lengrps, lengrps, suit, 0)
#' cod_al_data <- strip_age_length_data(cod_survey, 0.5, 0.5)
#' spr_ldist <-
#'   subset(cod_al_data$length_data, step == 1)
#' spr_ldist$age <- "all"
#' spr_ldist <- subset(spr_ldist,
#'                     select = c("year", "step", "area",
#'                                "age", "length", "number"))
#'
#' spr_aldist <-
#'   subset(cod_al_data$age_data, step == 1)
#'
#' # make the components
#' spr_ldist_comp <-
#'   make_gadget_catchdistribution(name = "spr.ldist",
#'                                 data = spr_ldist,
#'                                 fleetnames = "spr",
#'                                 stocknames = "cod",
#'                                 ageagg = list(all = 1))
#' spr_aldist_comp <-
#'   make_gadget_catchdistribution(name = "spr.aldist",
#'                                 data = spr_aldist,
#'                                 fleetnames = "spr",
#'                                 stocknames = "cod")
make_gadget_penalty <- function(...) {
    penalty_comp <- format_lik_comp(penalty_likelihood, ...)
    if (!identical(names(penalty_comp),
                   names(rm_null_elements(penalty_likelihood)))) {
        stop("Required names for penalty likelihood are not correct")
    } else {
        return(penalty_comp)
    }
}

#' @rdname gadget_likelihood_comps
make_gadget_understocking <- function(...) {
    understocking_comp <- format_lik_comp(understocking_likelihood, ...)
    if (!identical(names(understocking_comp),
                   names(rm_null_elements(understocking_likelihood)))) {
        stop("Required names for understocking likelihood are not correct")
    } else {
        return(understocking_comp)
    }
}

#' @rdname gadget_likelihood_comps
#' @param areaagg List with names corresponding to the aggregation and
#' values to the areas to be included in those aggregations
#' @param ageagg List with names corresponding to the aggregation and
#' values to the ages to be included in those aggregations
#' @param lenagg List with names corresponding to the aggregation and
#' values to the lengths to be included in those aggreations
make_gadget_catchdistribution <- function(...,
                                          areaagg = NULL,
                                          ageagg = NULL,
                                          lenagg = NULL) {
    comp <- format_lik_comp(catchdistribution_likelihood, ...)
    lik_type <- "catchdistribution"
    lik_name <- comp$name
    check_lik_data(comp,
                   data_cols = c("year", "step", "area",
                                     "age", "length", "number"),
                   lik_type)
    check_lik_names(comp, lik_type)
    comp <- format_aggfile_attributes(comp,
                                      areaagg = areaagg,
                                      ageagg = ageagg,
                                      lenagg = lenagg)
    format_datafile_attribute()
    return(comp)
}

#' @rdname gadget_likelihood_comps
make_gadget_catchstatistics <- function(...,
                                        areaagg = NULL,
                                        lenagg = NULL,
                                        ageaggfile = NULL) {
    comp <- format_lik_comp(catchstatistics_likelihood, ...)
    lik_type <- "catchstatistics"
    lik_name <- comp$name
    check_lik_data(comp,
                   data_cols = c("year", "step", "area",
                                 "age", "number", "mean"),
                   lik_type)
    check_lik_names(comp, lik_type)
    comp <- format_aggfile_attributes(comp,
                                      areaagg = areaagg,
                                      ageagg = ageagg,
                                      lenagg = lenagg)
    format_datafile_attribute()
    return(comp)
}

#' @rdname gadget_likelihood_comps
make_gadget_stockdistribution <- function(...,
                                          areaagg = NULL,
                                          lenagg = NULL,
                                          ageaggfile = NULL) {
    comp <- format_lik_comp(stockdistribution_likelihood, ...)
    lik_type <- "stockdistribution"
    lik_name <- comp$name
    check_lik_data(comp,
                   data_cols = c("year", "step", "area", "stock",
                                 "age", "length", "number"),
                   lik_type)
    check_lik_names(comp, lik_type)
    comp <- format_aggfile_attributes(comp,
                                      areaagg = areaagg,
                                      ageagg = ageagg,
                                      lenagg = lenagg)
    format_datafile_attribute()
    return(comp)
}

#' @rdname gadget_likelihood_comps
make_gadget_surveyindices <- function(...,
                                      areaagg = NULL,
                                      lenagg = NULL,
                                      ageaggfile = NULL) {
    si_type <- comp$sitype
    lik_template <-
        switch(si_type,
               lengths = si_lengths,
               ages = si_ages,
               fleets = si_fleets,
               acoustic = si_acoustic,
               effort = si_effort)
    comp <- format_lik_comp(lik_template, ...)
    lik_type <- "surveyindices"
    lik_name <- comp$name
    si_types <- c("lengths", "ages", "fleets", "acoustic", "effort")
    if (!check_names("^sitype$", comp)) {
        stop("\n",
             "You must provide a surveyindices type of one of the following",
             "\n",
             paste(paste0(" * ", si_types), collapse = "\n"))
    } else if (comp$sitype %in% si_types) {
        stop("\n",
             "You must provide a surveyindices type of one of the following",
             "\n",
             paste(paste0(" * ", si_types), collapse = "\n"))
    }
    base_colnames <- c("year", "step", "area")
    data_cols <-
        switch(si_type,
               length = c(base_colnames, "length", "number"),
               age = c(base_colnames, "age", "number"),
               fleet = c(base_colnames, "length", "number"),
               acoustic = c(base_colnames, "survey", "acoustic"),
               effort = c(base_colnames, "fleet", "effort"))
    check_lik_data(comp,
                   data_cols = data_cols,
                   lik_type)
    check_lik_names(comp, lik_type)
    comp <- format_aggfile_attributes(comp,
                                      areaagg = areaagg,
                                      ageagg = ageagg,
                                      lenagg = lenagg)
    format_datafile_attribute()
    return(comp)
}

#' @rdname gadget_likelihood_comps
make_gadget_surveydistribution <- function(...,
                                          areaagg = NULL,
                                          lenagg = NULL,
                                          ageaggfile = NULL) {
    comp <- format_lik_comp(surveydistribution_likelihood, ...)
    lik_type <- "surveydistribution"
    lik_name <- comp$name
    check_lik_data(comp,
                   data_cols = c("year", "step", "area",
                                 "age", "length", "number"),
                   lik_type)
    check_lik_names(comp, lik_type)
    comp <- format_aggfile_attributes(comp,
                                      areaagg = areaagg,
                                      ageagg = ageagg,
                                      lenagg = lenagg)
    format_datafile_attribute()
    return(comp)
}


#' @rdname gadget_likelihood_comps
#' @param preyagg List with names corresponding the the aggregated prey names
#' and values to the prey to be included in those aggregations
make_gadget_stomachcontent <- function(...,
                                       areaagg = NULL,
                                       lenagg = NULL,
                                       preyagg = NULL) {
    comp <- format_lik_comp(surveydistribution_likelihood, ...)
    lik_type <- "surveydistribution"
    lik_name <- comp$name
    check_lik_data(comp,
                   data_cols = c("year", "step", "area",
                                 "predator", "prey", "ratio"),
                   lik_type)
    check_lik_names(comp, lik_type)
    comp <- format_aggfile_attributes(comp,
                                      areaagg = areaagg,
                                      lenagg = lenagg,
                                      preyagg = preyagg)
    format_datafile_attribute()
    return(comp)
}

#' @rdname gadget_likelihood_comps
make_gadget_recaptures <- function(...,
                                   areaagg = NULL,
                                   lenagg = NULL) {
    comp <- format_lik_comp(recpatures_likelihood, ...)
    lik_type <- "recaptures"
    lik_name <- comp$name
    check_lik_data(comp,
                   data_cols = c("tagid", "year", "step",
                                 "area", "length", "number"),
                   lik_type)
    check_lik_names(comp, lik_type)
    comp <- format_aggfile_attributes(comp,
                                      areaagg = areaagg,
                                      lenagg = lenagg)
    format_datafile_attribute()
    return(comp)
}

#' @rdname gadget_likelihood_comps
make_gadget_recstatistics <- function(...,
                                      areaagg = NULL) {
    comp <- format_lik_comp(recstatistics_likelihood, ...)
    lik_type <- "recstatistics"
    lik_name <- comp$name
    base_cols <- c("tagid", "year", "step", "area", "number", "mean")
    data_cols <-
        switch(comp$`function`,
               lengthcalcstddev = base_cols,
               lengthgivenstddev = c(base_cols, "sd"),
               lengthnostddev = base_cols)
    check_lik_data(comp,
                   data_cols = data_cols,
                   lik_type)
    check_lik_names(comp, lik_type)
    comp <- format_aggfile_attributes(comp,
                                      areaagg = areaagg,
                                      lenagg = lenagg)
    format_datafile_attribute()
    return(comp)
}


#' @rdname gadget_likelihood_comps
make_gadget_migrationpenalty <- function(...) {
    comp <- format_lik_comp(migrationpenalty_likelihood, ...)
    lik_type <- "migrationpenalty"
    lik_name <- comp$name
    check_lik_names(comp, lik_type)
    return(comp)
}

#' @rdname gadget_likelihood_comps
make_gadget_migrationproportion <- function(...,
                                   areaagg = NULL) {
    comp <- format_lik_comp(migrationproportion_likelihood, ...)
    lik_type <- "migrationproportion"
    lik_name <- comp$name
    check_lik_data(comp,
                   data_cols = c("year", "step", "area", "ratio"),
                   lik_type)
    check_lik_names(comp, lik_type)
    comp <- format_aggfile_attributes(comp,
                                      areaagg = areaagg,
                                      lenagg = lenagg)
    format_datafile_attribute()
    return(comp)
}

#' @rdname gadget_likelihood_comps
make_gadget_catchinkilos <- function(...,
                                     areaagg = NULL) {
    comp <- format_lik_comp(catchinkilos_likelihood, ...)
    lik_type <- "catchinkilos"
    lik_name <- comp$name
    if (comp$aggregationlevel == 0) {
        data_cols <- c("year", "step", "area", "fleet", "biomass")
    } else {
        data_cols <- c("year", "area", "fleet", "biomass")
    }
    check_lik_data(comp,
                   data_cols = data_cols,
                   lik_type)
    check_lik_names(comp, lik_type)
    comp <- format_aggfile_attributes(comp,
                                      areaagg = areaagg,
                                      lenagg = lenagg)
    format_datafile_attribute()
    return(comp)
}

#' Gadget likelihood helper functions
#'
#' These functions are just helper functions that are used in the various
#' \code{\link{gadget_likelihood_comps}} functions. They are simply for
#' automation purposes to make things cleaner. These are Russian doll functions
#' that have helper functions within helper functions. This might make debugging
#' painful in the future; if this is future-you, then don't do this sort of thing
#' again. If this is another reader...I'm sorry.
#'
#' @param ... There are a number of different arguments used in these various
#' functions, and each depends on the function and what it is being used for.
#' See the source code for the \code{\link{gadget_likelihood_comps}} function
#' of interest to see what it does.
#'
#' @name likelihood_helper_funs
#'
#' @return Depends on the function
format_lik_comp <- function(comp, ...) {
    dots <- dots2list(...)
    comp <- modifyList(comp, dots)
    comp <- rm_null_elements(comp)
}

#' @rdname likelihood_helper_funs
check_lik_data <- function(comp, data_cols, lik_type) {
    if (!(class(comp$data) %in% c("data.frame", "matrix"))) {
        stop("\n\n", "You must provide data for ",
             sprintf("make_gadget_%s", lik_type),
             " as a data.frame")
    } else if (ncol(comp$data) != length(data_cols)) {
        stop("\n",
             sprintf("Data for make_gadget_%s must have the following columns",
                     lik_type),
             "\n",
             paste(paste0(" * ", data_cols),
                   collapse = "\n"))
    }
}

#' @rdname likelihood_helper_funs
check_lik_names <- function(comp, lik_type) {
    lik_template <- getFromNamespace(paste(lik_type, "likelihood", sep = "_"),
                                     ns = "gadgetSim")
    if (!identical(names(comp), names(rm_null_elements(lik_template)))) {
        stop("Required names for catchdistribution likelihood are not correct")
    }
}

#' @rdname likelihood_helper_funs
add_aggfile_attribute <- function(agg_type, env = parent.frame()) {
    aggfile_name <- lik_agg_filename(env$lik_type, env$lik_name, agg_type)
    aggfile_type <- paste0(agg_type, "aggfile")
    aggfile <- get(paste0(agg_type, "agg"), envir = env)
    attr(env$comp, paste(env$lik_name, aggfile_type, sep = ".")) <-
        structure(aggfile,
                  filename = aggfile_name)
    env$comp[[aggfile_type]] <- aggfile_name
}


#' @rdname likelihood_helper_funs
lik_agg_filename <- function(..., aggfile_dir = "Aggfiles") {
    dots <- dots2list(...)
    return(paste(aggfile_dir, paste(c(dots, c(list("agg"))), collapse = "."), sep = "/"))
}

#' @rdname likelihood_helper_funs
lik_data_filename <- function(..., datafile_dir = "Data") {
    dots <- dots2list(...)
    return(paste(datafile_dir, paste(dots, collapse = "."), sep = "/"))
}

#' @rdname likelihood_helper_funs
format_datafile_attribute <- function(env = parent.frame()) {
    data_filename <-
        lik_data_filename(env$lik_type, env$lik_name, env$comp$`function`)
    attr(env$comp, paste(env$lik_name, "datafile")) <-
        structure(env$comp$data, filename = data_filename)
    env$comp$data <- data_filename
}

#' @rdname likelihood_helper_funs
make_default_aggfile <- function(comp, agg_type) {
    if (agg_type == "area") {
        return(list(all = 1))
    } else if (agg_type == "len") {
        lens <- unique(comp$data$length)
        out <- data.frame(length = sprintf("len%s", lens[-length(lens)]),
                          lower = lens[-length(lens)],
                          upper = lens[-1],
                          stringsAsFactors = FALSE)
        return(out)
    } else if (agg_type == "age") {
        ages <- unique(comp$data$age)
        out <- data.frame(age = sprintf("age%s", ages[-length(ages)]),
                          lower = ages[-length(ages)],
                          upper = ages[-1],
                          stringsAsFactors = FALSE)
        return(out)
    } else if (agg_type == "prey") {
        prey <- unique(comp$data$prey)
        out <- data.frame(name = sprintf("prey%s", 1:length(prey)),
                          prey = prey,
                          stringsAsFactors = FALSE)
        return(out)
    } else {
        stop("\n\n", "I don't recognize aggfile type", agg_type)
    }
}

#' @rdname likelihood_helper_funs
format_aggfile_attributes <- function(comp, ...) {
    dots <- dots2list(...)
    lik_type <- comp$type
    lik_name <- comp$name
    if (check_names("areaaggfile", comp)) {
        if (!check_names("areaagg", dots)) {
            areaagg <- make_default_aggfile(comp, "area")
        } else if (is.null(dots$areaagg)) {
            areaagg <- make_default_aggfile(comp, "area")
        } else {
            areaagg <- dots$areaagg
        }
        add_aggfile_attribute("area")
    }
    if (check_names("ageaggfile", comp)) {
        if (!check_names("ageagg", dots)) {
            ageagg <- make_default_aggfile(comp, "age")
        } else if (is.null(dots$ageagg)) {
            ageagg <- make_default_aggfile(comp, "age")
        } else {
            ageagg <- dots$ageagg
        }
        add_aggfile_attribute("age")
    }
    if (check_names("lenaggfile", comp)) {
        if (!check_names("lenagg", dots)) {
            lenagg <- make_default_aggfile(comp, "len")
        } else if (is.null(dots$lenagg)) {
            lenagg <- make_default_aggfile(comp, "len")
        } else {
            lenagg <- dots$lenagg
        }
        add_aggfile_attribute("len")
    }
    if (check_names("preyaggfile", comp)) {
        if (!check_names("preyagg", dots)) {
            preyagg <- make_default_aggfile(comp, "prey")
        } else if (is.null(dots$preyagg)) {
            preyagg <- make_default_aggfile(comp, "prey")
        } else {
            preyagg <- dots$preyagg
        }
        add_aggfile_attribute("prey")
    }
    return(comp)
}


#' Create a Gadget likelihood file
#'
#' This function concentrates the various likelihood components created by
#' \code{\link{gadget_likelihood_comps}} and puts them all in a single object
#' ready for writing.
#'
#' @param ... Various Gadget likelihood components desired that are of class
#' \code{gadget_*_likelihood}, see \code{\link{gadget_likelihood_comps}}
#' @param penalty Logical. Should a penalty likelihood component be added
#' @param understocking Logical. Should an understocking likelihood component be
#' added
#'
#' @return A list of class \code{gadget_likelihood} that contains the various
#' likelihood components
#' @export
#'
#' @examples
#' # see examples in ?gadget_likelihood_comps
#' lik_file <- make_gadget_likelihood(spr_ldist_comp, spr_aldist_comp)
make_gadget_likelihood <- function(..., penalty = TRUE, understocking = TRUE) {
    comps <- dots2list(...)
    comp_class_check <-
        lapply(comps, function(x) {
            any(grepl("gadget_\\w*_likelihood", class(x)))
        })
    if (!all(unlist(comp_class_check))) {
        stop("Arguments to make_gadget_likelihood must of the following classes",
             "\n",
             paste(paste0(" *  gadget_",
                          c("penalty", "understocking", "catchdistribution",
                            "catchstatistics", "stockdistribution", "surveyindices",
                            "surveydistribution", "stomachcontent", "recaptures",
                            "recstatistics", "migrationpenatly", "migrationproportion",
                            "catchinkilos"),
                          "_likelihood"), collapse = "\n"), "\n",
             "see ?gadget_likelihood_comp for more details")
    }
    comp_types <-
        lapply(comps, function(x) {
            return(x$type)
        })
    if (understocking & !(any(unlist(comp_types) == "understocking"))) {
        comps <- c(list(understocking_likelihood), comps)
    }
    if (penalty & !(any(unlist(comp_types) == "penalty"))) {
        comps <- c(list(penalty_likelihood), comps)
    }
    # scrape attributes off of each component
    comp_attr <-
        lapply(comps, function(x) {
            tmp <- attributes(x)
            out <- tmp[-grep("^names$|^class$", names(tmp))]
            return(out)
        })
    # strip out attributes as you will combine them at the head list level
    # it is easier for writing this way
    comps <-
        lapply(comps, function(x) {
            attributes(x) <- attributes(x)[grep("^names$|^class$",
                                                names(attributes(x)))]
            return(x)
        })
    attributes(comps) <- c(attributes(comps), unlist(comp_attr, recursive = FALSE))
    return(structure(comps, class = c("gadget_likelihood", "list")))
}

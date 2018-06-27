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
#'                                 ageagg = list(all = 1:20))
#' spr_aldist_comp <-
#'   make_gadget_catchdistribution(name = "spr.aldist",
#'                                 data = spr_aldist,
#'                                 fleetnames = "spr",
#'                                 stocknames = "cod")
make_gadget_penalty <- function(...) {
    comp <- format_lik_comp(penalty_likelihood, ...)
    if (!identical(names(comp),
                   names(rm_null_elements(penalty_likelihood)))) {
        stop("Required names for penalty likelihood are not correct")
    } else {
        check_datafile(comp)
        return(comp)
    }
}

#' @rdname gadget_likelihood_comps
#' @export
make_gadget_understocking <- function(...) {
    comp <- format_lik_comp(understocking_likelihood, ...)
    if (!identical(names(comp),
                   names(rm_null_elements(understocking_likelihood)))) {
        stop("Required names for understocking likelihood are not correct")
    } else {
        return(comp)
    }
}

#' @rdname gadget_likelihood_comps
#' @export
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
    comp <- check_aggfiles(comp,
                           areaagg = areaagg,
                           ageagg = ageagg,
                           lenagg = lenagg)
    return(comp)
}

#' @rdname gadget_likelihood_comps
#' @export
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
    comp <- check_aggfiles(comp,
                           areaagg = areaagg,
                           ageagg = ageagg,
                           lenagg = lenagg)
    return(comp)
}

#' @rdname gadget_likelihood_comps
#' @export
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
    comp <- check_aggfiles(comp,
                           areaagg = areaagg,
                           ageagg = ageagg,
                           lenagg = lenagg)
    return(comp)
}

#' @rdname gadget_likelihood_comps
#' @export
make_gadget_surveyindices <- function(...,
                                      areaagg = NULL,
                                      lenagg = NULL,
                                      ageagg = NULL) {
    dots <- dots2list(...)
    if (!check_names("^sitype$", dots)) {
        stop("\n", "You must declare the sitype for this likelihood component",
             "\n", "see Gadget User Guide - 8.6")
    }
    lik_template <-
        switch(dots$sitype,
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
    } else if (!(comp$sitype %in% si_types)) {
        stop("\n",
             "You must provide a surveyindices type of one of the following",
             "\n",
             paste(paste0(" * ", si_types), collapse = "\n"))
    }
    base_colnames <- c("year", "step", "area")
    data_cols <-
        switch(dots$sitype,
               lengths = c(base_colnames, "length", "number"),
               ages = c(base_colnames, "age", "number"),
               fleets = c(base_colnames, "length", "number"),
               acoustic = c(base_colnames, "survey", "acoustic"),
               effort = c(base_colnames, "fleet", "effort"))
    check_lik_data(comp,
                   data_cols = data_cols,
                   lik_type)
    comp <- check_aggfiles(comp,
                           areaagg = areaagg,
                           ageagg = ageagg,
                           lenagg = lenagg)
    return(comp)
}

#' @rdname gadget_likelihood_comps
#' @export
make_gadget_surveydistribution <- function(...,
                                          areaagg = NULL,
                                          lenagg = NULL,
                                          ageagg = NULL) {
    comp <- format_lik_comp(surveydistribution_likelihood, ...)
    lik_type <- "surveydistribution"
    lik_name <- comp$name
    check_lik_data(comp,
                   data_cols = c("year", "step", "area",
                                 "age", "length", "number"),
                   lik_type)
    check_lik_names(comp, lik_type)
    comp <- check_aggfiles(comp,
                           areaagg = areaagg,
                           ageagg = ageagg,
                           lenagg = lenagg)
    return(comp)
}


#' @rdname gadget_likelihood_comps
#' @export
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
    comp <- check_aggfiles(comp,
                           areaagg = areaagg,
                           lenagg = lenagg,
                           preyagg = preyagg)
    return(comp)
}

#' @rdname gadget_likelihood_comps
#' @export
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
    comp <- check_aggfiles(comp,
                           areaagg = areaagg,
                           lenagg = lenagg)
    return(comp)
}

#' @rdname gadget_likelihood_comps
#' @export
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
    comp <- check_aggfiles(comp,
                           areaagg = areaagg,
                           lenagg = lenagg)
    return(comp)
}


#' @rdname gadget_likelihood_comps
#' @export
make_gadget_migrationpenalty <- function(...) {
    comp <- format_lik_comp(migrationpenalty_likelihood, ...)
    lik_type <- "migrationpenalty"
    lik_name <- comp$name
    check_lik_names(comp, lik_type)
    return(comp)
}

#' @rdname gadget_likelihood_comps
#' @export
make_gadget_migrationproportion <- function(...,
                                   areaagg = NULL) {
    comp <- format_lik_comp(migrationproportion_likelihood, ...)
    lik_type <- "migrationproportion"
    lik_name <- comp$name
    check_lik_data(comp,
                   data_cols = c("year", "step", "area", "ratio"),
                   lik_type)
    check_lik_names(comp, lik_type)
    comp <- check_aggfiles(comp,
                           areaagg = areaagg,
                           lenagg = lenagg)
    return(comp)
}

#' @rdname gadget_likelihood_comps
#' @export
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
    comp <- check_aggfiles(comp,
                           areaagg = areaagg,
                           lenagg = lenagg)
    return(comp)
}

#' @rdname gadget_likelihood_comps
#' @param label Character. The label for each length group
#' @param min Numeric. The minimum length or age value for each label
#' @param max Numeric. The maximum length or age value for each label
#' @export
make_si_aggfile <- function(label, min, max) {
    return(data.frame(label, minimum = min, maximum = max))
}

#' Gadget likelihood helper functions
#'
#' These functions are just helper functions that are used in the various
#' \code{\link{gadget_likelihood_comps}} functions. They are simply for
#' automation purposes to make things cleaner. These are Russian doll functions
#' that have helper functions within helper functions. This might make debugging
#' painful in the future; if this is future-you, then don't do this sort of
#' thing again. If this is another reader...I'm sorry.
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
        stop(sprintf("%s Required names for %s likelihood are not correct",
                     "\n", lik_type))
    }
}

#' @rdname likelihood_helper_funs
check_aggfiles <- function(comp, ...) {
    dots <- dots2list(...)
    lik_type <- comp$type
    lik_name <- comp$name
    agg_checker <- function(agg_type, env = parent.frame()) {
        aggfile_type <- paste0(agg_type, "aggfile")
        agg_arg <- paste0(agg_type, "agg")
        if (check_names(aggfile_type, env$comp)) {
            if (!check_names(agg_arg, env$dots)) {
                env$comp[[aggfile_type]] <-
                    make_default_aggfile(env$comp, agg_type)
            } else if (is.null(env$dots[[agg_arg]])) {
                env$comp[[aggfile_type]] <-
                    make_default_aggfile(env$comp, agg_type)
            } else {
                env$comp[[aggfile_type]] <- dots[[agg_arg]]
            }
        }
    }
    agg_checker("area")
    agg_checker("age")
    agg_checker("len")
    agg_checker("prey")
    comp <- aggregate_data(comp)
    return(comp)
}

#' @rdname likelihood_helper_funs
make_default_aggfile <- function(comp, agg_type) {
    if (agg_type == "area") {
        return(list(all = 1))
    } else if (agg_type == "len") {
        lens <- unique(comp$data$length)
        out <- data.frame(label = sprintf("len%s", lens[-length(lens)]),
                          minimum = lens[-length(lens)],
                          maximum = lens[-1],
                          stringsAsFactors = FALSE)
        return(out)
    } else if (agg_type == "age") {
        ages <- unique(comp$data$age)
        out <- data.frame(label = sprintf("age%s", ages),
                          ages = ages,
                          stringsAsFactors = FALSE)
        return(out)
    } else if (agg_type == "prey") {
        prey <- unique(comp$data$prey)
        out <- list(label = "all",
                    preynames = prey,
                    lengths = c(min(comp$data$length), max(comp$data$length)),
                    digestioncoefficients = c(1, 0, 0))
        return(out)
    } else {
        stop("\n\n", "I don't recognize aggfile type", agg_type)
    }
}

#' @rdname likelihood_helper_funs
check_datafile <- function(comp) {
    if (is.null(comp$data)) {
        stop("You have not entered any data for the ", comp$type,
             " likelihood component.")
    } else if (identical(comp$data, "")) {
        stop("You have not entered any data for the ", comp$type,
             " likelihood component")
    }
}

#' @rdname likelihood_helper_funs
aggregate_data <- function(comp) {
    UseMethod("aggregate_data", comp)
}


aggregate_data.gadget_catchdistribution_likelihood <- function(comp) {
    dat <- comp$data
    dat <- check_aggfile_names(dat)
    if (requireNamespace("dplyr", quietly = TRUE)) {
        `%>%` <- magrittr::`%>%`
        dat <-
            dat %>%
            dplyr::group_by(year, step, area, age, length) %>%
            dplyr::summarize(number = sum(number)) %>%
            dplyr::arrange(year, step, area, age, length) %>%
            as.data.frame()
    } else {
        dat <-
            aggregate(number ~ year + step + area + age + length,
                      data = dat, FUN = sum)
        dat <- dat[order(dat$year, dat$step, dat$area,
                         dat$age, dat$length),]
        rownames(dat) <- 1:nrow(dat)
    }
    comp$data <- dat
    return(comp)
}

aggregate_data.gadget_catchstatistics_likelihood <- function(comp) {
    dat <- comp$data
    dat <- check_aggfile_names(dat)
    if (requireNamespace("dplyr", quietly = TRUE)) {
        `%>%` <- magrittr::`%>%`
        if (comp$`function` == "lengthcalcstddev") {
            dat <-
                dat %>%
                dplyr::group_by(year, step, area, age) %>%
                dplyr::summarize(number = sum(number),
                                 mean = mean(length)) %>%
                dplyr::arrange(year, step, area, age) %>%
                as.data.frame()
        } else if (comp$`function` == "lengthgivenstddev") {
            dat <-
                dat %>%
                dplyr::group_by(year, step, area, age) %>%
                dplyr::summarize(number = sum(number),
                                 mean = mean(length),
                                 sd = sd(length)) %>%
                dplyr::arrange(year, step, area, age) %>%
                as.data.frame()
        } else if (comp$`function` == "weightgivenstddev") {
            dat <-
                dat %>%
                dplyr::group_by(year, step, area, age) %>%
                dplyr::summarize(number = sum(number),
                                 mean = mean(weight),
                                 sd = sd(weight)) %>%
                dplyr::arrange(year, step, area, age) %>%
                as.data.frame()
        } else if (comp$`function` == "weightnostddev") {
            dat <-
                dat %>%
                dplyr::group_by(year, step, area, age) %>%
                dplyr::summarize(number = sum(number),
                                 mean = mean(weight)) %>%
                dplyr::arrange(year, step, area, age) %>%
                as.data.frame()
        } else if (comp$`function` == "lengthnostddev") {
            dat <-
                dat %>%
                dplyr::group_by(year, step, area, age) %>%
                dplyr::summarize(number = sum(number),
                                 mean = mean(length)) %>%
                dplyr::arrange(year, step, area, age) %>%
                as.data.frame()
        } else if (comp$`function` == "weightgivenstddevlen") {
            warning("Sorry, it is not clear in the Gadget User Guide ",
                    "how this function should be aggregated. Nothing is ",
                    "aggregated here.")
        } else if (comp$`function` == "weightnostddevlen") {
            warning("Sorry, it is not clear in the Gadget User Guide ",
                    "how this function should be aggregated. Nothing is ",
                    "aggregated here.")
        } else {
            stop("Sorry, make_gadget_catchstatistics does not recognize ",
                 "function", comp$`function`)
        }
    } else {
        stop("make_gadget_catchstatistics can only properly aggregate data ",
             "when the R package tidyverse is installed. Please do so ",
             "using install.packages('tidyverse')")
    }
    comp$data <- dat
    return(comp)
}

aggregate_data.gadget_stockdistribution_likelihood <- function(comp) {
    dat <- comp$data
    dat <- check_aggfile_names(dat)
    if (requireNamespace("dplyr", quietly = TRUE)) {
        `%>%` <- magrittr::`%>%`
        dat <-
            dat %>%
            dplyr::group_by(year, step, area, stock, age, length) %>%
            dplyr::summarize(number = sum(number)) %>%
            dplyr::arrange(year, step, area, stock, age, length) %>%
            as.data.frame()
    } else {
        dat <-
            aggregate(number ~ year + step + area + stock + age + length,
                      data = dat, FUN = sum)
        dat <- dat[order(dat$year, dat$step, dat$area,
                         dat$stock, dat$age, dat$length),]
        rownames(dat) <- 1:nrow(dat)
    }
    comp$data <- dat
    return(comp)
}

aggregate_data.gadget_surveyindices_likelihood <- function(comp) {
    dat <- comp$data
    dat <- check_aggfile_names(dat)
    if (requireNamespace("dplyr", quietly = TRUE)) {
        `%>%` <- magrittr::`%>%`
        if (comp$sitype == "lengths") {
            dat <-
                dat %>%
                dplyr::group_by(year, step, area, length) %>%
                dplyr::summarize(number = sum(number)) %>%
                dplyr::arrange(year, step, area, length) %>%
                as.data.frame()
        } else if (comp$sitype == "ages") {
            dat <-
                dat %>%
                dplyr::group_by(year, step, area, age) %>%
                dplyr::summarize(number = sum(number)) %>%
                dplyr::arrange(year, step, area, age) %>%
                as.data.frame()
        } else if (comp$sitype == "fleets") {
            dat <-
                dat %>%
                dplyr::group_by(year, step, area, length) %>%
                dplyr::summarize(number = sum(number)) %>%
                dplyr::arrange(year, step, area, length) %>%
                as.data.frame()
        } else if (comp$sitype == "acoustic") {
            dat <-
                dat %>%
                dplyr::group_by(year, step, area, survey) %>%
                dplyr::summarize(acoustic = sum(acoustic)) %>%
                dplyr::arrange(year, step, area, survey) %>%
                as.data.frame()
        } else if (comp$sitype == "effort") {
            dat <-
                dat %>%
                dplyr::group_by(year, step, area, fleet) %>%
                dplyr::summarize(effort = sum(effort)) %>%
                dplyr::arrange(year, step, area, fleet) %>%
                as.data.frame()
        } else {
            stop("Sorry, make_gadget_surveyindices does not recognize ",
                 "surveyindices type", comp$sitype)
        }
    } else {
        if (comp$sitype == "lengths") {
            dat <-
                aggregate(number ~ year + step + area + length,
                          data = dat, FUN = sum)
            dat <- dat[order(dat$year, dat$step, dat$area, dat$length),]
        } else if (comp$sitype == "ages") {
            dat <-
                aggregate(number ~ year + step + area + age,
                          data = dat, FUN = sum)
            dat <- dat[order(dat$year, dat$step, dat$area, dat$age),]
        } else if (comp$sitype == "fleets") {
            dat <-
                aggregate(number ~ year + step + area + length,
                          data = dat, FUN = sum)
            dat <- dat[order(dat$year, dat$step, dat$area, dat$length),]
        } else if (comp$sitype == "acoustic") {
            dat <-
                aggregate(acoustic ~ year + step + area + survey,
                          data = dat, FUN = sum)
            dat <- dat[order(dat$year, dat$step, dat$area, dat$survey),]
        } else if (comp$sitype == "effort") {
            dat <-
                aggregate(effort ~ year + step + area + fleet,
                          data = dat, FUN = sum)
            dat <- dat[order(dat$year, dat$step, dat$area, dat$fleet),]

        } else {
            stop("Sorry, make_gadget_surveyindices does not recognize ",
                 "surveyindices type", comp$sitype)
        }
        rownames(dat) <- 1:nrow(dat)
    }
    comp$data <- dat
    return(comp)
}

aggregate_data.gadget_stomachcontent_likelihood <- function(comp) {
    dat <- comp$data
    dat <- check_aggfile_names(dat)
    if (requireNamespace("dplyr", quietly = TRUE)) {
        `%>%` <- magrittr::`%>%`
        dat <-
            dat %>%
            dplyr::group_by(year, step, area, predator, prey) %>%
            dplyr::summarize(ratio = sum(ratio)) %>%
            dplyr::arrange(year, step, area, predator, prey) %>%
            as.data.frame()
    } else {
        dat <-
            aggregate(ratio ~ year + step + area + predator + prey,
                      data = dat, FUN = sum)
        dat <- dat[order(dat$year, dat$step, dat$area, dat$predator, dat$prey),]
        rownames(dat) <- 1:nrow(dat)
    }
    comp$data <- dat
    return(comp)
}

aggregate_data.gadget_recaptures_likelihood <- function(comp) {
    dat <- comp$data
    dat <- check_aggfile_names(dat)
    if (requireNamespace("dplyr", quietly = TRUE)) {
        `%>%` <- magrittr::`%>%`
        dat <-
            dat %>%
            dplyr::group_by(tagid, year, step, area, length) %>%
            dplyr::summarize(number = sum(number)) %>%
            dplyr::arrange(tagid, year, step, area, length) %>%
            as.data.frame()
    } else {
        dat <-
            aggregate(number ~ tagid + year + step + area + length,
                      data = dat, FUN = sum)
        dat <- dat[order(dat$tagid, dat$year, dat$step, dat$area, dat$length),]
        rownames(dat) <- 1:nrow(dat)
    }
    comp$data <- dat
    return(comp)
}

aggregate_data.gadget_recstatistics_likelihood <- function(comp) {
    dat <- comp$data
    dat <- check_aggfile_names(dat)
    if (requireNamespace("dplyr", quietly = TRUE)) {
        `%>%` <- magrittr::`%>%`
        if (comp$`function` == "lengthcalcstddev") {
            dat <-
                dat %>%
                dplyr::group_by(tagid, year, step, area) %>%
                dplyr::summarize(number = sum(number),
                                 mean = mean(length)) %>%
                dplyr::arrange(tagid, year, step, area) %>%
                as.data.frame()

        } else if (comp$`function` == "lengthgivenstddev") {
            dat <-
                dat %>%
                dplyr::group_by(tagid, year, step, area) %>%
                dplyr::summarize(number = sum(number),
                                 mean = mean(length),
                                 sd = sd(length)) %>%
                dplyr::arrange(tagid, year, step, area) %>%
                as.data.frame()
        } else if (comp$`function` == "lengthnostddev") {
            dat <-
                dat %>%
                dplyr::group_by(tagid, year, step, area) %>%
                dplyr::summarize(number = sum(number),
                                 mean = mean(length)) %>%
                dplyr::arrange(tagid, year, step, area) %>%
                as.data.frame()
        } else {
            stop("make_gadget_recstatistics does not recognize function type ",
                 comp$`function`)
        }
    } else {
        stop("make_gadget_recstatistics can only properly aggregate data ",
             "when the R package tidyverse is installed. Please do so ",
             "using install.packages('tidyverse')")    }
    comp$data <- dat
    return(comp)
}

aggregate_data.gadget_migrationproportion_likelihood <- function(comp) {
    dat <- comp$data
    dat <- check_aggfile_names(dat)
    if (requireNamespace("dplyr", quietly = TRUE)) {
        `%>%` <- magrittr::`%>%`
        dat <-
            dat %>%
            dplyr::group_by(year, step, area) %>%
            dplyr::summarize(ratio = sum(ratio)) %>%
            dplyr::arrange(year, step, area) %>%
            as.data.frame()
    } else {
        dat <-
            aggregate(ratio ~ year + step + area,
                      data = dat, FUN = sum)
        dat <- dat[order(dat$year, dat$step, dat$area),]
        rownames(dat) <- 1:nrow(dat)
    }
    comp$data <- dat
    return(comp)
}

aggregate_data.gadget_catchinkilos_likelihood <- function(comp) {
    dat <- comp$data
    dat <- check_aggfile_names(dat)
    if (requireNamespace("dplyr", quietly = TRUE)) {
        `%>%` <- magrittr::`%>%`
        dat <-
            dat %>%
            dplyr::group_by(year, step, area, fleet) %>%
            dplyr::summarize(biomass = sum(biomass)) %>%
            dplyr::arrange(year, step, area, fleet) %>%
            as.data.frame()
    } else {
        dat <-
            aggregate(biomass ~ year + step + area + fleet,
                      data = dat, FUN = sum)
        dat <- dat[order(dat$year, dat$step, dat$area, dat$fleet),]
        rownames(dat) <- 1:nrow(dat)
    }
    comp$data <- dat
    return(comp)
}

check_aggfile_names <- function(dat, env = parent.frame()) {
    if (check_names("^areaaggfile$", env$comp)) {
        dat$area <-
            names(env$comp$areaaggfile)[match(dat$area, env$comp$areaaggfile)]
    }
    if (check_names("^ageaggfile$", env$comp)) {
        if (all(dat$age == "all")) {
            if (!check_names("all", env$comp$ageaggfile)) {
                stop("Ages in your datafile area aggregated to all ",
                     "but your ageaggfile does not match.", "\n",
                     "Please supply a list with named element `all` ",
                     "containing the ages to aggregate.")
            }
        } else {
            if (is.data.frame(env$comp$ageaggfile)) {
                dat$age <-
                    env$comp$ageaggfile$label[
                        findInterval(dat$age, env$comp$ageaggfile$ages)]
            } else if (is.list(env$comp$ageaggfile)) {
                agg_ages <- env$comp$ageaggfile
                dat_ages <- rep(seq_along(agg_ages),
                                vapply(agg_ages, length, numeric(1)))
                dat$age <- dat_ages[match(dat_ages, unlist(agg_ages))]
            }
        }
    }
    if (check_names("^lenaggfile", env$comp)) {
        dat$length <-
            env$comp$lenaggfile$label[findInterval(dat$length,
                            env$comp$lenaggfile$minimum)]
    }
    # need to add preyaggfile yet here

    return(dat)
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
        stop("Arguments to make_gadget_likelihood must be of the ",
             "following classes",
             "\n",
             paste(paste0(" *  gadget_",
                          c("penalty", "understocking", "catchdistribution",
                            "catchstatistics", "stockdistribution",
                            "surveyindices", "surveydistribution",
                            "stomachcontent", "recaptures", "recstatistics",
                            "migrationpenatly", "migrationproportion",
                            "catchinkilos"),
                          "_likelihood"), collapse = "\n"), "\n",
             "see ?gadget_likelihood_comp for more details")
    }
    comp_types <-
        lapply(comps, function(x) {
            return(x$type)
        })
    if (understocking & !(any(unlist(comp_types) == "understocking"))) {
        comps <- c(list(make_gadget_understocking()), comps)
    }
    if (penalty & !(any(unlist(comp_types) == "penalty"))) {
        comps <- c(list(make_gadget_penalty()), comps)
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
    attributes(comps) <-
        c(attributes(comps), unlist(comp_attr, recursive = FALSE))
    return(structure(comps, class = c("gadget_likelihood", "list")))
}

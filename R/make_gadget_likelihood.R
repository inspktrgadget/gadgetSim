# functions to make gadget likelihood components and likelihood file

make_gadget_penalty <- function(...) {
    penalty_comp <- format_lik_comp(penalty_likelihood, ...)
    if (!identical(names(penalty_comp),
                   names(rm_null_elements(penalty_likelihood)))) {
        stop("Required names for penalty likelihood are not correct")
    } else {
        return(penalty_comp)
    }
}

make_gadget_understocking <- function(...) {
    understocking_comp <- format_lik_comp(understocking_likelihood, ...)
    if (!identical(names(understocking_comp),
                   names(rm_null_elements(understocking_likelihood)))) {
        stop("Required names for understocking likelihood are not correct")
    } else {
        return(understocking_comp)
    }
}

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


make_gadget_migrationpenalty <- function(...) {
    comp <- format_lik_comp(migrationpenalty_likelihood, ...)
    lik_type <- "migrationpenalty"
    lik_name <- comp$name
    check_lik_names(comp, lik_type)
    return(comp)
}

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

format_lik_comp <- function(lik_comp, ...) {
    dots <- dots2list(...)
    comp <- modifyList(lik_comp, dots)
    comp <- rm_null_elements(comp)
}

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

check_lik_names <- function(comp, lik_type) {
    lik_template <- getFromNamespace(paste(lik_type, "likelihood", sep = "_"),
                                     ns = "gadgetSim")
    if (!identical(names(comp), names(rm_null_elements(lik_template)))) {
        stop("Required names for catchdistribution likelihood are not correct")
    }
}

add_aggfile_attribute <- function(agg_type, env = parent.frame()) {
    aggfile_name <- lik_agg_filename(env$lik_type, env$lik_name, agg_type)
    aggfile_type <- paste0(agg_type, "aggfile")
    aggfile <- get(paste0(agg_type, "agg"), envir = env)
    attr(env$comp, aggfile_type) <-
        structure(aggfile,
                  filename = aggfile_name)
    env$comp[[aggfile_type]] <- aggfile_name
}


# helper functions to create the above likelihood components
lik_agg_filename <- function(..., aggfile_dir = "Aggfiles") {
    dots <- dots2list(...)
    return(paste(aggfile_dir, paste(c(dots, c(list("agg"))), collapse = "."), sep = "/"))
}

lik_data_filename <- function(..., datafile_dir = "Data") {
    dots <- dots2list(...)
    return(paste(datafile_dir, paste(dots, collapse = "."), sep = "/"))
}

format_datafile_attribute <- function(env = parent.frame()) {
    data_filename <-
        lik_data_filename(env$lik_type, env$lik_name, env$comp$`function`)
    attr(env$comp, "datafile") <-
        structure(env$comp$data, filename = data_filename)
    env$comp$data <- data_filename
}

make_default_aggfile <- function(comp, agg_type) {
    if (agg_type == "area") {
        return(list(all = 1))
    } else if (agg_type == "len") {
        lens <- comp$data$length
        out <- data.frame(length = sprintf("len%s", lens[-length(lens)]),
                          lower = lens[-length(lens)],
                          upper = lens[-1])
        return(out)
    } else if (agg_type == "age") {
        ages <- comp$data$age
        out <- data.frame(age = sprintf("age%s", ages[-length(ages)]),
                          lower = ages[-length(ages)],
                          upper = ages[-1])
        return(out)
    } else if (agg_type == "prey") {
        prey <- unique(comp$data$prey)
        out <- data.frame(name = sprintf("prey%s", 1:length(prey)),
                          prey = prey)
        return(out)
    } else {
        stop("\n\n", "I don't recognize aggfile type", agg_type)
    }
}

format_aggfile_attributes <- function(comp, ...) {
    dots <- dots2list(...)
    if (check_names("areaaggfile", comp)) {
        if (!check_names("areaagg", dots)) {
            areaagg <- make_default_aggfile(comp, "area")
        } else if (is.null(dots$areaagg)) {
            areaagg <- make_default_aggfile(comp, "area")
        }
        add_aggfile_attribute("area")
    }
    if (check_names("ageaggfile", comp)) {
        if (!check_names("ageagg", dots)) {
            ageagg <- make_default_aggfile(comp, "age")
        } else if (is.null(dots$ageagg)) {
            ageagg <- make_default_aggfile(comp, "age")
        }
        add_aggfile_attribute("age")
    }
    if (check_names("lenaggfile", comp)) {
        if (!check_names("lenagg", dots)) {
            lenagg <- make_default_aggfile(comp, "len")
        } else if (is.null(dots$lenagg)) {
            lenagg <- make_default_aggfile(comp, "len")
        }
        add_aggfile_attribute("len")
    }
    if (check_names("preyaggfile", comp)) {
        if (!check_names("preyagg", dots)) {
            preyagg <- make_default_aggfile(comp, "prey")
        } else if (is.null(dots$preyagg)) {
            preyagg <- make_default_aggfile(comp, "prey")
        }
        add_aggfile_attribute("prey")
    }
    return(comp)
}

## format attributes of various pieces of gadget models
## I have since revamped the entire read, make and write procedure
## these format functions can probably get deleted





#-------------------------------------------------------------------------------
#------------------------------
# the following are helper functions that I stole from make_likelihood_comps
#' @rdname likelihood_helper_funs
add_aggfile_attribute <- function(agg_type, env = parent.frame(), path = NULL) {
    aggfile_name <-
        lik_agg_filename(env$lik_type, env$lik_name, agg_type, path = path)
    aggfile_type <- paste0(agg_type, "aggfile")
    aggfile <- get(paste0(agg_type, "agg"), envir = env)
    attr(env$comp, paste(env$lik_name, aggfile_type, sep = ".")) <-
        structure(aggfile,
                  filename = aggfile_name)
    env$comp[[aggfile_type]] <- aggfile_name
}


#' @rdname likelihood_helper_funs




#' @rdname likelihood_helper_funs
format_datafile_attribute <- function(env = parent.frame(), path = NULL) {
    type <- env$comp$type
    name <- env$comp$name
    data_filename <-
        switch(type,
               catchdistribution = lik_data_filename(type,
                                                     name,
                                                     env$comp$`function`,
                                                     path = path),
               surveyindices = lik_data_filename(type,
                                                 name,
                                                 env$comp$sitype,
                                                 path = path),
               penalty = check_path("Data/bounds.penaltyfile"),
               lik_data_filename(type, name, env$comp$`function`, path = path))
    attr(env$comp, paste(env$lik_name, "datafile", sep = ".")) <-
        structure(env$comp$data, filename = data_filename)
    env$comp$data <- data_filename
    names(env$comp)[names(env$comp) == "data"] <- "datafile"
}



#' @rdname likelihood_helper_funs
format_aggfile_attributes <- function(comp, ..., path = NULL) {
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
        add_aggfile_attribute("area", path = path)
    }
    if (check_names("ageaggfile", comp)) {
        if (!check_names("ageagg", dots)) {
            ageagg <- make_default_aggfile(comp, "age")
        } else if (is.null(dots$ageagg)) {
            ageagg <- make_default_aggfile(comp, "age")
        } else {
            ageagg <- dots$ageagg
        }
        add_aggfile_attribute("age", path = path)
    }
    if (check_names("lenaggfile", comp)) {
        if (!check_names("lenagg", dots)) {
            lenagg <- make_default_aggfile(comp, "len")
        } else if (is.null(dots$lenagg)) {
            lenagg <- make_default_aggfile(comp, "len")
        } else {
            lenagg <- dots$lenagg
        }
        add_aggfile_attribute("len", path = path)
    }
    if (check_names("preyaggfile", comp)) {
        if (!check_names("preyagg", dots)) {
            preyagg <- make_default_aggfile(comp, "prey")
        } else if (is.null(dots$preyagg)) {
            preyagg <- make_default_aggfile(comp, "prey")
        } else {
            preyagg <- dots$preyagg
        }
        add_aggfile_attribute("prey", path = path)
    }
    return(comp)
}

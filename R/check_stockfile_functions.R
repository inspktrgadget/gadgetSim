# functions to check different parts of gadget stockfile and plug in appropriate values
# these functions are solely meant for use within make_gadget_stockfile

check_refweightfile <- function(out) {
    if (is.null(out$refweightfile)) {
        if (out$doesgrow == 0) {
            refweightfile <- list(refweightfile = "")
        } else if (out$doesgrow == 1) {
            if (growthfunction == "lengthvbsimple") {
                rfwt_p <- out$growthparameters[3:4]
                refwgt <- rfwt_p[1] * reflength ^ rfwt_p[2]
                refweightfile <-
                    structure(list(
                        refweightfile = sprintf("Modelfiles/%s.refwgt", dots$stockname)
                        ),
                        refweightfile = data.frame(length = reflengths, weight = refwgt))
            } else {
                warning("Sorry, gadgetSim is not smart enough to creat a refweightfile for
                         growth functions other than lenghtvbsimple yet.
                         You'll have to specify it manually")
            }
        }
    } else {
        refweightfile <- list(refweightfile = out$refweightfile)
    }
    return(refweightfile)
}

check_growthandeatlengths <- function(out) {
    if (is.null(out$growthandeatlengths)) {
        grw_eat_len <- list(growthandeatlengths = lenaggfile)
    } else {
        grw_eat_len <- list(growthandeatlengths = out$growthandeatlengths)
    }
    return(grw_eat_len)
}

check_stock_growth <- function(out) {
    if (out$growth == 1) {
        if (!(grepl("^growthfunction", names(out)))) {
            stop("You set growth equal to 1, but did not specify a growth function")
        }
        grw_ind <- grep("^doesgrow", names(out))
        if (!(grepl("^beta&^maxlengthgroupgrowth", names(out)))) {
            mort_ind <- grep("^naturalmortality", names(out))
            growth <- as.list(out[grw_ind:mort_ind-1])
            if (!(grepl("^beta", names(out)))) {
                beta <- to_gadget_formula(quote(bbin.mult * bbin))
                growth$beta <- beta
            }
            if (!(grepl("^maxlengthgroupgrowth", names(out)))) {
                growth$maxlengthgroupgrowth <- 15
            }
        } else {
            mlgg_ind <- grep("^maxlengthgroupgrowth", names(out))
            growth <- as.list(out[grw_ind:mlgg_ind])
        }
    } else {
        growth <- out$growth
    }
    return(growth)
}



check_stock_m <- function(out) {
    if (is.null(out$naturalmortality)) {
        m <- as.list(naturalmortality = rep(0.2, times = ((out$maxage - out$minage) + 1)))
        warning("You did not specify a natural mortality...setting all ages to 0.2")
    } else {m <- as.list(out$naturalmortality)}
    return(m)
}


check_stock_iseaten <- function(out) {
    if (out$iseaten == 1) {
        if (!(grepl("^preylengths&^energyconent", names(out)))) {
            iseaten <- as.list(out$iseaten)
            if (!(grepl("^preylengths", names(out)))) {
                iseaten$preylengths <- lenaggfile
            }
            if (!(grepl("^energycontent", names(out)))) {
                iseaten$energycontent <- 1
            }
        } else {
            iseaten <- as.list(out$iseaten, out$preylengths, out$energycontent)
        }
    } else {
        iseaten <- list(iseaten = 0)
    }
    return(iseaten)
}

check_stock_predator <- function(out) {
    if (out$doeseat == 1) {
        doeseat_ind <- grep("^doeseat", names(out))
        init_cond_ind <- grep("^initialconditions", names(out))
        predator <- as.list(out[doeseat_ind:(init_cond_ind - 1)])
    } else {
        predator <- list(doeseat = 0)
    }
    return(predator)
}

check_stock_initcond <- function(out) {
    if (!is.null(out)) {
        initcond_ind <- grep("^initialconditions", names(out))
        doesmigrate_ind <- grep("^doesmigrate", names(out))
        if ((doesmigrate_ind - initcond_ind) == 1) {
            ic_class <- class(out$initialconditions)
            if ("data.frame" %in% ic_class) {
                initcond <-
                    list(minage = out$minage, maxage = out$maxage,
                         minlength = out$minlength, maxlength = out$maxlength,
                         dl = out$dl)
                if (length(ic_class) > 1) {
                    initcond[ic_class[1]] <-
                        sprintf("Modelfiles/%s.init.%s", out$stockname, ic_class[1])
                    attr(initcond, "initcond_data") <- out$initialconditions
                } else {
                    stop(cat("Initial conditions data must be of one of the following classes",
                             paste(paste0(" * ",
                                          c("normalcondfile", "normalparamfile", "numberfile")),
                                   sep = "\n"),
                             sep = "\n"))
                }
            } else {
                stop("You have not entered useful information for initial conditions")
            }
        } else {
            initcond <- as.list(out[initcond_ind:(doesmigrate_ind) - 1])
        }
    } else {
        stop("You have not entered any information about initial conditions")
    }
    return(initcond)
}

normalcondfile <- function(...) {
    df <- as.data.frame(dots2list(...))
    class(df) <- c("normalcondfile", "data.frame")
}

normalparamfile <- function(...) {
    df <- as.data.frame(dots2list(...))
    class(df) <- c("normalparamfile", "data.frame")
}

numberfile <- function(...) {
    df <- as.data.frame(dots2list(...))
    class(df) <- c("numberfile", "data.frame")
}

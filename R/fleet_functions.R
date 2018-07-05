# some basic selectivity functions and fleet formulas

#' Selectivity functions
#'
#' Compute various selection probabilities on lengths. \code{logistic_selectivity}
#' produces an asymptotic selection probability based on the logistic function.
#' \code{exponential_l50_selectivity} is the same as \code{logistic_selectivity}.
#' \code{andersen_selectivity} and \code{gamma_selectivity} both produce
#' dome-shaped selectivity curves. Straightline is a simple line and constant
#' is a simple line with no slope.
#'
#' @param length Numeric. Length to evaluate selectivity on
#' @param alpha Numeric. Steepness parameter for \code{logistic} and
#' \code{exponential_l50 functions}, constant proportion for \code{constant}
#' function and slope for \code{straightline} selectivity. Left to right scaling
#' parameter for the \code{gamma} function
#' @param beta Numeric. Intercept for the \code{straightline} function.
#' Controls amount of doming on the \code{gamma} selectivity
#' @param gamma Numeric. Controls doming along with \code{beta} in \code{gamma}
#' selectivity
#' @param l50 Numeric. Half-saturation parameter
#' @param p0-p3 Numeric. These arguments controls the shape of the Andersen
#' selectivity function.
#' @param max_prop Numeric in \eqn{[0,1]}. This function controls the maximum
#' level of selectivity in the \code{logistic} and \code{exponential_l50}
#' selectivity.
#'
#' @return Numeric vector the length of \code{length} ranging from 0 to
#' \code{max_prop}
#' @export
#'
#' @name selectivity
#'
#' @examples
#' lengths <- 1:100
#' curve(logistic_selectivity(x, 0.2, 50, 0.5), 0, 100,
#'       ylim = c(0, 1), ylab = "Selectivity", xlab = "Length")
#' curve(exponential_l50_selectivity(x, 0.2, 50, 0.5), add = TRUE,
#'       col = "red", lty = 2)
#' curve(gamma_selectivity(x, 5, 0.7, 20), add = TRUE, col = 2)
#' curve(andersen_fleet_selectivity(x, 0.1, 0.1, 0.5, 50), add = TRUE, col = 3)
#' curve(constant_selectivity(x, 0.1), add = TRUE, col = 4)
#' curve(straightline_selectivity(x, 0.001, 0.1), add = TRUE, col = 5)
#' legend("topleft", legend = c("logistic", "exponential_l50", "gamma",
#'                               "andersen_fleet", "constant", "straightline"),
#'        col = c(1, "red", 2, 3, 4, 5), lty = c(1,2,1,1,1,1))
constant_selectivity <- function(lengths, alpha) {
    if (alpha > 1) {
        stop("If alpha is >1 you increase the population rather than sample it")
    }
    return(rep(alpha, length(lengths)))
}

#' @rdname selectivity
#' @export
straightline_selectivity <- function(lengths, alpha, beta) {
    selectivity <- (alpha * lengths) + beta
    if (any(selectivity > 1)) {
        stop("The parameters you've entered increase the population rather",
             " than sample it")
    }
    return((alpha * lengths) + beta)
}

#' @rdname selectivity
#' @export
logistic_selectivity <- function(lengths, alpha, l50, max_prop = 1) {
    if (max_prop > 1) {
        stop("If max_prop is >1 you increase the population rather ",
             "than sample it")
    }
    return(max_prop / (1 + exp(-alpha * (lengths - l50))))
}

#' @rdname selectivity
#' @export
exponential_l50_selectivity <- logistic_selectivity

#' @rdname selectivity
#' @export
andersen_fleet_selectivity <- function(lengths, p0, p1, p2, p3) {
    selectivity <- p0 + ((1 - p0) * exp(-(((log(p3 / lengths) - p1)^2) / p2)))
    return(selectivity)
}

#' @rdname selectivity
#' @export
gamma_selectivity <- function(lengths, alpha, beta, gamma) {
    selectivity <-
        ((lengths / ((alpha - 1) * beta * gamma)) ^ (alpha - 1)) *
        exp(alpha - 1 - (lengths / (beta * gamma)))
}


#' Functions to assemble suitability function lines for writing to Gadget files
#'
#' To specify consumption for either prey or fleets Gadget requires suitability
#' information to be displayed in a certain manner (see Section 4.9 in
#' Gadget User Guide). These functions will output the correct format for each
#' respective suitability type. These functions are meant to be used in
#' conjunction when putting together Gadget stocks or fleets.
#'
#' @param fleet Character. The name of the fleet. Used to name switches for
#' suitability parameters
#' @param stock Character. The name of the stock to serve as the prey
#' @param params Optional. List of parameter names as either character vectors
#' or numbers. If character values are given, switches will be returned as
#' function parameters. If numbers are given, then the respective arguments to
#' the functions will be fixed.
#' @param param_names Optional. Character to use to name the params. This is
#' of use if there is more than one stock for the fleet, but the same
#' selectivity curve is desired
#'
#' @return A character vector of function information pasted together in a
#' format ready for writing to Gadget file.
#' @export
#'
#' @name suit_formulas
#'
#' @examples
#' make_exponential_suit("comm", "cod")
#' make_gamma_suit("spr", "cod")
#'
#' # include multiple stocks
#' make_exponential_suit("comm", stock = c("immature", "mature"))
#' # fix a function
#' make_constant_suit("spr", "cod", params = 1)
#'
#' # or more generally
#' make_fleet_suit("comm", "cod", fun = "exponentiall50",
#'                 params = list("alpha", "l50"))
make_constant_suit <- function(fleet, stock, params = list("alpha"),
                               param_names = NULL) {
    return(make_fleet_suit(fleet = fleet, stock = stock, fun = "constant",
                           params = params, param_names = param_names))
}

#' @rdname suit_formulas
#' @export
make_straightline_suit <- function(fleet, stock,
                                   params = list("alpha", "beta"),
                                   param_names = NULL) {
    return(make_fleet_suit(fleet = fleet, stock = stock, fun = "straightline",
                           params = params, param_names = param_names))
}

#' @rdname suit_formulas
#' @export
make_exponential_suit <- function(fleet, stock,
                                  params = list("alpha", "beta",
                                                "gamma", "delta"),
                                  param_names = NULL) {
        return(make_fleet_suit(fleet = fleet, stock = stock,
                               fun = "exponential", params = params,
                               param_names = param_names))
    }

#' @rdname suit_formulas
#' @export
make_exponentiall50_suit <- function(fleet, stock,
                                     params = list("alpha", "l50"),
                                     param_names = NULL) {
    return(make_fleet_suit(fleet = fleet, stock = stock,
                           fun = "newexponentiall50", params = params,
                           param_names = param_names))
}

#' @rdname suit_formulas
#' @export
make_richards_suit <- function(fleet, stock,
                               params = as.list(paste0("p", 0:4)),
                               param_names = NULL) {
    return(make_fleet_suit(fleet = fleet, stock = stock, fun = "richards",
                           params = params, param_names = param_names))
}

#' @rdname suit_formulas
#' @export
make_andersen_suit <- function(fleet, stock,
                               params = as.list(paste0("p", 0:4)),
                               param_names = NULL) {
    return(make_fleet_suit(fleet = fleet, stock = stock, fun = "andersen",
                           params = params, param_names = param_names))
}

#' @rdname suit_formulas
#' @export
make_andersenfleet_suit <- function(fleet, stock,
                                    params = as.list(paste0("p", 0:5)),
                                    param_names = NULL) {
    return(make_fleet_suit(fleet = fleet, stock = stock, fun = "andersenfleet",
                              params = params, param_names = param_names))
}

#' @rdname suit_formulas
#' @export
make_gamma_suit <- function(fleet, stock,
                            params = list("alpha", "beta", "gamma"),
                            param_names = NULL) {
    return(make_fleet_suit(fleet = fleet, stock = stock, fun = "gamma",
                           params = params, param_names = param_names))
}

# remove fleet_suit_formula
# \code{fleet_suit_formula} is now defunct and will be removed.
# Please use \code{make_fleet_suit} or variants thereof
fleet_suit_formula <- function(fleet="comm", stock=NULL, fun="exponentiall50",
                               params=NULL, param_names = NULL) {
    if (is.null(param_names)) {
        params2paste <-
            lapply(stock, function(x) {
                tmp <-
                    lapply(params, function(y) {
                        if (is.numeric(y)) {
                            return(y)
                        } else {
                            return(sprintf("#%1$s.%2$s.%3$s", x, fleet, y))
                        }
                    })
                return(paste(tmp, collapse = "\t"))
            })
    } else {
        params2paste <-
            lapply(seq_along(stock), function(x) {
                tmp <-
                    lapply(params, function(y) {
                        if (is.numeric(y)) {
                            return(y)
                        } else {
                            return(sprintf("#%1$s.%2$s.%3$s", param_names,
                                           fleet, y))
                        }
                })
                return(paste(tmp, collapse = "\t"))
            })

    }
    fun_call <- paste(stock, "function", fun, params2paste, sep = "\t")
    return(paste(fun_call, sep = "\n"))
}

#' @rdname suit_formulas
make_fleet_suit <- function(fleet="comm", stock=NULL, fun="exponentiall50",
                               params=NULL, param_names = NULL) {
    if (is.null(param_names)) {
        params2paste <-
            lapply(stock, function(x) {
                tmp <-
                    lapply(params, function(y) {
                        if (is.numeric(y)) {
                            return(y)
                        } else {
                            return(sprintf("#%1$s.%2$s.%3$s", x, fleet, y))
                        }
                    })
                return(unlist(tmp))
            })
    } else {
        params2paste <-
            lapply(seq_along(stock), function(x) {
                tmp <-
                    lapply(params, function(y) {
                        if (is.numeric(y)) {
                            return(y)
                        } else {
                            return(sprintf("#%1$s.%2$s.%3$s", param_names,
                                           fleet, y))
                        }
                    })
                return(unlist(tmp))
            })

    }
    dat <- data.frame(stock = stock, fun = fun)
    params <- t(as.data.frame(params2paste))
    dat <- cbind(dat, params)
    rownames(dat) <- 1:nrow(dat)
    return(dat)
}
#' Compute various per-recruit analyses
#'
#' These functions compute yield-per-recruit, spawning stock biomass (ssb) per
#' recruit, and an overall yield curve given fishing mortality, natural
#' mortality, ages, the growth function, the length-weight relationship,
#' recruitment, selectivity, and maturity.
#'
#' \code{ypr_curve} produces a vector of yield-per-recruit at each value in
#' \code{fish_mort}, \code{ssb_rec_curve} returns a vector with values for
#' spawning-stock-biomass or spawners per recruit, and \code{yield_curve}
#' provides a vector with values for yield corresponding to each value in
#' \code{fish_mort}
#'
#' @param fish_mort Numeric. A vector depicting fishing mortality
#' @param nat_mort Numeric. A vector of either length 1 or the same length as
#' \code{ages}
#' @param ages Numeric vector representing ages
#' @param growth_fun The name of the growth function to be used
#' @param growth_params List of named elements corresponding to arguments to
#' \code{growth_fun}
#' @param lw_rel Function name determining the length-weight relationship
#' @param lw_params List of named elements corresponding to arguments to
#' \code{lw_rel}
#' @param sel_fun Function determining the length-based selectivity of the fleet
#' @param sel_params List of named elements corresponding to arguments to
#' \code{sel_fun}
#'
#' @return A vector the same length as \code{fish_mort} which corresponds to
#' the ypr, ssb per recruit, or overall yield at the respective value of
#' \code{fish_mort}
#' @name yield_curves
#'
#' @export
#'
#' @examples
#' curve(ypr_curve(fish_mort = x, nat_mort = 0.2, ages = 0:20,
#'                 growth_fun = vb,
#'                 growth_params = list(linf = 125, k = 0.15, t0 = -0.5),
#'                 lw_rel = lw, lw_params = list(alpha = 7e-06, beta = 3.1),
#'                 sel_fun = logistic_selectivity,
#'                 sel_params = list(alpha = 0.25, l50 = 40)),
#'       from = 0, to = 2, ylab = "YPR")
#' curve(ssb_rec_curve(fish_mort = x, nat_mort = 0.2, ages = 0:20,
#'                     growth_fun = "vb",
#'                     growth_params = list(linf = 125, k = 0.15, t0 = -0.5),
#'                     lw_rel = "lw",
#'                     lw_params = list(alpha = 7e-06, beta = 3.1),
#'                     mat_fun = "logistic_selectivity",
#'                     mat_params = list(alpha = 0.25, l50 = 30),
#'                     sel_fun = "logistic_selectivity",
#'                     sel_params = list(alpha = 0.25, l50 = 40)),
#'       from = 0, to = 2, ylab = "SSB per recruit")
ypr_curve <- function(fish_mort, nat_mort, ages,
                     growth_fun = vb, growth_params = NULL,
                     lw_rel = lw, lw_params = NULL,
                     sel_fun = logistic_selectivity, sel_params = NULL) {
    # check to see if nat_mort is a constant or not
    if (length(nat_mort) != length(ages)) {
        if (length(nat_mort) == 1) {
            nat_mort <- rep(nat_mort, length(ages))
        } else {
            stop("Natural mortality must be a vector the same length as ",
                 "ages or a constant")
        }
    }
    yr_at_f <- vapply(fish_mort, function(x) {
        length_at_age <-
            do.call(growth_fun, c(growth_params, list(age = ages)))
        weight_at_age <-
            do.call(lw_rel, c(lw_params, list(length = length_at_age)))
        f_at_age <- do.call(sel_fun, c(list(lengths = length_at_age),
                                       sel_params)) * x
        z_at_age <- f_at_age + nat_mort
        z_temp <- c(0, z_at_age[1:(length(z_at_age) - 1)])
        yield <- sum((f_at_age / z_at_age) *
                         (1 - exp(-z_at_age)) *
                         weight_at_age *
                         exp(-cumsum(z_temp)))
        return(yield)
    }, numeric(1))
    return(yr_at_f)
}

#' @rdname yield_curves
#' @export
#' @param sel_fun Function determining maturity ogive of the stock
#' @param sel_params List of named elements corresponding to arguments to
#' \code{sel_fun}
ssb_rec_curve <- function(fish_mort, nat_mort, ages,
                          growth_fun = vb, growth_params = NULL,
                          lw_rel = lw, lw_params = NULL,
                          mat_fun = logistic_selectivity, mat_params = NULL,
                          sel_fun = logistic_selectivity, sel_params = NULL) {
    # check to see if nat_mort is a constant or not
    if (length(nat_mort) != length(ages)) {
        if (length(nat_mort) == 1) {
            nat_mort <- rep(nat_mort, length(ages))
        } else {
            stop("Natural mortality must be a vector the same length as ages ",
                 "or a constant")
        }
    }
    ssb_at_f <- vapply(fish_mort, function(x) {
        length_at_age <-
            do.call(growth_fun, c(growth_params, list(age = ages)))
        weight_at_age <-
            do.call(lw_rel, c(lw_params, list(length = length_at_age)))
        pmat_at_age <-
            do.call(mat_fun, c(mat_params, list(length = length_at_age)))
        f_at_age <- do.call(sel_fun, c(list(lengths = length_at_age),
                                       sel_params, max_prop = 1)) * x
        z_at_age <- f_at_age + nat_mort
        z_temp <- c(0, z_at_age[1:(length(z_at_age) - 1)])
        ssb <- sum(weight_at_age * pmat_at_age * exp(-cumsum(z_temp)))
        return(ssb)
    }, numeric(1))
    return(ssb_at_f)
}

#' @rdname yield_curves
#' @export
#' @examples
#' curve(yield_curve(fish_mort = x, nat_mort = 0.2, ages = 0:20,
#'                   growth_params = list(linf = 125, k = 0.15, t0 = -0.5),
#'                   lw_params = list(alpha = 7e-06, beta = 3.1),
#'                   rec_params = list(mu = 4e8, lambda = 1.067e08),
#'                   mat_params = list(alpha = 0.25, 30),
#'                   sel_params = list(alpha = 0.25, 40)), 0, 2, ylab = "Yield")
yield_curve <- function(fish_mort, nat_mort, ages,
                        growth_fun = vb, growth_params = NULL,
                        lw_rel = lw, lw_params = NULL,
                        rec_fun = bev_holt, rec_params = NULL,
                        mat_fun = logistic_selectivity, mat_params = NULL,
                        sel_fun = logistic_selectivity, sel_params = NULL,
                        ...) {
    # check to see if nat_mort is a constant or not
    if (length(nat_mort) != length(ages)) {
        if (length(nat_mort) == 1) {
            nat_mort <- rep(nat_mort, length(ages))
        } else {
            stop("Natural mortality must be a vector the same length as ages ",
                 "or a constant")
        }
    }
    yield <-
        vapply(fish_mort, function(x) {
            ypr <- ypr_curve(fish_mort = x, nat_mort = nat_mort, ages = ages,
                             growth_fun = growth_fun,
                             growth_params = growth_params,
                             lw_rel = lw_rel, lw_params = lw_params,
                             sel_fun = sel_fun, sel_params = sel_params)
            sr <- ssb_rec_curve(fish_mort = x, nat_mort = nat_mort, ages = ages,
                                growth_fun = growth_fun,
                                growth_params = growth_params,
                                lw_rel = lw_rel, lw_params,
                                mat_fun = mat_fun, mat_params = mat_params,
                                sel_fun = sel_fun, sel_params = sel_params)
            if (identical(match.fun(rec_fun), match.fun(bev_holt))) {
                spawners <- (rec_params$mu * sr) - rec_params$lambda
            } else if (identical(match.fun(rec_fun), match.fun(ricker))) {
                spawners <- (log(rec_params$mu) * log(sr)) / rec_params$lambda
            } else {
                if (!("spawner_sr_fun" %in% names(list(...)))) {
                    stop("If not using Beverton-Holt or Ricker recruitment,",
                         "\n",
                         "you must specify your own function to calculate",
                         "\n",
                         "spawners from the spawners-per-recruit curve as",
                         "\n",
                         "spawner_sr_fun and spawner_sr_params")
                } else {
                    spawners <-
                        do.call(spawner_sr_fun, c(sr, spawner_sr_params))
                }
            }
            recruits <- do.call(rec_fun, c(spawners, rec_params))
            return(ypr * recruits)
        }, numeric(1))
    return(yield)
}



#' Compute various fishing mortality reference point
#'
#' These functions will compute a variety of reference points relating to
#' fishing mortality. Various parameters about the stock must be entered as
#' these use yield-per-recruit analysis to determing the reference points.
#'
#' \code{f_msy} will determine the fishing mortality that produces maximum
#' sustained yield, \code{f_crash} returns the fishing mortality that will make
#' the stock go extinct (i.e. crash the stock), \code{f_0.1} produces the
#' fishing mortality that is roughly 1/10th of the slope at the origin of the
#' yield-per-recruit curve. \code{f_lim} will find both of the fishing
#' mortalities to which \eqn{F_{msy}} is scaled by \code{limit}
#'
#' @inheritParams yield_curves
#' @param ...
#'
#' @return A numeric vector of length 1, except for \code{f_lim}, which returns
#' 2 values
#' @export
#'
#' @rdname f_values
#'
#' @examples
#' f_msy(fish_mort = seq(0, 2, 0.01), nat_mort = 0.2, ages = 0:20,
#'       growth_fun = vb,
#'       growth_params = list(linf = 125, k = 0.15, t0 = -0.5),
#'       lw_rel = lw, lw_params = list(alpha = 7e-06, beta = 3.1),
#'       rec_fun = bev_holt,
#'       rec_params = list(mu = 4e8, lambda = 1.7e08),
#'       mat_fun = logistic_selectivity,
#'       mat_params = list(alpha = 0.25, l50 = 30),
#'       sel_fun = logistic_selectivity,
#'       sel_params = list(alpha = 0.25, l50 = 40))
f_msy <- function(fish_mort, nat_mort, ages,
                  growth_fun = vb, growth_params = NULL,
                  lw_rel = lw, lw_params = NULL,
                  rec_fun = bev_holt, rec_params = NULL,
                  mat_fun = logistic_selectivity, mat_params = NULL,
                  sel_fun = logistic_selectivity, sel_params = NULL,
                  ...) {
    yield <- yield_curve(fish_mort = fish_mort, nat_mort = nat_mort,
                         ages = ages, growth_fun = growth_fun,
                         growth_params = growth_params,
                         lw_rel = lw_rel, lw_params = lw_params,
                         rec_fun = rec_fun, rec_params,
                         mat_fun = mat_fun, mat_params,
                         sel_fun = sel_fun, sel_params = sel_params, ...)
    return(fish_mort[which.max(yield)])
}

#' @rdname f_values
#' @export
f_crash <- function(fish_mort, nat_mort, ages,
                    growth_fun = vb, growth_params = NULL,
                    rec_fun = bev_holt, rec_params = NULL,
                    lw_rel = lw, lw_params = NULL,
                    mat_fun = logistic_selectivity, mat_params = NULL,
                    sel_fun = logistic_selectivity, sel_params = NULL) {
    ssb <- seq(1, 4e9, by = 1e6)
    rec <- do.call(rec_fun, c(list(ssb = ssb), rec_params))
    # rough estimation of derivative of the S-R curve
    sr_derivative <- diff(rec) / diff(ssb)
    slope_at_origin <- sr_derivative[1]
    # match up arguments with ssb_rec_curve
    sr_curve <- ssb_rec_curve(fish_mort = fish_mort, nat_mort = nat_mort,
                              ages = ages, growth_fun = growth_fun,
                              growth_params = growth_params,
                              lw_rel = lw_rel, lw_params = lw_params,
                              mat_fun = mat_fun, mat_params = mat_params,
                              sel_fun = sel_fun, sel_params = sel_params)
    # determine f_crash
    crash <- (1 / slope_at_origin)
    crash_curve <- rev(sort(c(sr_curve, crash)))
    f_crash <- fish_mort[which(crash_curve == crash) + 1]
    return(f_crash)
}

#' @rdname f_values
#' @export
f_0.1 <- function(fish_mort, nat_mort, ages,
                  growth_fun = vb, growth_params = NULL,
                  lw_rel = lw, lw_params = NULL,
                  sel_fun = logistic_selectivity, sel_params = NULL) {
    ypr <- ypr_curve(fish_mort = fish_mort, nat_mort = nat_mort, ages = ages,
                     growth_fun = growth_fun, growth_params = growth_params,
                     lw_rel = lw_rel, lw_params = lw_params,
                     sel_fun = sel_fun, sel_params = sel_params)
    dypr_df <- diff(ypr) / diff(fish_mort)
    slope_at_origin <- dypr_df[1]
    slope_0.1 <- max(dypr_df[dypr_df <= (slope_at_origin * 0.1)])
    return(fish_mort[which(dypr_df == slope_0.1)])
}


#' @rdname f_values
#' @export
f_lim <- function(fish_mort, limit, nat_mort, ages,
                  growth_fun = vb, growth_params = NULL,
                  lw_rel = lw, lw_params = NULL,
                  rec_fun = bev_holt, rec_params = NULL,
                  mat_fun = logistic_selectivity, mat_params = NULL,
                  sel_fun = logistic_selectivity, sel_params = NULL,
                  ...) {
    yield <- do.call("yield_curve", as.list(sys.call())[-1])
    msy <- max(yield)
    ref_pt <- msy * limit
    lhs <- yield[1:which(yield == msy)]
    rhs <- yield[which(yield == msy):length(yield)]
    lower_pt <- which.min(abs(lhs - ref_pt))
    upper_pt <- which.min(abs(rhs - ref_pt))
    ref_pts <- c(lower_pt, (upper_pt + length(lhs)))
    return(return(fish_mort[ref_pts]))
}

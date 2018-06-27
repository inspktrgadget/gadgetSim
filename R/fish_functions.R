# growth and recruitment functions and formulas


#' Basic functions for growth curves
#'
#' \code{vb} produces the von Bertalanffy growth curve for length at age
#' \deqn{L_{\infty} * (1 - \exp(-k * (a - t_0)))} where \eqn{a} is age. \code{lw} returns the length-weight
#' relationship as determined by parameters \eqn{\alpha} and \eqn{\beta} in the function
#' \deqn{\alpha * L^{\beta}}, where \eqn{L} is length.
#'
#' @param linf Numeric. Parameter specifying aximum length at age
#' @param k Numeric. Parameter specifying steepness of the growth curve
#' @param t0 Numeric. Shifting parameter that specifies theoretical age at length 0
#' @param age Numeric vector of ages
#'
#' @return A numeric vector corresponding to mean length-at-age for \code{vb} or mean
#' weight-at-length for \code{lw}
#'
#' @name growth_funs
#' @export
#'
#'
#' @examples
#' age <- 1:20
#' length <- vb(125, 0.15, -0.5, age)
#' wt <- lw(length, 1e-06, 3)
vb <- function(linf, k, t0, age) {
    linf * (1 - exp(-k * (age - t0)))
}

#' @rdname growth_funs
#' @export
lw <- function(alpha, beta, length) {
    return(alpha * (length ^ beta))
}



#' Basic functions for recruitment curves
#'
#' These functions produce the basic stock-recruitment curves used by Gadget. \code{simple_ssb}
#' uses the equation \eqn{\mu * S}, \code{ricker} returns the number of recruits per spawning
#' stock biomass \eqn{S} per the Ricker formula: \deqn{R = \mu S e^{-\lambda S}}. \code{bev_holt}
#' returns the number of recruits using the Beverton-Holt recruitment curve
#' \deqn{R = (\mu * S)(\lambda + S)}
#'
#' @param ssb Numeric. The spawning stock biomass used to compute the recruitment curve
#' @param mu Numeric. Parameter that controls maximum number of recruits for \code{bev_holt} and
#' \code{ricker}
#'
#' @return A numeric vector the length of \code{ssb}
#'
#' @name recruit_funs
#' @export
#'
#' @examples
#' curve(ricker(x, 1000, 0.1), 0, 100, xlab = "SSB", ylab = "R")
#' curve(bev_holt(x, 3000, 10), add = TRUE, col = "red")
simple_ssb <- function(ssb, mu) {
    return(mu * ssb)
}

#' @rdname recruit_funs
#' @param lambda Numeric. Controls density dependence in \code{ricker} curve and steepness in
#' \code{bev_holt}
#' @export
ricker <- function(ssb, mu, lambda) {
    return(mu * ssb * exp(-(lambda) * ssb))
}

#' @rdname recruit_funs
#' @export
bev_holt <- function(ssb, mu, lambda) {
    return((mu * ssb) / (lambda + ssb))
}





#' Produce growth functions in a gadget readable formula
#'
#' @param stockname Character. The stockname desired for variables in the von Bertalanffy function
#' @param age Numeric vector of ages
#'
#' @return A character vector of the von Bertalanffy function the same length as \code{age}.
#' Format is as a Gadget formula, see \code{\link{to_gadget_formula}}
#'
#' @name growth_fun_formulas
#'
#' @export
#'
#' @examples
#' vb_formula("cod", 1:2)
vb_formula <- function(stockname, age, params = NULL) {
    if (!is.null(params)) {
        if (!(length(params) == 3)) {
            stop("Params must be a vector consisting of the following parameters", "\n",
                 paste(paste0(" * ", c("linf", "k", "t0")), collapse = "\n"), "\n",
                 "in that order (or named)")
        }
        if (is.list(params)) {
            params <- unlist(params)
        }
        param_check <- function(param_name, index) {
            if (check_names(param_name, params)) {
                return(params[names(params) == param_name])
            } else {
                return(params[index])
            }
        }
        linf <- param_check("linf", 1)
        k <- param_check("k", 2)
        t0 <- param_check("t0", 3)
    } else {
        add_stockname <- function(arg) {
            return(sprintf("%s.%s", stockname, arg))
        }
        linf <- add_stockname("linf")
        k <- add_stockname("k")
        t0 <- add_stockname("t0")
    }
    vonb <-
        as_quoted_(
            paste0(linf,
                   " * (1 - exp((-1 *",
                   k,
                   ") * (",
                   age,
                   " - ",
                   t0,
                   ")))"))
    sapply(vonb, to_gadget_formula)
}

#' @rdname growth_fun_formulas
#' @export
vb_recl_formula <- function(stockname, age, params = NULL) {
    if (!is.null(params)) {
        if (!(length(params) == 3)) {
            stop("Params must be a vector consisting of the following parameters", "\n",
                 paste(paste0(" * ", c("linf", "k", "recl")), collapse = "\n"), "\n",
                 "in that order (or named)")
        }
        if (is.list(params)) {
            params <- unlist(params)
        }
        param_check <- function(param_name, index) {
            if (check_names(param_name, params)) {
                return(params[names(params) == param_name])
            } else {
                return(params[index])
            }
        }
        linf <- param_check("linf", 1)
        k <- param_check("k", 2)
        t0 <- param_check("recl", 3)
    } else {
        add_stockname <- function(arg) {
            return(sprintf("%s.%s", stockname, arg))
        }
        linf <- add_stockname("linf")
        k <- add_stockname("k")
        recl <- add_stockname("recl")
    }
    add_stockname <- function(arg) {
        return(sprintf("%s.%s", stockname, arg))
    }
    vonb <-
        as_quoted_(
            paste0(linf,
                   "* (1 - exp((-1 *",
                   k,
                   ") * (",
                   age,
                   "- (1 + (log(1 - (",
                   recl,
                   "/",
                   linf,
                   ")))/",
                   k,
                   "))))"))
    sapply(vonb, to_gadget_formula)
}

#' Produce recruitment functions in a Gadget readable formula
#'
#' These functions will produce recruitment curves such as Beverton-Holt or
#' Ricker in a format that gadgetSim can use to output to files. These are for
#' use in creating spawnfiles.
#'
#' @param stockname A character vector of a stockname that will precede the switches for \eqn{\mu} and
#' \eqn{\lambda} arguments, or, optionally, a numberic vector of length 1 if SimpleSSB is desired or
#' of length 2 that will be used as \eqn{\mu} and \eqn{\lambda} for either the Ricker or
#' Beverton-Holt recruitment functions to fix those parameter values
#'
#' @return A data.frame describing the function and parameters
#' @export
#'
#' @name recruit_formulas
#'
#' @examples
#' bev_holt_formula("cod")
#' bev_holt_formula(c(4e9, 1e8))
#' ricker_formula("cod")
simple_ssb_formula <- function(stockname, params = NULL) {
    if (!is.null(params)) {
        if (length(params) != 1) {
            stop("You must enter a single parameter (mu) for SimpleSSB recruitment")
        }
        mu <- as.vector(params)
    } else {
        mu <- c(paste0("#", stockname, "simplessb.mu"))
    }
    return(data.frame(fun = "simplessb", mu))
}

#' @rdname recruit_formulas
#' @export
ricker_formula <- function(stockname, params = NULL) {
    if (!is.null(params)) {
        if (length(params) != 2) {
            stop("You must enter mu and lambda for Ricker Recruitment")
        }
        mu <- as.vector(params[1])
        lam <- as.vector(params[2])
    } else {
        mu <- c(paste0("#", stockname, ".ricker.mu"))
        lam <- c(paste0("#", stockname, ".ricker.lam"))
    }
    return(data.frame(fun = "ricker", mu, lam))
}

#' @rdname recruit_formulas
#' @export
bev_holt_formula <- function(stockname, params = NULL) {
    if (!is.null(params)) {
        if (length(params) != 2) {
            stop("If any parameters are entered, the you must enter mu and ",
                 "lambda for Beverton-Holt Recruitment")
        }
        mu <- as.vector(params[1])
        lam <- as.vector(params[2])
    } else {
        mu <- c(paste0("#", stockname, ".bh.mu"))
        lam <- c(paste0("#", stockname, ".bh.lam"))
    }
    return(data.frame(fun = "bevertonholt", mu, lam))
}

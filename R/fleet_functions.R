# some basic selectivity functions and fleet formulas

#' Selectivity functions
#'
#' Compute various selection probabilities on lengths. \code{logistic_selectivity} produces an
#' asymptotic selection probability based on the logistic function.
#' \code{exponential_l50_selectivity} is the same as \code{logistic_selectivity}.
#' \code{andersen_selectivity} and \code{gamma_selectivity} both produce dome-shaped selectivity
#' curves. Straightline is a simple line and constant is a simple line with no slope.
#'
#' @param length Numeric. Length to evaluate selectivity on
#' @param alpha Numeric. Steepness parameter for \code{logistic} and \code{exponential_l50 functions},
#' constant proportion for \code{constant} function and slope for \code{straightline} selectivity.
#' Left to right scaling parameter for the \code{gamma} function
#' @param beta Numeric. Intercept for the \code{straightline} function. Controls amoung of doming
#' on the \code{gamma} selectivity
#' @param gamma Numeric. Controls doming along with \code{beta} in \code{gamma} selectivity
#' @param l50 Numeric. Half-saturation parameter
#' @param p0-p3 Numeric. These arguments controls the shape of the Andersen selectivity function.
#' @param max_prop Numeric in \eqn{[0,1]}. This function controls the maximum level of selectivity
#' in the \code{logistic} and \code{exponential_l50} selectivity.
#'
#' @return Numeric vector the length of \code{length} ranging from 0 to \code{max_prop}
#' @export
#'
#' @name selectivity
#'
#' @examples
#' lengths <- 1:100
#' curve(logistic_selectivity(x, 0.2, 50, 0.5), 0, 100,
#'       ylim = c(0, 1), ylab = "Selectivity", xlab = "Length")
#' curve(exponential_l50_selectivity(x, 0.2, 50, 0.5), add = TRUE, col = "red", lty = 2)
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
        stop("The parameters you've entered increase the population rather than sample it")
    }
    return((alpha * lengths) + beta)
}

#' @rdname selectivity
#' @export
logistic_selectivity <- function(lengths, alpha, l50, max_prop) {
    if (max_prop > 1) {
        stop("If max_prop is >1 you increase the population rather than sample it")
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
#' To specify consumption for either prey or fleets Gadget requires suitability information to
#' be displayed in a certain manner (see Section 4.9 in Gadget User Guide). These functions will
#' output the correct format for each respective suitability type. These functions are meant to be
#' used in conjunction when putting together Gadget stocks or fleets.
#'
#' @param fleet Character. The name of the fleet. Used in naming switches for suitability parameters
#' @param stock Character. The name of the stock to serve as the prey
#' @param params Optional. List of parameter names as either character vectors or numbers. If
#' character values are given, switches will be returned as function parameters. If numbers are given,
#' then the respective arguments to the functions will be fixed.
#'
#' @return A character vector of function information pasted together in a format ready for writing
#' to Gadget file.
#' @export
#'
#' @name suit_formulas
#'
#' @examples
#' exponential_suit_formula("comm", "cod")
#' gamma_suit_formula("spr", "cod")
#'
#' # include multiple stocks
#' exponential_suit_formula("comm", stock = c("immature", "mature"))
#' # fix a function
#' constant_suit_formula("spr", "cod", params = 1)
#'
#' # or more generally
#' fleet_suit_formula("comm", "cod", fun = "exponentiall50", params = list("alpha", "l50"))
constant_suit_formula <- function(fleet, stock, params = list("alpha")) {
    return(fleet_suit_formula(fleet = fleet, stock = stock, fun = "constant", params = params))
}

#' @rdname suit_formulas
straightline_suit_formula <- function(fleet, stock, params = list("alpha", "beta")) {
    return(fleet_suit_formula(fleet = fleet, stock = stock, fun = "straightline", params = params))
}

#' @rdname suit_formulas
exponential_suit_formula <- function(fleet, stock, params = list("alpha", "beta", "gamma", "delta")) {
    return(fleet_suit_formula(fleet = fleet, stock = stock, fun = "exponential", params = params))
}

#' @rdname suit_formulas
exponentiall50_suit_formula <- function(fleet, stock, params = list("alpha", "l50")) {
    return(fleet_suit_formula(fleet = fleet, stock = stock, fun = "newexponentiall50", params = params))
}

#' @rdname suit_formulas
richards_suit_formula <- function(fleet, stock, params = as.list(paste0("p", 0:4))) {
    return(fleet_suit_formula(fleet = fleet, stock = stock, fun = "richards", params = params))
}

#' @rdname suit_formulas
andersen_suit_formula <- function(fleet, stock, params = as.list(paste0("p", 0:4))) {
    return(fleet_suit_formula(fleet = fleet, stock = stock, fun = "andersen", params = params))
}

#' @rdname suit_formulas
andersenfleet_suit_formula <- function(fleet, stock, params = as.list(paste0("p", 0:5))) {
    return(fleet_suit_formula(fleet = fleet, stock = stock, fun = "andersenfleet", params = params))
}

#' @rdname suit_formulas
gamma_suit_formula <- function(fleet, stock, params = list("alpha", "beta", "gamma")) {
    return(fleet_suit_formula(fleet = fleet, stock = stock, fun = "gamma", params = params))
}

#' @rdname suit_formulas
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
                            return(sprintf("#%1$s.%2$s.%3$s", param_names, fleet, y))
                        }
                })
                return(paste(tmp, collapse = "\t"))
            })

    }
    fun_call <- paste(stock, "function", fun, params2paste, sep = "\t")
    return(paste(fun_call, sep = "\n"))
}

# some basic selectivity functions

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

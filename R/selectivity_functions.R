# some basic selectivity functions

#' Selectivity functions
#'
#' Compute various selection probabilities on lengths. \code{logistic_selectivity} produces an asymptotic selection probability based on the
#' logistic function: \deqn{S(l) = proportion / (1 + exp(-\alpha(l - l50)))}
#'
#' @param length Numeric. Length to evaluate selectivity on
#' @param alpha Numeric. Steepness parameter
#' @param l50 Numeric. Half-saturation parameter
#' @param max_prop Numeric.
#'
#' @return Numeric vector the length of \code{length} ranging from 0 to \code{max_prop}
#' @export
#'
#' @name selectivity
#'
#' @examples
#' lengths <- 1:100
#' logistic_selectivity(lengths, 0.2, 50, 0.1)
#'
#' curve(logistic_selectivity(x, 0.2, 50, 0.1))
logistic_selectivity <- function(length, alpha, l50, max_prop) {
    if (max_prop > 1) {
        stop("If max_prop is >1 you increase the population rather than sample it")
    }
    return(max_prop / (1 + exp(-alpha * (length - l50))))
}

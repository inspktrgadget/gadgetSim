# functions used in sampling output from gadget

#' Create length distributions and sample Gadget output
#'
#' These functions create length distributions from Gadget StockStdPrinter output and sample
#' that output to simulate surveys. Error can be added to simulated surveys in \code{survey_gadget}
#' and age and length subsamples can be taken with \code{strip_age_length_data} to more closely
#' mimic real world surveys
#'
#'
#' @param stock_data A \code{data.frame} of stock data retrieved via \code{\link{get_stock_std}}
#' or \code{\link{read_gadget_stock_std}}
#' @param length_groups Numeric vector of length groups to distribute by. Probably should be the
#' same as dl in Gadget model
#' @param keep_zero_counts Logical. Keep year/step/area/age combinations with no individuals
#'
#' @details Length-structured population information from Gadget is output as mean length and standard
#' deviation for each year, step, area, and age combination. \code{add_lengthgroups} takes
#' output from \code{\link{get_stock_std}} (or \code{\link{read_gadget_stock_std}}) and
#' distributes the number for each respective combination into numbers at each length
#' specified by \code{length_groups}. The return value for \code{add_lengthgroups} is a
#' wide \code{data.frame} that can then be fed into \code{survey_gadget}, which simulates
#' surveys given the selectivity provided in \code{survey_suitability} and error given by
#' \code{survey_sigma}. If it is desired to have a subsample of length and age data to more closely
#' mimic real world scenarios, then the output from \code{survey_gadget} can be fed into
#' \code{strip_age_length_data} and subsamples of length and age data will be returned according to
#' the given proportions. The entire process can be replicated \emph{n} number of times using
#' \code{replicate_datasets}
#'
#'
#' @return \code{add_lengthgroup} returns a wide \code{data.frame} similar to that of
#' \code{stock_data}, but with values distributed across each length group which are represented
#' as each column. The output of \code{add_lengthgroup} is meant to go directly to
#' \code{survey_gadget} which returns a \code{data.frame} similar to \code{stock_data}, but
#' disaggregated by length. \code{strip_age_length_data} returns a named list of two
#' \code{data.frame}s. One for length data and one for age data. \code{replicate_datasets} returns
#' a names list of length 3, each element containing a \code{data.frame} with nrows the number of
#' replications times the nrow of \code{stock_data}. The first of these \code{data.frame}s is for
#' survey indices, the second for length data, and the third for age data.
#'
#' @export
#'
#' @name sample_gadget
#'
#' @examples
#' path <- system.file(gad_mod_dir, package = "gadgetSim")
#' cod_stock_std <- get_stock_std(main = "WGTS/main.final", params_file = "WGTS/params.final",
#'                                path = path, fit_dir = "WGTS")
#' lengrps <- seq(0.5, 50.5, by = 1)
#' suitability <- logistic_selectivity(lengrps, 0.15, 20, 0.1)
#' cod_lendist <- add_lengthgroups(cod_stock_std$cod0, lengrps)
#' cod_samples <- survey_gadget(cod_lendist, lengrps, suitability, 0.1)
#' cod_comp_data <- strip_age_length_data(cod_samples, length_prop = 0.2, age_prop = 0.2)
#'
#' # the above process can be automated n number of times
#' cod_comp_data <-
#'     replicate_datasets(cod_stock_std$cod0, lengrps, suitability,
#'                        survey_sigma = 0.1, length_prop = 0.2, age_prop = 0.2, n = 10)
add_lengthgroups <- function(stock_data, length_groups, keep_zero_counts = FALSE) {
    if (length(length_groups) < 2) {
        stop("Length group should have at least 2 members")
    }
    if (!keep_zero_counts) {
        stock_data <- stock_data[stock_data$number > 0, ]
    }
    lengrp_lower <- length_groups[-length(length_groups)]
    lengrp_upper <- length_groups[-1]
    len_dist <- function(len) {
        pnorm(rep(len, each = nrow(stock_data)), stock_data$length, stock_data$length.sd)
    }
    stock_number <- rep(stock_data$number, times = length(lengrp_upper))
    stock_len_numbers <- stock_number * (len_dist(lengrp_upper) - len_dist(lengrp_lower))
    lengrp_names <- list(c(), paste("length", lengrp_lower, lengrp_upper, sep = "_"))
    lengrp_matrix <-
        as.data.frame(matrix(stock_len_numbers,
                             dimnames = lengrp_names,
                             ncol = length(lengrp_lower)))
    return(cbind(stock_data, lengrp_matrix))
}


#' @rdname sample_gadget
#' @param survey_suitability Numeric vector the same length as \code{length_groups} representing the
#' selection probability for each length in \code{length_groups}
#' @param survey_sigma Numeric value of multiplicative error to place on samples
survey_gadget <- function(stock_data, length_groups, survey_suitability, survey_sigma) {
    lengrp_lower <- length_groups[-length(length_groups)]
    lengrp_upper <- length_groups[-1]
    base_names <- grep("^length|^weight|^number",
                       names(stock_data), value = TRUE, invert = TRUE)
    base_data <- stock_data[, base_names, drop = FALSE]
    do.call("rbind", lapply(seq_len(length(lengrp_lower)), function(i) {
        length_col <-
            paste("length", lengrp_lower[[i]], lengrp_upper[[i]], sep = "_")
        out <- base_data
        out$length <- mean(c(lengrp_upper[[i]], lengrp_lower[[i]]))
        out$weight <- stock_data$weight
        mult_error <- exp(rnorm(nrow(base_data), 0, survey_sigma) - survey_sigma/2)
        out$number <- round(stock_data[, length_col] * mult_error * survey_suitability[[i]])
        return(out)
    }))
}

#' @rdname sample_gadget
#' @param length_prop Numeric. The desired proportion of data to sample lengths on.
#' Must be between 0 and 1.
#' @param age_prop Numeric. The desired proporion of length samples to also sample ages.
#' Must be between 0 and 1. Note that \code{age_prop} only corresponds to the proportion of length
#' samples to also sample for age. Therefore the actual proportion of age samples relative to
#' sampled numbers is \code{length_prop * age_prop}
strip_age_length_data <- function(stock_data, length_prop = 1, age_prop = NULL) {
    if (length_prop > 1 | age_prop > 1) {
        stop("You cannot have a length_prop or age_prop > 1")
    } else if (length_prop == 1 | age_prop == 1) {
        warning("You left one or both of the sampling proportions at 1,
                 which will not actually strip any data")
    }
    length_data <- stock_data
    length_data$number <- round(length_data$number * length_prop)
    if (!is.null(age_prop)) {
        age_data <- length_data
        age_data$number <- round(age_data$number * age_prop)
    }
    if (requireNamespace("dplyr", quietly = TRUE)) {
        `%>%` <- magrittr::`%>%`
        length_data <-
            length_data %>%
            dplyr::group_by(year, step, area, length) %>%
            dplyr::summarize(number = sum(number)) %>%
            as.data.frame()
    } else {
        length_data <-
            aggregate(number ~ year + step + area + length,
                      data = length_data, FUN = sum)
    }
    age_data <- subset(age_data, select = c("year", "step", "area", "age", "length", "number"))
    return(list(length_data = length_data, age_data = age_data))
}

#' @rdname sample_gadget
#' @param n Integer. The number of times to replicate the sampling procedure
replicate_datasets <- function(stock_data, length_groups, survey_suitability,
                               survey_sigma, length_prop = 1, age_prop = NULL,
                               n = 10, keep_zero_counts = FALSE) {
    dat_list <-
        lapply(1:n, function(x) {
            ldist <- add_lengthgroups(stock_data,
                                       length_groups = length_groups,
                                       keep_zero_counts = keep_zero_counts)
            sample <- survey_gadget(ldist, length_groups = length_groups,
                                    survey_suitability = survey_suitability,
                                    survey_sigma = survey_sigma)
            sample$replicate <- x
            comp_data <- strip_age_length_data(sample,
                                               length_prop = length_prop,
                                               age_prop = age_prop)
            comp_data$length_data$replicate <- x
            comp_data$age_data$replicate <- x
            return(c(list(index = sample), comp_data))
        })
    indices <-
        do.call("rbind", lapply(1:n, function(x) {
            return(dat_list[[x]]$index)
        }))
    length_data <-
        do.call("rbind", lapply(1:n, function(x) {
            return(dat_list[[x]]$length_data)
        }))
    age_data <-
        do.call("rbind", lapply(1:n, function(x) {
            return(dat_list[[x]]$age_data)
        }))
    return(list(indices = indices, length_data = length_data, age_data = age_data))
}

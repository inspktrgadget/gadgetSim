# functions used in sampling output from gadget

#' Add length groups to Gadget StockStdPrinter output
#'
#' @param stock_data A \code{data.frame} of stock data retrieved via
#' \code{\link{get_stock_std}} or \code{\link{read_gadget_stock_std}}
#' @param length_groups Numeric vector of length groups to distribute by.
#' The difference in values probably should be the same as dl in Gadget model
#' @param keep_zero_counts Logical. Keep year/step/area/age combinations with
#' no individuals
#' @param variable Character vector of the variable on which to distribute
#' length groups. Default is "number". Can be "number.consumed" or
#' "biomass.consumed"
#'
#' Returns a wide \code{data.frame} similar to that of \code{stock_data}, but
#' with values distributed across each length group which are represented as
#' each column. The output of \code{add_lengthgroup} is meant to go directly to
#' \code{\link{survey_gadget}}
#'
#' @examples
#' cod_lendist <- add_lengthgroups(cod_stock_std$cod0, lengrps)
add_lengthgroups <- function(stock_data, length_groups, variable = "number",
                             keep_zero_counts = FALSE) {
    if (length(length_groups) < 2) {
        stop("Length group should have at least 2 members")
    }
    if (!keep_zero_counts) {
        stock_data <- stock_data[stock_data[,variable] > 0, ]
    }
    lengrp_lower <- length_groups[-length(length_groups)]
    lengrp_upper <- length_groups[-1]
    len_dist <- function(len) {
        pnorm(rep(len, each = nrow(stock_data)),
              stock_data$length,
              stock_data$length.sd)
    }
    stock_number <- rep(stock_data[,variable, drop = TRUE], 
                        times = length(lengrp_upper))
    stock_len_numbers <-
        stock_number * (len_dist(lengrp_upper) - len_dist(lengrp_lower))
    lengrp_names <-
        list(c(), paste("length", lengrp_lower, lengrp_upper, sep = "_"))
    lengrp_matrix <-
        as.data.frame(matrix(stock_len_numbers,
                             dimnames = lengrp_names,
                             ncol = length(lengrp_lower)))
    return(cbind(stock_data, lengrp_matrix))
}

#' Create length distributions and sample Gadget output
#'
#' These functions create length distributions from Gadget StockStdPrinter
#' output and sample that output to simulate surveys. Error can be added to
#' simulated surveys in \code{survey_gadget} and age and length subsamples can
#' be taken with \code{strip_age_length_data} to more closely mimic real world
#' surveys
#'
#'
#' @param stock_data A \code{data.frame} of stock data retrieved via
#' \code{\link{get_stock_std}} or \code{\link{read_gadget_stock_std}}
#' @param length_groups Numeric vector of length groups to distribute by.
#' The difference in values probably should be the same as dl in Gadget model
#' @param survey_suitability Numeric vector the same length as
#' \code{length_groups} representing the selection probability for each length
#' in \code{length_groups}
#' @param survey_sigma Numeric value of multiplicative error to place on samples
#' @param variable Character vector of the variable on which to distribute
#' length groups. Default is "number". Can be "number.consumed" or
#' "biomass.consumed"
#' @param keep_zero_counts Logical. Keep year/step/area/age combinations with
#' no individuals
#'
#' @details Length-structured population information from Gadget is output as
#' mean length and standard deviation for each year, step, area, and age
#' combination. \code{survey_gadget} takes output from
#' \code{\link{get_stock_std}} (or \code{\link{read_gadget_stock_std}}) and
#' distributes the number for each respective combination into numbers at each
#' length specified by \code{length_groups}, simulates
#' surveys given the selectivity provided in \code{survey_suitability} and
#' error given by \code{survey_sigma}. If it is desired to have a subsample of
#' length and age data to more closely mimic real world scenarios, then the
#' output from \code{survey_gadget} can be fed into \code{strip_age_length_data}
#' and subsamples of length and age data will be returned according to the given
#' proportions. The entire process can be replicated \emph{n} number of times
#' using \code{replicate_datasets}
#'
#'
#' @return \code{survey_gadget} returns a \code{data.frame} similar to
#' \code{stock_data}, but disaggregated by length based on the std. dev. found
#' in the StockStdPrinter.
#' \code{strip_age_length_data} returns a named list of two
#' \code{data.frame}s. One for length data and one for age data.
#' \code{replicate_datasets} returns a names list of length 3, each element
#' containing a \code{data.frame} with nrows the number of replications times
#' the nrow of \code{stock_data}. The first of these \code{data.frame}s is for
#' survey indices, the second for length data, and the third for age data.
#'
#' @export
#'
#' @name sample_gadget
#'
#' @examples
#' path <- system.file(gad_mod_dir, package = "gadgetSim")
#' cod_stock_std <- get_stock_std(main = "main",
#'                                path = path)
#' lengrps <- seq(0.5, 150.5, by = 1)
#' suitability <- logistic_selectivity(lengrps, 0.15, 20, 0.1)
#' cod_samples <- survey_gadget(cod_stock_std$cod, lengrps, suitability, 0.1)
#' cod_comp_data <-
#'   strip_age_length_data(cod_samples, length_samples = 0.2, age_samples = 0.2)
#'
#' # the above process can be automated n number of times
#' cod_comp_data <-
#'     replicate_datasets(cod_stock_std$cod, lengrps, suitability,
#'                        survey_sigma = 0.1, length_samples = 0.2,
#'                        age_samples = 0.2, n = 10)
survey_gadget <- function(stock_data, length_groups,
                          survey_suitability, survey_sigma,
                          variable = "number",
                          keep_zero_counts = FALSE) {
    stock_data <-
        add_lengthgroups(stock_data, length_groups = length_groups,
                         variable = variable,
                         keep_zero_counts = keep_zero_counts)
    lengrp_lower <- length_groups[-length(length_groups)]
    lengrp_upper <- length_groups[-1]
    base_names <- grep(paste0("^length|^weight$|", sprintf("^%s$", variable)),
                       names(stock_data), value = TRUE, invert = TRUE)
    var_names <- c("number", "number.consumed", "biomass.consumed")
    base_names <- base_names[base_names != var_names[variable != var_names]]
    base_data <- stock_data[, base_names, drop = FALSE]
    lengrp_data <-
        lapply(seq_len(length(lengrp_lower)), function(i) {
            length_col <-
                paste("length", lengrp_lower[[i]], lengrp_upper[[i]], sep = "_")
            out <- base_data
            out$length <- mean(c(lengrp_upper[[i]], lengrp_lower[[i]]))
            out$weight <- stock_data$weight
            out$number <-
                round(stock_data[, length_col] *
                          (exp(rnorm(nrow(base_data),0,survey_sigma) -
                                   survey_sigma/2)) * survey_suitability[[i]])
            return(out)
        })
    if (requireNamespace("dplyr", quietly = TRUE)) {
        out <- dplyr::bind_rows(lengrp_data)
    } else {
        out <- data.frame(do.call(rbind, lengrp_data))
    }
    return(out)
}


#' @rdname sample_gadget
#' @param length_samples Numeric. If \code{length_samples} <= 1, that proportion
#' of \code{stock_data} will be sampled for each year/step/area combination.
#' If \code{length_samples > 1}, then a number of length samples equal to the
#' integer value provided will be sampled for each year/step/area combination.
#' @param age_samples Numeric. If \code{age_samples} <= 1, that proportion
#' of \code{length_samples} will be sampled for each year/step/area combination.
#' If \code{age_samples > 1}, then a number of age samples equal to the
#' integer value provided will be sampled for each year/step/area combination
#' from the length data already sampled. Note that if \code{age_samples} >= 1
#' then this corresponds to the proportion of length samples to also sample for
#' age. Therefore the actual proportion of age samples relative to sampled
#' numbers is \code{length_samples * age_samples}. Sampling is similar for if
#' \code{length_samples} and \code{age_samples} > 1; however, if
#' \code{length_samples} is given as an integer value then \code{age_samples}
#' must be \code{<= length_samples}
#' @param quiet Logical. If either \code{length_samples} or \code{age_samples}
#' are equal to  1, then this will not strip any length or age data,
#' respectively, and will issue a warning stating so. \code{quiet = TRUE}
#' suppresses this warning.
#'
#' @export
strip_age_length_data <- function(stock_data,
                                  length_samples = 1, age_samples = 1,
                                  quiet = FALSE) {
    if (length_samples > 1) {
        if (age_samples <= 1) {
            age_samples <- length_samples * age_samples
        } else if (age_samples > length_samples) {
            stop("Age sample number cannot be greater than length ",
                 "sample number")
        }
        # obtain the number of length samples for each year/step/area
        if (requireNamespace("dplyr", quietly = TRUE)) {
            `%>%` <- magrittr::`%>%`
            length_data <-
                stock_data %>%
                group_by(year, step, area) %>%
                sample_n(size = length_samples,
                         weight = number,
                         replace = TRUE) %>%
                mutate(number = 1) %>%
                group_by(year, step, area, age, length) %>%
                summarize(number = sum(number))
            age_data <- length_data
            length_data <-
                length_data %>%
                ungroup() %>%
                mutate(age = "all") %>%
                group_by(year, step, area, age, length) %>%
                summarize(number = sum(number)) %>%
                filter(number > 0)
            age_data <-
                age_data %>%
                group_by(year, step, area) %>%
                sample_n(size = age_samples,
                         replace = TRUE,
                         weight = number) %>%
                mutate(number = 1) %>%
                group_by(year, step, area, age, length) %>%
                summarize(number = sum(number)) %>%
                filter(number > 0)
        } else {
            warning("Sampling by sample number can be somewhat time-intensive ",
                    "consider installing the tidyverse package to speed things",
                    " up a bit")
            years <- unique(stock_data$year)
            steps <- unique(stock_data$step)
            areas <- unique(stock_data$area)
            length_data <-
                do.call("rbind", lapply(years, function(x) {
                    do.call("rbind", lapply(steps, function(y) {
                        do.call("rbind", lapply(areas, function(z) {
                            dat <- subset(stock_data,
                                          year == x & step == y & area == z)
                            dat <- dat[sample(rownames(dat),
                                              size = length_samples,
                                              replace = TRUE,
                                              prob = dat$number), ]
                            return(as.matrix(dat))
                        }))
                    }))
                }))
            length_data <- as.data.frame(length_data)
            length_data$number <- 1
            length_data <-
                aggregate(number ~ year + step + area + age + length,
                          data = length_data, FUN = sum)
            age_data <- length_data
            length_data$age <- "all"
            length_data <-
                do.call("rbind", lapply(years, function(x) {
                    do.call("rbind", lapply(steps, function(y) {
                        do.call("rbind", lapply(areas, function(z) {
                            dat <- subset(length_data,
                                          year == x & step == y & area == z)
                            dat <- dat[sample(rownames(dat),
                                              size = length_samples,
                                              replace = TRUE,
                                              prob = dat$number), ]
                            return(as.matrix(dat))
                        }))
                    }))
                }))
            age_data <- as.data.frame(age_data)
            age_data$number <- 1
            age_data <-
                aggregate(number ~ year + step + area + age + length,
                          data = age_data, FUN = sum)
            length_data <- subset(length_data, number > 0)
            rownames(length_data) <- 1:nrow(length_data)
            age_data <- subset(age_data, number > 0)
            rownames(age_data) <- 1:nrow(age_data)
        }
    } else if (length_samples <= 1) {
        if (length_samples == 1 | age_samples == 1) {
            if (!quiet) {
                warning("You left one or both of the sampling proportions ",
                        "at 1, which will not actually strip any data")
            }
        }
        length_data <- stock_data
        length_data$number <- round(length_data$number * length_samples)
        age_data <- length_data
        if (age_samples < 1) {
            age_data$number <- round(age_data$number * age_samples)
        } else if (age_samples > 1) {
            if (requireNamespace("dplyr", quietly = TRUE)) {
                `%>%` <- magrittr::`%>%`
                length_data <- filter(length_data, number > 0)
                age_data <-
                    age_data %>%
                    group_by(year, step, area) %>%
                    sample_n(size = age_samples,
                             replace = TRUE,
                             weight = number) %>%
                    mutate(number = 1) %>%
                    group_by(year, step, area, age, length) %>%
                    summarize(number = sum(number)) %>%
                    filter(number > 0)
            } else {
                warning("Sampling by sample number can be somewhat ",
                        "time-intensive consider installing the tidyverse ",
                        "package to speed things up a bit")
                years <- unique(stock_data$year)
                steps <- unique(stock_data$step)
                areas <- unique(stock_data$area)
                length_data <-
                    do.call("rbind", lapply(years, function(x) {
                        do.call("rbind", lapply(steps, function(y) {
                            do.call("rbind", lapply(areas, function(z) {
                                dat <- subset(stock_data,
                                              year == x & step == y & area == z)
                                dat <- dat[sample(rownames(dat),
                                                  size = length_samples,
                                                  replace = TRUE,
                                                  prob = dat$number), ]
                                return(as.matrix(dat))
                            }))
                        }))
                    }))
                length_data <- as.data.frame(length_data)
                length_data$number <- 1
                length_data <-
                    aggregate(number ~ year + step + area + age + length,
                              data = length_data, FUN = sum)
                age_data <- length_data
                length_data$age <- "all"
                length_data <-
                    do.call("rbind", lapply(years, function(x) {
                        do.call("rbind", lapply(steps, function(y) {
                            do.call("rbind", lapply(areas, function(z) {
                                dat <- subset(length_data,
                                              year == x & step == y & area == z)
                                dat <- dat[sample(rownames(dat),
                                                  size = length_samples,
                                                  replace = TRUE,
                                                  prob = dat$number), ]
                                return(as.matrix(dat))
                            }))
                        }))
                    }))
                age_data <- as.data.frame(age_data)
                age_data$number <- 1
                age_data <-
                    aggregate(number ~ year + step + area + age + length,
                              data = age_data, FUN = sum)
                length_data <- subset(length_data, number > 0)
                rownames(length_data) <- 1:nrow(length_data)
                age_data <- subset(age_data, number > 0)
                rownames(age_data) <- 1:nrow(age_data)
            }
        }
        if (requireNamespace("dplyr", quietly = TRUE)) {
            `%>%` <- magrittr::`%>%`
            length_data <-
                length_data %>%
                dplyr::group_by(year, step, area, length) %>%
                dplyr::summarize(number = sum(number)) %>%
                dplyr::mutate(age = "all") %>%
                dplyr::select(year, step, area, age, length, number) %>%
                filter(number > 0) %>%
                as.data.frame()
            age_data <-
                age_data %>%
                select(year, step, area, age, length, number) %>%
                filter(number > 0)
        } else {
            length_data <-
                aggregate(number ~ year + step + area + length,
                          data = length_data, FUN = sum)
            length_data$age <- "all"
            length_data <-
                subset(length_data,
                       select = c("year", "step", "area",
                                  "age", "length", "number"))
            length_data <- subset(length_data, number > 0)
            rownames(length_data) <- 1:nrow(length_data)
            age_data <- subset(age_data, select = c("year", "step", "area",
                                                    "age", "length", "number"))
            age_data <- subset(age_data, number > 0)
            rownames(age_data) <- 1:nrow(age_data)
        }
    }
    return(list(length_data = length_data, age_data = age_data))
}


#' @rdname sample_gadget
#' @param n Integer. The number of times to replicate the sampling procedure
#' @export
replicate_datasets <- function(stock_data, length_groups, survey_suitability,
                               survey_sigma, length_samples = 1, age_samples = NULL,
                               quiet = FALSE,
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
                                               length_samples = length_samples,
                                               age_samples = age_samples,
                                               quiet = quiet)
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
    return(list(indices = indices,
                length_data = length_data,
                age_data = age_data))
}

#' Strip survey samples for the desired length or age groups to use for
#' surveyindices data
#'
#' Easily pick out the data that you want to use for surveyindices
#'
#' @param data_samples Data.frame of sampled model output as returned by
#' \code{\link{survey_gadget}}, for example
#' @param by Character. Currently either only "lengths" or "ages"
#' @param groups Numeric. The values by which to subset \code{by}
#'
#' @return A data.frame of filtered or subsetted data
#' @export
#'
#' @examples
#' dat <- expand.grid(year = 1:10, step = 1, area = 1,
#'                    ages = 1:10, lengths = 1:100)
#' dat$number <- sample(1:1000, nrow(dat), replace = TRUE)
#' si_dat <- get_si_data(dat, by = "lengths", groups = 1:20)
get_si_data <- function(data_samples, by = "lengths", groups) {
    if (by == "lengths") {
        if (requireNamespace("dplyr", quietly = TRUE)) {
            length_data <-
                data_samples %>%
                dplyr::filter(length %in% groups) %>%
                dplyr::group_by(year, step, area, length) %>%
                dplyr::summarize(number = sum(number)) %>%
                dplyr::filter(number > 0) %>%
                dplyr::arrange(year, step, area, length) %>%
                as.data.frame()
        } else {
            length_data <-
                subset(subset(data_samples, length %in% groups),
                       select = c("year", "step", "area", "length", "number"))
            length_data <-
                aggregate(number ~ year + step + area + length,
                          data = length_data, FUN = sum)
            length_data <-
                length_data[order(length_data$year, length_data$step,
                                  length_data$area, length_data$length),]
            length_data <- subset(length_data, number > 0)
            rownames(length_data) <- 1:nrow(length_data)
        }
        return(length_data)
    } else if (by == "ages") {
        if (requireNamespace("dplyr", quietly = TRUE)) {
            age_data <-
                data_samples %>%
                dplyr::filter(age %in% groups) %>%
                dplyr::group_by(year, step, area, age) %>%
                dplyr::summarize(number = sum(number)) %>%
                dplyr::filter(number > 0) %>%
                dplyr::arrange(year, step, area, age) %>%
                as.data.frame()
        } else {
            age_data <-
                subset(subset(data_samples, age %in% groups),
                       select = c("year", "step", "area", "age", "number"))
            age_data <-
                aggregate(number ~ year + step + area + age,
                          data = age_data, FUN = sum)
            age_data <- subset(age_data, number > 0)
            age_data <-
                age_data[order(age_data$year, age_data$step,
                                  age_data$area, age_data$age),]
            rownames(age_data) <- 1:nrow(age_data)
        }
        return(age_data)
    } else {
        stop("\n", "by must be lengths or ages")
    }
}

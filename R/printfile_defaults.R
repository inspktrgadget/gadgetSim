# defaults for printfiles

#' Gadget print file defaults
#'
#' Pre-baked defaults of printfile types for use in functions
#'
#' @name printfileDefaults
NULL

printfile_years <- "all"
printfile_steps <- "all"
printfile_ys <- c(printfile_years, printfile_steps)

rm_optional_args <- function(printfile, optional_args) {
    return(names(printfile)[-grep(optional_args, names(printfile))])
}

#' @rdname printfileDefaults
stock_std <-
    structure(list(
        type = "stockstdprinter",
        stockname = NULL,
        scale = 1,
        printfile = NULL,
        precision = NULL,
        printatstart = 0,
        yearsandsteps = printfile_ys
    ), class = c("stock_std", "list"))

#' @rdname printfileDefaults
stock_full <-
    structure(list(
        type = "stockfullprinter",
        stocknames = NULL,
        printfile = NULL,
        precision = NULL,
        printatstart = 0,
        yearsandsteps = printfile_ys
    ), class = c("stock_full", "list"))

#' @rdname printfileDefaults
stock <-
    structure(list(
        type = "stockprinter",
        stocknames = NULL,
        areaaggfile = NULL,
        ageaggfile = NULL,
        lenaggfile = NULL,
        printfile = NULL,
        precision = NULL,
        printatstart = 0,
        yearsandsteps = printfile_ys
    ), class = c("stock", "list"))


#' @rdname printfileDefaults
predator <-
    structure(list(
        type = "predatorprinter",
        predatornames = NULL,
        preynames = NULL,
        areaaggfile = NULL,
        predlenaggfile = NULL,
        preylenaggfile = NULL,
        biomass = NULL,
        printfile = NULL,
        precision = NULL,
        yearsandsteps = printfile_ys
    ), class = c("predator", "list"))

#' @rdname printfileDefaults
predator_over <-
    structure(list(
        type = "predatoroverprinter",
        predatornames = NULL,
        areaaggfile = NULL,
        lenaggfile = NULL,
        printfile = NULL,
        precision = NULL,
        yearsandsteps = printfile_ys
    ), class = c("predator_over", "list"))

#' @rdname printfileDefaults
prey_over <-
    structure(list(
        type = "preyoverprinter",
        preynames = NULL,
        areaaggfile = NULL,
        lenaggfile = NULL,
        printfile = NULL,
        precision = NULL,
        yearsandsteps = printfile_ys
    ), class = c("prey_over", "list"))

#' @rdname printfileDefaults
stock_prey_full <-
    structure(list(
        type = "stockpreyfullprinter",
        preyname = NULL,
        printfile = NULL,
        precision = NULL,
        yearsandsteps = NULL
    ), class = c("stock_prey_full", "list"))

#' @rdname printfileDefaults
stock_prey <-
    structure(list(
        type = "stockpreyprinter",
        preynames = NULL,
        printfile = NULL,
        areaaggfile = NULL,
        ageaggfile = NULL,
        lenaggfile = NULL,
        precision = NULL,
        yearsandsteps = printfile_ys
    ), class = c("stock_prey", "list"))

#' @rdname printfileDefaults
predator_prey <-
    structure(list(
        type = "predatorpreyprinter",
        predatornames = NULL,
        preynames = NULL,
        areaaggfile = NULL,
        ageaggfile = NULL,
        lenaggfile = NULL,
        printfile = NULL,
        precision = NULL,
        yearsandsteps = printfile_ys
    ), class = c("predator_prey", "list"))

#' @rdname printfileDefaults
likelihood <-
    structure(list(
        type = "likelihoodprinter",
        likelihood = NULL,
        printfile = NULL
    ), class = c("likelihood", "list"))

#' @rdname printfileDefaults
likelihood_summary <-
    structure(list(
        type = "likelihoodsummaryprinter",
        printfile = NULL
    ), class = c("likelihoodsummary", "list"))


#' Non-optional arguments to Gadget printfile components
#'
#' Pre-baked defaults of necessary printfile arguments for use in various functions
#'
#' @name printfileArgs
NULL

#' @rdname printfileArgs
stock_std_args <- rm_optional_args(stock_std, "precision|printatstart")

#' @rdname printfileArgs
stock_full_args <- rm_optional_args(stock_full, "precision|printatstart")

#' @rdname printfileArgs
stock_args <- rm_optional_args(stock, "precision|printatstart")

#' @rdname printfileArgs
predator_args <- rm_optional_args(predator, "precision|biomass")

#' @rdname printfileArgs
predator_over_args <- rm_optional_args(predator_over, "precision")

#' @rdname printfileArgs
prey_over_args <- rm_optional_args(prey_over, "precision")

#' @rdname printfileArgs
stock_prey_full_args <- rm_optional_args(stock_prey_full, "precision")

#' @rdname printfileArgs
stock_prey_args <- rm_optional_args(stock_prey, "precision")

#' @rdname printfileArgs
predator_prey_args <- rm_optional_args(predator_prey, "precision")

#' @rdname printfileArgs
likelihood_args <- names(likelihood)

#' @rdname printfileArgs
likelihood_summary_args <- names(likelihood_summary)

# functions to make R versions of various gadget files
# to be used when exporting R objects to gadget-readable files

#' Values for  may only be one of the following: \code{stock_std, stock_full,
#' stock, predator, predator_over, prey_over, stock_prey_full, stock_prey, predator_prey,
#' likelihood, likelihood_summary}.

make_gadget_printfile <- function(..., main = "main", file, path = NULL,
                                  output = "out", aggfiles = "print.aggfiles",
                                  printatstart = 1, steps = "all") {
    if (!is.null(path)) {
        main <- read_gadget_main(check_path(main))
        dir_name <- basename(path)
    } else {
        main <- read_gadget_main(main)
        dir_name <- basename(getwd())
    }
    stockfiles <- main$stockfiles
    fleetfiles <- main$fleetfiles
    lik_files <- main$likelihood
    header <- sprintf("; printfile for %s - created by gadgetSim %s on %s",
                      dir_name, packageVersion("gadgetSim"), date())
    # plug the information given in ... into the appropriate printfile component template
    updated_printfiles <-
        lapply(seq_along(list(...)), function(x) {
                pf_template <- getFromNamespace(names(list(...))[x], ns = "gadgetSim")
                new_pf <- update_printfile(pf_template, list(...)[[x]])
                if (is.null(new_pf$printfile)) {
                    type_label <- gsub("_", "\\.", class(new_pf)[1])
                    get_stocks <- unlist(new_pf[grep("name", names(new_pf))])
                    if (!is.null(output)) {
                        new_pf$printfile <-
                            sprintf("%s/%s.%s",
                                    output,
                                    paste(get_stocks, collapse = "."),
                                    type_label)
                    } else {
                        new_pf$printfile <-
                            sprintf("%s.%s", paste(get_stocks, collapse = "."),
                                    type_label)
                    }
                } else if (!is.null(output)) {
                    new_pf$printfile <- sprintf("%s/%s", output, new_pf$printfile)
                }
                return(new_pf)
            })
    # figure out which printfile components need aggregation files and what types
    stocks2agg <-
        lapply(updated_printfiles, function(x) {
            x_nms <- names(x)
            x_stocknames <- grep("names", x_nms)
            x_agg_ind <- grep("aggfile", x_nms)
            null_agg_ind <-
                vapply(seq_along(x), function(y) {
                    if (is.null(x[[y]]) & (y %in% x_agg_ind)) {
                        return(y)
                    } else {return(0)}
                }, numeric(1))
            return(x[c(x_stocknames, null_agg_ind)])
        })
    # make the aggregate files
    agg_info <-
        lapply(seq_along(updated_printfiles), function(x) {
            if (length(stocks2agg[[x]]) == 0) {
                return(NULL)
            }
        })
    updated_printfiles <-
        lapply(updated_printfiles, update_printfile_dirs,
               print_dir = output, aggfile = aggfiles)
    formatted_printfiles <-
        lapply(updated_printfiles, format_printfile)
    likfile2print <- paste(c(header, formatted_printfiles), collapse = "\n")
    # create directories for file writing
    if (!dir.exists(check_path(aggfiles))) {
        dir.create(check_path(aggfiles))
    }
    if (!dir.exists(check_path(output))) {
        dir.create(check_path(output))
    }
    write(likfile2print, file = check_path(file))
    # first get and format the likelihood components
    likelihood <- read_gadget_likelihood(lik_files, path = path)
    lik_comps <- likelihood[-grep("penalty|understocking", names(likelihood))]
    lik_comps <-
        as.vector(unlist(lapply(lik_comps, function(x) {
            return(unique(x$name))
        })))
    lik_printers <- paste(comp_lab,
                          sprintf("type\t%s", lik_comps),
                          sprintf("printfile\t%s/%s", output, lik_comps),
                          sep = "\n", collapse = "\n;\n")
    # next get and format the stocks
    stocks <- read_gadget_stockfiles(stockfiles, path = path)
    stocks2print <- as.vector(unlist(lapply(stocks, function(x) return(x$stockname))))
    stock_std <-
        paste(comp_lab,
              "type\tstockstdprinter",
              sprintf("stockname\t%s", stocks2print),
              sprintf("printfile\t%s/%s.std", output, stocks2print),
              sprintf("printatstart\t%s", printatstart),
              sprintf("yearsandsteps\t%s\t%s", "all", steps),
              sep = "\n", collapse = "\n;\n")
    stock_full_aggfiles <-
        lapply(stocks, function(x) {
            areas <- sprintf("area%1$s\t%1$s", x$livesonareas)
            age <- paste("allages",
                         paste(as.numeric(x$minage):as.numeric(x$maxage), collapse = "\t"),
                         sep = "\t")
            stock_len <- as.numeric(x$minlength):as.numeric(x$maxlength)
            len <-
                paste(paste0("len", stock_len[-1]),
                      stock_len[-length(stock_len)], stock_len[-1],
                      sep = "\t", collapse = "\n")
            null_list <-
                lapply(c("areas", "age", "len"), function(y) {
                    tmp <- get(y, envir = parent.frame(n=2))
                    header <-
                        sprintf("; %s aggregation file for %s created with gadgetSim %s at %s",
                                y, x$stockname, packageVersion("gadgetSim"), date())
                    out <- paste(header, tmp, sep = "\n")
                    write(out,
                          file = paste(check_path(aggfiles, env = parent.frame(n=4)),
                                       sprintf("%s.%s.agg", x$stockname, y),
                                       sep = "/"))
                })
        })
    stock_full <-
        paste(comp_lab,
              "type\tstockprinter",
              sprintf("stocknames\t%s", stocks2print),
              sprintf("areaaggfile\t%s/%s.area.agg", aggfiles, stocks2print),
              sprintf("ageaggfiles\t%s/%s.age.agg", aggfiles, stocks2print),
              sprintf("lenaggfile\t%s/%s.len.agg", aggfiles, stocks2print),
              sprintf("printfile\t%s/%s.full", output, stocks2print),
              sprintf("printatstart\t%s", printatstart),
              sprintf("yearsandsteps\tall\t%s", steps),
              sep = "\n", collapse = "\n;\n")
    # lastly get and format fleets


}

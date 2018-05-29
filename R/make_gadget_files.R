# functions to make R versions of various gadget files
# to be used when exporting R objects to gadget-readable files


#' Create a list of class gadget.main
#'
#' @param ... Either a list with named objects pertaining to the main file for
#' Gadget or any number of objects named similarly
#'
#' @return A list of class \code{gadget.main}
#' @export
#'
#' @examples
#' main <- list(timefile = "time", areafile = "area", stockfiles = "cod")
#' make_gadget_mainfile(main)
make_gadget_mainfile <- function(...) {
    dots <- dots2list(...)
    out <- structure(modifyList(gadget_main_default, dots),
                     class = c("gadget.main", "list"))
    if (!isTRUE(all.equal(names(out), names(gadget_main_default)))) {
        stop("You have either removed an argument from mainfile or added one not recognized")
    } else if (is.null(out$stockfiles) | out$stockfiles == "") {
        warning("You have no stock in this mainfile")
    }
    return(out)
}

#' Make Gadget timefile
#'
#' @param st_year Integer. The starting year of the Gadget model
#' @param end_year Integer. The final year of the Gadget model
#' @param timesteps Either a numeric vector giving the number of timesteps and the breakdown of
#' months, or, more commonly and conventiently one of "annually", "biannually", "quarterly",
#' "monthly'
#'
#' @return A list of class \code{gadget.time}
#' @export
#'
#' @examples
#' make_gadget_timefile(1985, 2015, "quarterly")
#' make_gadget_timefile(1985, 2015, c(6, 2, 1, 3, 3, 1, 2))
make_gadget_timefile <- function(st_year, end_year, timesteps) {
    if (is.character(timesteps)) {
        timesteps <-
            switch(timesteps,
                   annually = c(1,12),
                   biannual = c(2, 6, 6),
                   quarterly = c(4, rep(3, 4)),
                   monthly = c(12, rep(1, 12)))
    }
    tf <- list(firstyear = st_year,
               firststep = 1,
               lastyear = end_year,
               laststep = timesteps[1],
               notimesteps = timesteps)
    out <- modifyList(gadget_timefile_default, tf)
    return(out)
}

#' Make Gadget areafile
#'
#' Creates a list of class \code{gadget.area} that can be used to write out a Gadget areafile
#'
#' @param areas Numeric vector describing the different areas
#' @param size Size of each area
#' @param temp_data Data.frame with 4 columns containing year, step, area, and mean temperature
#'
#' @return A list of class \code{gadget.area}
#' @export
#'
#' @examples
#' make_gadget_areafile(areas = 1, size = 1e6,
#'                      temp_data = expand.grid(year = 1:5, step = 1:4, area = 1, temp = 3))
#' make_gadget_areafile(areas = 1:2, size = c(1e6, 1.5e6),
#'                      temp_data = expand.grid(year = 1:2, step = 1:4, area = 1:2, temp = 3))
make_gadget_areafile <- function(areas, size, temp_data) {
    out <- modifyList(gadget_areafile_default,
                      list(areas = areas,
                           size = size,
                           temperature = temp_data))
    return(out)
}

#' Make Gadget stockfile
#'
#' Make a list of class \code{gadget.stock} that can be used to write out a Gadget stockfile
#'
#' @param ... A list of named elements. The names correspond to arguments found in Gadget stockfiles
#'
#' @return A list of class \code{gadget.stock} containing information about that stock to be used
#' by Gadget
#' @export
#'
#' @examples
#' # setup basic stock information
#' minage <- 1
#' maxage <- 10
#' minlength <- 1
#' maxlength <- 100
#' dl <- 1
#' alpha <- 1e-04
#' beta <- 3
#' reflength <- seq(minlength, maxlength, dl)
#' stock_info <-
#'     list(stockname = "cod", livesonareas = 1, minage = 1, maxage = 10,
#'          minlength = minlength, maxlength = maxlength, dl = dl)
#'
#' # setup refweightfile
#' stock_refwgt <-
#'     data.frame(len = reflength,
#'                weight = alpha * reflength ^ beta)
#'
#' # setup growth
#' stock_growth <-
#'     list(growthfunction = "lengthvbsimple",
#'          growthparameters =
#'              c(to_gadget_formula(quote(cod.linf)), to_gadget_formula(quote(cod.k)),
#'                0.001, 3))
#' # setup naturalmortality
#' stock_m <- list(naturalmortality = rep(0.2, 10))
#'
#' # setup initial conditions
#' init_data <-
#'     normalparamfile(age = seq(minage, maxage, 1),
#'                     area = 1,
#'                     age.factor = "#cod.init.age",
#'                     area.factor = "#cod.init.area",
#'                     mean = vb_formula("cod", minage:maxage),
#'                     sd = 1:10,
#'                     alpha = alpha,
#'                     beta = beta)
#' stock_initcond <- list(normalparamfile = init_data)
#'
#' # setup spawning
#' spawnfile <-
#'     make_gadget_spawnfile(
#'       stockname = "cod",
#'       start_year = 1,
#'       end_year = 10,
#'       recruitment = ricker_formula("cod")
#'     )
#' stock_spawnfile <-
#'     list(spawnfile = spawnfile)
#'
#' # create gadget stockfile
#' cod <-
#'    make_gadget_stockfile(stock = stock_info,
#'                          refweightfile = stock_refwgt,
#'                          growth = stock_growth,
#'                          naturalmortality = stock_m,
#'                          iseaten = 1,
#'                          initialconditions = stock_initcond,
#'                          doesspawn = stock_spawnfile)
make_gadget_stockfile <- function(...) {
    dots <- dots2list(...)
    if (!check_names("^stock", dots)) {
        stop("Named argument 'stock' must be provided with basic information about stock")
    }
    # checking and plugging in various components
    stock <- check_stock_info(dots)
    lenaggfile <- make_lenaggfile(dots)
    refweightfile <- check_stock_refweightfile(dots)
    growthandeatlengths <- check_stock_grw_eat_len(dots)
    growth <- check_stock_growth(dots)
    naturalmortality <- check_stock_m(dots)
    iseaten <- check_stock_iseaten(dots)
    predator <- check_stock_predator(dots)
    initialconditions <- check_stock_initcond(dots)
    migration <- check_stock_migration(dots)
    maturation <- check_stock_maturity(dots)
    movement <- check_stock_movement(dots)
    renewal <- check_stock_renewal(dots)
    spawning <- check_stock_spawning(dots)
    straying <- check_stock_straying(dots)
    stock_items <- c("stock", "refweightfile", "growthandeatlengths", "growth",
                     "naturalmortality", "iseaten", "predator", "initialconditions",
                     "migration", "maturation", "movement", "renewal", "spawning", "straying")
    stock_attr <-
        lapply(stock_items, function(x) {
            dat <- get(x)
            if (length(attributes(dat)) > 1) {
                return(attributes(dat)[-1][[1]])
            } else {
                return(NULL)
            }
        })
    stock_attr <- setNames(stock_attr, stock_items)
    stock_attr <- Filter(Negate(is.null), stock_attr)
    stockfile_names <- unlist(lapply(stock_items, function(x) names(get(x))))
    out <- c(stock, refweightfile, growthandeatlengths, growth, naturalmortality,
             iseaten, predator, initialconditions, migration, maturation, movement,
             renewal, spawning, straying)
    attributes(out) <- c(names = list(stockfile_names), stock_attr)
    class(out) <- c("gadget.stock", "list")
    return(out)
}

#' Create a list of class "gadget_spawnfile" that can be used to write to file
#'
#' This function creates a list of class "gadget_spawnfile". This list can then be used to
#' write a spawnfile for a stock to be used by Gadget
#'
#' @param stockname Character. The stockname desired (used to define switches)
#' @param start_year Numeric. The first year spawning will take place
#' @param end_year Numeric. The last year spawning will take place
#' @param ... Other vectors to be added. Should be named elements corresponding to those needed
#' for a Gadget spawnfile, see Gadget User Guide.
#'
#' @return A list of class "gadget_spawnfile"
#' @export
#'
#' @examples
#' make_gadget_spawnfile("cod", 1, 10)
make_gadget_spawnfile <- function(stockname, start_year, end_year, ...) {
    spawn_params <- list(
        spawnsteps = 1,
        spawnareas = 1,
        firstspawnyear = start_year,
        lastspawnyear = end_year,
        spawnstocksandratios = paste(stockname, 1, sep = "\t"),
        proportionfunction = paste("constant", 1, sep = "\t"),
        mortalityfunction = paste("constant", 0, sep = "\t"),
        weightlossfunction = paste("constant", 0, sep = "\t"),
        recruitment = bev_holt_formula(stockname),
        stockparameters = paste(sprintf("#%s.recl", stockname),
                                sprintf("#%s.rec.sd", stockname),
                                paste0(c("0.0001", "3"),
                                       collapse = "\t"),
                                sep = "\t")
    )
    override_defaults <- dots2list(...)
    spawn_params <- modifyList(spawn_params, override_defaults)
    class(spawn_params) <- c("gadget_spawnfile", "list")
    return(spawn_params)
}

make_gadget_fleet <- function(...) {

}

#' Make Gadget printfile and write out to file
#'
#' @param ... Any number of lists containing details of Gadget printfile components. Arguments to
#' \code{...} must be the same name as one of the default printfile lists,
#' see \code{\link{printfileDefaults}}
#'
#' @param main List of class \code{gadget.main} or character vector of the main printfile to be read
#' @param printfile Character. A path to where the printfile will be written
#' @param path Optional. Path to Gadget model
#' @param output_dir Character. Path to the directory where printfiles will be written
#' @param aggfile_dir Character. Path to the directory where aggfiles will be written
#' @param printatstart A numeric value of 0 or 1. 0 indicates printing at end of timesteps.
#' 1 indicates printing at the start
#' @param steps The steps in which to print the various printfile components.
#' Can be numeric or \code{"all"}
#'
#' @return NULL. Writes the printfiles provided in \code{...} to the directory
#' provided by \code{output_dir}
#' @export
#'
#' @examples
#' make_gadget_printfile(stock_std = list(stockname = "cod"), printfile = "printfile", path = gad_mod_dir)
make_gadget_printfile <- function(..., main = "main", printfile, path = NULL, fit_dir = NULL,
                                  output_dir = "out", aggfile_dir = "print.aggfiles",
                                  printatstart = 1, steps = "all") {
    if (!is.null(path)) {
        if ("gadget.main" %in% class(main)) {
            main <- main
        } else {
            main <- read_gadget_main(file = main, path = path)
        }
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
    # check to see if dots is given as a list or a number of named objects
    dots <- list(...)
    dot_len <- length(dots)
    if (dot_len < 1) {
        stop("You must provide a named object in ...")
    } else if (dot_len == 1) {
        if (!is.null(names(dots))) {
            dots <- dots
        } else {
            dots <- as.list(...)
            if (!any(names(dots) %in% pf_types)) {
                stop(cat("... must have names",
                         paste0("* ", pf_types),
                         "or be a list of objects with names as such",
                         sep = "\n"))
            }
        }
    } else {
        dots <- dots
    }
    # plug the information given in ... into the appropriate printfile component template
    updated_printfiles <-
        lapply(seq_along(dots), function(x) {
                pf_template <- getFromNamespace(names(dots)[x], ns = "gadgetSim")
                new_pf <- update_printfile(pf_template, dots[[x]])
                if (is.null(new_pf$printfile)) {
                    type_label <- gsub("_", "\\.", class(new_pf)[1])
                    stocks2get <- unlist(new_pf[grep("name", names(new_pf))])
                    if (!is.null(output_dir)) {
                        new_pf$printfile <-
                            sprintf("%s/%s.%s",
                                    output_dir,
                                    paste(stocks2get, collapse = "."),
                                    type_label)
                    } else {
                        new_pf$printfile <-
                            sprintf("%s.%s", paste(stocks2get, collapse = "."),
                                    type_label)
                    }
                } else if (!is.null(output_dir)) {
                    new_pf$printfile <- sprintf("%s/%s", output_dir, new_pf$printfile)
                }
                return(new_pf)
            })
    # check directory structure of fit_dir
    if (!is.null(fit_dir)) {
        printfile <- paste(c(fit_dir, printfile), collapse = "/")
        output_dir <- paste(c(fit_dir, output_dir), collapse = "/")
        aggfile_dir <- paste(c(fit_dir, aggfile_dir), collapse = "/")
    }
    # make and add aggregate files for those components that need them
    added_aggfiles <- lapply(updated_printfiles, make_aggfiles,
                             aggfile_dir = aggfile_dir, path = path)
    # check check for aggfiles, create if needed, and write out aggfiles
    null_list <- lapply(added_aggfiles, write_aggfiles,
                        aggfile_dir = aggfile_dir, path = path)
    formatted_printfiles <- lapply(added_aggfiles, format_printfile)
    likfile2print <- paste(c(header, formatted_printfiles), collapse = "\n;\n")
    # create directories for file writing
    if (!dir.exists(check_path(output_dir))) {
        dir.create(check_path(output_dir))
    }
    write(likfile2print, file = check_path(printfile))
}



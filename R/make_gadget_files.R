# functions to make R versions of various gadget files
# to be used when exporting R objects to gadget-readable files


#' Create a list of class "gadget_main"
#'
#' @param ... Either a list with named objects pertaining to the main file for
#' Gadget or any number of objects named similarly
#'
#' @return A list of class \code{gadget_main}
#' @export
#'
#' @examples
#' main <- list(timefile = "time", areafile = "area", stockfiles = "cod")
#' make_gadget_mainfile(main)
make_gadget_mainfile <- function(...) {
    dots <- dots2list(...)
    out <- structure(modifyList(gadget_main_default, dots))
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
#' @return A list of class \code{gadget_time}
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
#' Creates a list of class \code{gadget_area} that can be used to write out a Gadget areafile
#'
#' @param areas Numeric vector describing the different areas
#' @param size Size of each area
#' @param temp_data Data.frame with 4 columns containing year, step, area, and mean temperature
#'
#' @return A list of class \code{gadget_area}
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
#' Make a list of class \code{gadget_stock} that can be used to write out a Gadget stockfile
#'
#' @param ... A list of named elements. The names of these elements must correspond to arguments
#' found in Gadget stockfiles (i.e. for growth, use growth = list(), for spawning, use
#' spawning = list(), etc), see Gadget User Guide. Chapter 4. Stock Files.
#'
#' @return A list of class \code{gadget_stock} containing information about that stock to be used
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
#'     list(stockname = "cod", livesonareas = 1, minage = minage, maxage = maxage,
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
#' stock_m <- rep(0.2, 10)
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
#' stock_spawnfile <-
#'     make_gadget_spawnfile(
#'       stockname = "cod",
#'       start_year = 1,
#'       end_year = 10,
#'       recruitment = ricker_formula("cod")
#'     )
#'
#' # create gadget stockfile
#' cod <-
#'    make_gadget_stockfile(stock = stock_info,
#'                          refweightfile = stock_refwgt,
#'                          growth = stock_growth,
#'                          naturalmortality = stock_m,
#'                          iseaten = 1,
#'                          initialconditions = stock_initcond,
#'                          spawning = stock_spawnfile)
make_gadget_stockfile <- function(...) {
    dots <- dots2list(...)
    if (!check_names("^stock", dots)) {
        stop("Named argument 'stock' must be provided with basic information about stock")
    }
    # checking and plugging in various components
    stock <- check_stock_info(dots)
    lenaggfile <- make_stock_lenaggfile(dots)
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
    class(out) <- c("gadget_stock", "list")
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
    dots <- dots2list(...)
    dot_args <-
        lapply(dots, function(x) {
            tmp <- lapply(x, function(y) {
                if (length(y) > 1) {
                    return(paste(y, collapse = "\t"))
                } else {
                    return(y)
                }
            })
            tmp <- paste(tmp, collapse = "\t")
        })
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
                                sprintf("#%s.alpha", stockname),
                                sprintf("#%s.beta", stockname),
                                sep = "\t")
    )
    spawn_params <- modifyList(spawn_params, dot_args)
    class(spawn_params) <- c("gadget_spawnfile", "list")
    return(spawn_params)
}



#' Create a list of class "gadget_fleet" that can be used to write out a Gadget fleet file
#'
#' This function takes named lists as arguments with elements that correspond to the different
#' parameters of a Gadget fleet component (see Gadget User Guide). Names of objects passed to
#' \code{...} will be used as the fleet names. The first element of the named list should by type,
#' which specifies the type that fleet will be
#' (i.e. \code{type = totalfleet, type = numberfleet}, etc.)
#'
#' WARNING: The implementation of this function for fleets of type quotafleet has not been
#' adequately tested. It should be expected that this portion of the function behaves unexpectedly.
#' If you encounter this and wish it to get fixed sooner rather than later,
#' post an issue to \url{https://github.com/inspktrgadget/gadgetSim}
#'
#'
#' @param ... Named objects corresponding to the appropriate arguments to various Gadget fleet types
#'
#' @return A list of class \code{gadget_fleet} (or a list of class \code{gadget_fleets} if
#' composed of multiple fleets)
#' @export
#'
#' @examples
#' base_data <- expand.grid(year = 1:10, steps = 1:4, area = 1, fleetname = "btm")
#' base_data$amount <- sample(1e5:1e6, nrow(base_data), replace = TRUE)
#' btm_fleet <-
#'   list(type = "totalfleet",
#'        suitability = exponentiall50_suit_formula("btm", "cod"),
#'        amount = base_data)
#' make_gadget_fleet(btm = btm_fleet)
#'
#' # different fleet type
#' lin_flt_data <- expand.grid(year = 1:10, steps = 1:4, area = 1, fleetname = "lin")
#' lin_flt_data$scaling <- c(seq(0.01,0.8, length.out = 20), seq(0.8,0.2,length.out = 20))
#' lin_flt <-
#'   list(type = "linearfleet",
#'        suitability = exponentiall50_suit_formula("lin", "cod"),
#'        amount = lin_flt_data)
#' make_gadget_fleet(lin = lin_flt)
#'
#' # can handle multiple fleet types
#' make_gadget_fleet(btm = btm_fleet, lin = lin_flt)
make_gadget_fleet <- function(...) {
    dots <- dots2list(...)
    fleets <-
        lapply(seq_along(dots), function(x, nms) {
            fleet <- dots[[x]]
            if (!check_names("^type", fleet)) {
                stop("Type of fleet must be one of the following", "\n",
                     paste(paste(" * ", fleet_types), collapse = "\n"))
            }
            sub_list <- function(l) {
                return(l[names(l) != "type"])
            }
            modify_fleet <- function(fleet_template, fleet) {
                out <- modifyList(fleet_template, sub_list(fleet))
                out[[fleet$type]] <- nms[x]
                filename <- sprintf("Data/fleet.%s.data", nms[x])
                out$amount <- filename
                attr(out, "amount") <- structure(fleet$amount, filename = filename)
                return(out)
            }
            if (fleet$type == "totalfleet") {
                out <- modify_fleet(totalfleet, fleet)
            } else if (fleet$type == "numberfleet") {
                out <- modify_fleet(numberfleet, fleet)
            } else if (fleet$type == "linearfleet") {
                out <- modify_fleet(linearfleet, fleet)
            } else if (fleet$type == "effortfleet") {
                out <- modify_fleet(effortfleet)
            } else if (fleet$type == "quotafleet") {
                out <- modifyList(quotafleet, sub_list(fleet))
                out$quotafleet <- nms[x]
                filename <- sprintf("Data/fleet.%s.data", nms[x])
                out$amount <- filename
                attr(out, "amount") <- structure(fleet$amount, filename = filename)
            } else {
                stop("Type of fleet file must be one of the following", "\n",
                     paste(paste(" * ", fleet_types), collapse = "\n"))
            }
            if (!identical(names(out), names(getFromNamespace(fleet$type, ns = "gadgetSim")))) {
                stop("You added a name that is not known to a fleet type or forgot a required one")
            }
            return(out)
        }, nms = names(dots))
    return(structure(fleets, class = c("gadget_fleets", "list")))
}


#' Make Gadget printfile and write out to file
#'
#' @param ... Any number of lists containing details of Gadget printfile components. Arguments to
#' \code{...} must be the same name as one of the default printfile lists,
#' see \code{\link{printfileDefaults}}
#'
#' @param main List of class \code{gadget_main} or character vector of the Gadget mainfile to be read
#' @param path Optional. Path to Gadget model
#' @param fit_dir Optional. Path to sub-directory of Gadget model where optimized files are housed
#' (i.e. WGTS from \code{\link[Rgadget]{Rgadget::gadget.fit}})
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
#' printfile <-  make_gadget_printfile(stock_std = list(stockname = "cod"),
#'                       path = gad_mod_dir)
make_gadget_printfile <- function(..., main = "main", path = NULL, fit_dir = NULL,
                                  output_dir = "out", aggfile_dir = "print.aggfiles",
                                  printatstart = 1, steps = "all") {
    if ("gadget_main" %in% class(main)) {
        main <- main
    } else {
        main <- read_gadget_main(file = main, path = path)
    }
    dir_name <- ifelse(is.null(path), getwd(), basename(path))
    dots <- dots2list(...)
    if (!any(names(dots) %in% pf_types)) {
        stop("... must have names", "\n",
             paste(paste0(" * ", pf_types), collapse = "\n"), "\n",
             "or be a list of objects with names as such")
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
    # make and add aggregate files for those components that need them
    added_aggfiles <- lapply(updated_printfiles, make_aggfiles,
                             aggfile_dir = aggfile_dir, path = path)
    names(added_aggfiles) <- names(dots)
    return(structure(added_aggfiles, class = c("gadget_printfile", "list")))
}


#' Create a data.frame of class "gadget_params"
#'
#' These functions create and update a data.frame of class \code{gadget_params}. This is the
#' parameter file for switches used in a Gadget model
#'
#' @return A data.frame of class \code{gadget_params}. \code{make_gadget_params} returns an
#' empty data.frame, and \code{init_params} updates the parameter found by \code{switch}.
#' @export
#'
#' @name params
#'
#' @examples
#' params <- make_gadget_params()
#' params <- init_params(params, "linf", 125, 115, 135)
#' params <- init_params(params, "k", 0.25, 0.1, 0.3, optimize = FALSE)
#'
#' # setup for efficient use with dplyr
#' params <-
#'   make_gadget_params() %>%
#'   init_params("linf", 125, 115, 135) %>%
#'   init_params("k", 0.25, 0.1, 0.3, optimize = FALSE)
make_gadget_params <- function() {
    return(structure(data.frame(switch = NULL, value = NULL, lower = NULL,
                                upper = NULL, optimize = NULL),
                     class = c("gadget_params", "data.frame")))
}

#' @rdname params
#' @export
init_params <- function(df, switch, value, lower, upper, optimize = TRUE) {
    # check to make sure that lower < upper
    if (upper < lower) {
        tmp <- lower
        lower <- upper
        upper <- tmp
    }
    # check to ensure that value is between bounds
    if (value < lower | value > upper) {
        stop(sprintf("The value for %s is outside of lower and upper bounds", switch))
    }
    row_num <- grep(switch, df$switch)
    optimize <- ifelse(optimize, 1, 0)
    if (length(row_num) == 0) {
        df <- rbind(df, data.frame(switch, value, lower, upper, optimize))
    } else if (length(row_num) == 1) {
        df[row_num, ] <- data.frame(switch, value, lower, upper, optimize)
    } else {
        stop(sprintf("There is more than one switch by the name of %s", switch))
    }
    class(df) <- c("gadget_params", "data.frame")
    return(df)
}


#' Assemble an entire Gadget model with a single function
#'
#' This function is the almagamation of all the make_gadget_* functions.
#' It allows a user to put together all aspects of a Gadget model for simulation
#' tesing purposes. Note that this function does not write a likelihood file.
#' If a Gadget model optimized to data is more desirable, then you should look to the
#' Rgadget package (specifically \code{\link[Rgadget]{gadget.iterative}}).
#'
#' This function utilizes the framework of gadgetSim functions to easily assemble and write files
#' for a Gadget simulation model. It does not call the model directly, rather it sets up all files
#' desired for inclusion into the Gadget model. You will still; however, have to set up the params
#' files separately.
#'
#' \code{...} should be a number of named elements corresponding to the following names: time,
#' area, stock (or stocks), and fleet (or fleets). Each of these named elements should also
#' correspond to their respective gadget_* classes as produced by the family of make_gadget_*
#' functions (i.e. \code{make_gadget_model(time = make_gadget_timefile(1985, 2015, "quarterly"))},
#' see example below).
#'
#' @return Nothing. A Gadget model with the basic necessary files will be written
#' to \code{path} if specified. Otherwise the Gadget model files will be written
#' to the current directory.
#' @export
#'
#' @examples
#' #------------------------------
#' # setup time and area
#' st_year <- 1985
#' end_year <- 2015
#' time <- make_gadget_timefile(st_year, end_year, "quarterly")
#' area <-
#'   make_gadget_areafile(areas = 1, size = 1e6,
#'                        temp_data = expand.grid(year = st_year:end_year, step = 1:4,
#'                                                area = 1, temp = 3))
#'
#' #------------------------------
#' # setup the stock
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
#'     list(stockname = "cod", livesonareas = 1, minage = minage, maxage = maxage,
#'          minlength = minlength, maxlength = maxlength, dl = dl)
#'
#' # setup refweightfile
#' stock_refwgt <-
#'     data.frame(len = reflength,
#'                weight = alpha * reflength ^ beta)
#'
#' # setup growth
#' linf <- 125
#' k <- 0.15
#' t0 <- -0.5
#' stock_growth <-
#'     list(growthfunction = "lengthvbsimple",
#'          growthparameters = c(linf, k, alpha, beta))
#'
#' # setup naturalmortality
#' stock_m <- rep(0.2, 10)
#'
#' # setup initial conditions
#' init_data <-
#'     normalparamfile(age = seq(minage, maxage, 1),
#'                     area = 1,
#'                     age.factor = 1e6 * exp(-cumsum(rep(0.2, (maxage - minage + 1)))),
#'                     area.factor = 1,
#'                     mean = vb_formula("cod", minage:maxage,
#'                                       params = list(linf = linf, k = k, t0 = t0)),
#'                     sd = c(1:10, rep(10, 10)),
#'                     alpha = alpha,
#'                     beta = beta)
#' stock_initcond <- list(normalparamfile = init_data)
#'
#' # setup spawning
#' stock_spawnfile <-
#'     make_gadget_spawnfile(
#'       stockname = "cod",
#'       start_year = st_year,
#'       end_year = end_year,
#'       proportionfunction = c("exponential", -0.25, 35),
#'       recruitment = bev_holt_formula("cod", params = c(4e08, 1.067e08)),
#'       stockparameters = c(20, 2, alpha, beta)
#'     )
#'
#' # create gadget stockfile
#' cod <-
#'    make_gadget_stockfile(stock = stock_info,
#'                          refweightfile = stock_refwgt,
#'                          growth = stock_growth,
#'                          naturalmortality = stock_m,
#'                          iseaten = 1,
#'                          initialconditions = stock_initcond,
#'                          spawning = stock_spawnfile)
#'
#' #------------------------------
#' # setup the fleet
#' lin_flt_data <- expand.grid(year = st_year:end_year,
#'                             steps = 1:4,
#'                             area = 1,
#'                             fleetname = "lin")
#' lin_flt_data <- lin_flt_data[order(lin_flt_data$year), ]
#' lin_flt_data$scaling <- c(seq(0.01,0.8, length.out = 62),
#'                           seq(0.8,0.2,length.out = 62))
#' lin_fleet <-
#'     list(type = "linearfleet",
#'          suitability = exponentiall50_suit_formula("lin", "cod"),
#'          amount = lin_flt_data)
#' lin <- make_gadget_fleet(lin = lin_fleet)
#'
#' #------------------------------
#' # simulate the Gadget model
#' make_gadget_model(time = time,
#'                   area = area,
#'                   stock = cod,
#'                   fleet = lin,
#'                   path = "test_model")
#'
#' # must set up params first, then can run
#' stock_std <- get_stock_std(path = "test_model")
#'
#' #------------------------------
#' # see the results
#' plot(number ~ year, subset(stock_std$cod, age == 2, step == 1), type = "l")
#'
#' # if using tidyverse
#' g <-
#'   ggplot(data=filter(stock_std$cod, step == 1), aes(x=year, y=number)) +
#'   geom_line() + facet_wrap(~age, scales = "free_y")
make_gadget_model <- function(..., path = NULL) {
    check_dir_exists(path)
    dots <- dots2list(...)
    # check and write time file
    if (check_names("^time", dots)) {
        if (!("gadget_time" %in% class(dots$time))) {
            stop("Gadget timefile must be of class gadget_time")
        } else {
            write_gadget_file(dots$time, path = path)
            timefile_name <- "time"
        }
    } else {timefile_name <- ""}
    check_dir_exists(check_path("Modelfiles"))
    # check and write areafile
    if (check_names("^area", dots)) {
        if (!("gadget_area" %in% class(dots$area))) {
            stop("Gadget areafile must be of class gadget_time")
        } else {
            write_gadget_file(dots$area, file = "Modelfiles/area", path = path)
            areafile_name <- "Modelfiles/area"
        }
    } else {areafile_name <- ""}
    # check and write stock files
    if (check_names("^stock|^stocks", dots)) {
        stocks <- dots[grep("^stock|^stocks", names(dots))]
        stockfile_names <-
            lapply(stocks, function(x) {
                if (!("gadget_stock" %in% class(x))) {
                    stop("Gadget stockfile must be of class gadget_stock or gadget_stocks")
                } else {
                    write_gadget_file(x, path = path)
                    return(x$stockname)
                }
            })
        stockfile_names <- paste(stockfile_names, collapse = "\t")
    } else {stockfile_names <- ""}
    # check and write fleet files
    if (check_names("^fleet|^fleets", dots)) {
        fleets <- dots[[grep("^fleet|^fleets", names(dots))]]
        if (!("gadget_fleets" %in% class(fleets))) {
            stop("Fleets must be of class gadget_fleets")
        } else {
            write_gadget_file(fleets, path = path)
            fleetfile_name <- "Modelfiles/fleet"
        }
    } else {fleetfile_name <- ""}
    # write out mainfile
    mainfile <- make_gadget_mainfile(list(timefile = timefile_name,
                                          areafile = areafile_name,
                                          stockfiles = stockfile_names,
                                          fleetfiles = fleetfile_name))
    write_gadget_file(mainfile, path = path)
}


## some core functions for this package

#' Call Gadget executable on the command line
#'
#' This function will call the gadget command with the given switches as arguments
#'
#' @param switches List of named switches to supply to Gadget executable.
#' Names correspond to the switch; values correspond to additional arguments.
#' If no argument is needed the value should be logical \code{TRUE}.
#' @param path Optional. Character vector of path to the directory where Gadget files are located
#' @param gadget_exe The Gadget executable to use
#' @param print_out Logical. Should Gadget command line output be printed.
#' @param print_err Logical. Should Gadget command line errors be printed.
#'
#' @return If \code{print_out = TRUE} or \code{print_err = TRUE}, a character vector giving the output
#' of the command, one line per string, else nothing
#' @export
#'
#' @examples
#' path <- system.file(gad_mod_dir, package = "gadgetSim")
#' call_gadget(switches = list(s = TRUE, i = "params.in", log = "logfile"), path = path)
#' call_gadget(switches = list(s = TRUE, i = "WGTS/params.final", main = "WGTS/main.final"),
#'             path = path)
call_gadget <- function(switches = list(s = TRUE, i = "params.in"), path = NULL,
                        gadget_exe = "gadget", print_out = TRUE, print_err = TRUE) {
    # check to make sure all files listed in switches are present in the Gadget model directory
    switch_test <- unlist(Filter(is.character, switches))
    check_files_exist(switch_test, path = path)
    switch_names <- paste0("-", names(switches))
    long_form_switch <- grep("version|help", switch_names)
    if (length(long_form_switch > 0)) {
        switch_names[long_form_switch] <- paste0("-", switch_names[long_form_switch])
    }
    switches[switches == TRUE] <- ""
    cmd_line_args <- paste(switch_names, switches)
    if (!is.null(path)) {
        Sys.setenv(GADGET_WORKING_DIR = normalizePath(path))
        on.exit(Sys.setenv(GADGET_WORKING_DIR = ""))
    }
    system2(gadget_exe, args = cmd_line_args, stdout = print_out, stderr = print_err)
}



#' Make and retrieve output from the StockStdPrinter printfile component of a Gadget model
#'
#' @inheritParams read_gadget_main
#' @inheritParams call_gadget
#' @inheritParams read_gadget_stockfiles
#' @param params_file Character. Path to the params file used to call Gadget
#' @param ... Additional arguments to include
#'
#' @return List of \code{data.frame}s, one for each stock, of output from StockStdPrinter
#' printfile component
#'
#' @export
#'
#' @examples
#' \dontrun{
#' path <- system.file(gad_mod_dir, package = "gadgetSim")
#' stocks_data <- get_stock_std(path = path)
#' }
get_stock_std <- function(main = "main", params_file = "params.in", path = NULL,
                          fit_dir = "FIT", gadget_exe = "gadget", ...) {
    if (requireNamespace("Rgadget", quietly = TRUE)) {
        if (dir.exists(check_path(fit_dir))) {
            if ("WGTS.Rdata" %in% dir(check_path(fit_dir))) {
                load(paste(check_path(fit_dir), "WGTS.Rdata", sep = "/"))
                if (exists("out")) {
                    got_stock_std <- TRUE
                    stocks <- unique(out$stock.std$stock)
                    stock_std <-
                        lapply(stocks, function(x) {
                            tmp <- subset(out$stock.std, stock == x)
                            tmp <- subset(tmp, select = year:biomass.consumed)
                            names(tmp) <-
                                c("year", "step", "area", "age", "number", "length",
                                  "weight", "length.sd", "number.consumed", "biomass.consumed")
                            return(tmp)
                        })
                    stock_std <- setNames(stock_std, stocks)
                    return(stock_std)
                } else {
                    got_stock_std <- FALSE
                }
            } else {
                got_stock_std <- FALSE
            }
        } else {
            got_stock_std <- FALSE
        }
    } else {
        got_stock_std <- FALSE
    }
    if (!got_stock_std) {
        if (!("gadget_main" %in% class(main))) {
            main <- read_gadget_main(main, path = path)
        }
        check_dir_exists(check_path(fit_dir))
        stocks <- get_stocknames(read_gadget_stockfiles(main = main, path = path))
        dots <- lapply(stocks, function(x) {
            return(list(stockname = x))
        })
        names(dots) <- rep("stock_std", length(dots))
        output_dir <- "out.fit"
        printfile_name <- "printfile.fit"
        aggfile_dir <- "print.aggfiles"
        if (!is.null(check_path(fit_dir))) {
            printfile_name <- paste(fit_dir, printfile_name, sep = "/")
            output_dir <- paste(fit_dir, output_dir, sep = "/")
            aggfile_dir <- paste(fit_dir, aggfile_dir, sep = "/")
            main_print <-
                modifyList(main, list(printfiles = printfile_name))
            write_gadget_file(main_print,
                              file = paste(fit_dir, "main.print", sep = "/"),
                              path = path)
            switches <-
                list(s = TRUE, i = params_file,
                     main = paste(fit_dir, "main.print", sep = "/"))
        } else {
            main_print <-
                modifyList(main, list(printfile = printfile_name))
            write_gadget_file(main_print,
                              file = "main.print",
                              path = path)
            switches <-
                list(s = TRUE, i = params_file,
                     main = "main.print")
        }
        printfile <-
            make_gadget_printfile(dots, main = main, path = path,
                                  output_dir = output_dir, aggfile_dir = aggfile_dir)
        write_gadget_file(printfile, file = printfile_name, path = path,
                          output_dir = output_dir, aggfile_dir = aggfile_dir)
        call_gadget(switches = switches, path = path, gadget_exe = gadget_exe,
                    print_out = FALSE, print_err = FALSE)
        stock_std <-
            read_gadget_stock_std(output_dir = output_dir, path = path)
        return(stock_std)
    }
}

#' Assemble an entire Gadget model with a single function
#'
#' This function is the almagamation and pinnacle of gadgetSim. It allows a user to put together
#' all aspects of a Gadget model for simulation tesing purposes. Note that this function does not
#' write a likelihood file. If a Gadget model optimized to data is more desirable, then you should
#' look to the Rgadget package (specifically \code{\link[Rgadget]{gadget.iterative}}).
#'
#' This function utilizes the framework of gadgetSim functions to easily assemble and write files
#' for a Gadget simulation model. It does not call the model directly, rather it sets up all files
#' desired for inclusion into the Gadget model. You will still; however, have to set up the params
#' files separately.
#'
#' \code{...} should be a number of named elements corresponding to the following names: time, area,
#' stock (or stocks), and fleet (or fleets). Each of these named elements should also correspond to
#' their respective gadget_* classes as produced by the family of make_gadget_* functions (i.e.
#' \code{simulate_gadget(time = make_gadget_timefile(1985, 2015, "quarterly"))}, see example below).
#'
#' @return Nothing. A Gadget model with the basic necessary files will be written to \code{path} if
#' specified. Otherwise the Gadget model files will be written to the current directory.
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
#'       start_year = st_year,
#'       end_year = end_year,
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
#'
#' #-------------------------------
#' # setup the fleet
#' lin_flt_data <- expand.grid(year = st_year:end_year, steps = 1:4, area = 1, fleetname = "lin")
#' lin_flt_data <- lin_flt_data[order(lin_flt_data$year), ]
#' lin_flt_data$scaling <- c(seq(0.01,0.8, length.out = 62), seq(0.8,0.2,length.out = 62))
#' lin_fleet <-
#'     list(type = "linearfleet",
#'          suitability = exponentiall50_suit_formula("lin", "cod"),
#'          amount = lin_flt_data)
#' lin <- make_gadget_fleet(lin = lin_fleet)
#'
#' #------------------------------
#' # simulate the Gadget model
#' simulate_gadget(time = time, area = area, stock = cod, fleet = lin, path = "test_model")
#'
#' # must set up params first, then can run
#' stock_std <- get_stock_std(path = "test_model")
simulate_gadget <- function(..., path = NULL) {
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


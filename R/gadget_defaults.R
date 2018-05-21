#' Gadget defaults
#'
#' Some pre-baked defaults to use in setting up Gadget simulations
#'
#' @name gadget_defaults
NULL

#' @rdname gadget_defaults
#' @export
gad_mod_dir <- "gadget_model"

#' @rdname gadget_defaults
#' @export
gadget_main_default <-
    structure(
        list(timefile = "time",
             areafile = "Modelfiles/area",
             printfiles = "; Required comment",
             stockfiles = "",
             tagfiles = "",
             otherfoodfiles = "",
             fleetfiles = "",
             likelihoodfiles = ""),
        class = c("gadget.main", "list"))

#' @rdname gadget_defaults
gadget_timefile_default <-
    structure(
        list(firstyear = NULL,
             firststep = NULL,
             lastyear = NULL,
             laststep = NULL,
             notimesteps = NULL),
        class = c("gadget.time", "list")
    )

#' @rdname gadget_defaults
gadget_areafile_default <-
    structure(
        list(areas = NULL,
             size = NULL,
             temperature = NULL),
        class = c("gadget.area", "list")
    )
#' @rdname gadget_defaults
gadget_stockfile_default <-
    structure(
        list(stockname = NULL,
             livesonareas = NULL,
             minage = NULL,
             maxage = NULL,
             minlength = NULL,
             maxlength = NULL,
             dl = NULL,
             refweightfile = NULL,
             growthandeatlengths = NULL,
             doesgrow = 0,
             naturalmortality = NULL,
             iseaten = 0,
             doeseat = 0,
             initialconditions = NULL,
             doesmigrate = 0,
             doesmature = 0,
             doesmove = 0,
             doesrenew = 0,
             doesspawn = 0,
             doesstray = 0),
        class = c("gadget.stock", "list")
    )

#' @rdname gadget_defaults
gadget_tagfile_default <-
    structure(
        list(tagid = NULL,
             stock = NULL,
             tagarea = NULL,
             endyear = NULL,
             tagloss = NULL,
             numbers = NULL),
        class = c("gadget.tagfile", "list")
    )

#' @rdname gadget_defaults
gadget_otherfood_default <-
    structure(
        list(foodname = NULL,
             livesonareas = NULL,
             lengths = NULL,
             energycontent = NULL,
             amount = NULL),
        class = c("gadget.otherfood", "list")
    )


#' Fleet Defaults
#'
#' Pre-baked default lists to use for reading in fleets
#'
#' @name fleet_defaults
NULL

#' @rdname fleet_defaults
totalfleet <-
    structure(
        list(totalfleet = "",
             livesonareas = "",
             multiplicative = "",
             suitability = "",
             amount = ""),
        class = c("gadget.fleet", "list"))

#' @rdname fleet_defaults
numberfleet <- totalfleet
names(numberfleet)[1] <- "numberfleet"

#' @rdname fleet_defaults
linearfleet <- totalfleet
names(linearfleet)[1] <- "linearfleet"

#' @rdname fleet_defaults
effortfleet <- totalfleet
names(effortfleet)[1] <- "effortfleet"

#' @rdname fleet_defaults
quotafleet <-
    structure(
        list(quotafleet = "",
             livesonareas = "",
             multiplicative = "",
             suitability = "",
             quotafunction = "",
             biomasslevel = "",
             quotalevel = "",
             amount = ""),
        class = c("gadget.fleet", "list"))


#' Switch defaults
#'
#' These are vectors of the switches that are used in when calling Gadget
#' from the command line, but you figured out a different way to implement this
#' You can probably delete these objects
#'
#' @name switches
single_switch <- c("s", "l", "n", "v", "h", "i", "opt", "main",
                   "p", "o", "print", "precision", "log", "loglevel",
                   "seed", "m", "printinitial", "printfinal", "maxratio")

#' @rdname switches
double_switch <- c("version", "help")

#' @rdname switches
file_switch <- single_switch[6:length(single_switch)]

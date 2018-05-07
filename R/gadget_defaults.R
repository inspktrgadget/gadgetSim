#' Gadget defaults
#'
#' Some pre-baked defaults to use in setting up Gadget simulations
#'
#' @name gadgetDefaults
NULL

#' @rdname gadgetDefaults
#' @export
gad_mod_dir <- "gadget_model"

#' @rdname gadgetDefaults
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


#' Fleet Defaults
#'
#' Pre-baked default lists to use for reading in fleets
#'
#' @name fleetDefaults
NULL

#' @rdname fleetDefaults
totalfleet <-
    structure(list(totalfleet = "",
                   livesonareas = "",
                   multiplicative = "",
                   suitability = "",
                   amount = ""),
              class = c("gadget.fleet", "list"))

#' @rdname fleetDefaults
numberfleet <- totalfleet
names(numberfleet)[1] <- "numberfleet"

#' @rdname fleetDefaults
linearfleet <- totalfleet
names(linearfleet)[1] <- "linearfleet"

#' @rdname fleetDefaults
effortfleet <- totalfleet
names(effortfleet)[1] <- "effortfleet"

#' @rdname fleetDefaults
quotafleet <-
    structure(list(quotafleet = "",
                   livesonareas = "",
                   multiplicative = "",
                   suitability = "",
                   quotafunction = "",
                   biomasslevel = "",
                   quotalevel = "",
                   amount = ""),
              class = c("gadget.fleet", "list"))





single_switch <- c("s", "l", "n", "v", "h", "i", "opt", "main",
                   "p", "o", "print", "precision", "log", "loglevel",
                   "seed", "m", "printinitial", "printfinal", "maxratio")

double_switch <- c("version", "help")

file_switch <- single_switch[6:length(single_switch)]

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
        class = c("gadget_main", "list"))

#' @rdname gadget_defaults
gadget_timefile_default <-
    structure(
        list(firstyear = NULL,
             firststep = NULL,
             lastyear = NULL,
             laststep = NULL,
             notimesteps = NULL),
        class = c("gadget_time", "list")
    )

#' @rdname gadget_defaults
gadget_areafile_default <-
    structure(
        list(areas = NULL,
             size = NULL,
             temperature = NULL),
        class = c("gadget_area", "list")
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
        class = c("gadget_stock", "list")
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
        class = c("gadget_tagfile", "list")
    )

#' @rdname gadget_defaults
gadget_otherfood_default <-
    structure(
        list(foodname = NULL,
             livesonareas = NULL,
             lengths = NULL,
             energycontent = NULL,
             amount = NULL),
        class = c("gadget_otherfood", "list")
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
             livesonareas = 1,
             multiplicative = 1,
             suitability = "",
             amount = ""),
        class = c("gadget_fleet", "list"))

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
        class = c("gadget_fleet", "list"))

fleet_types <-
    c("totalfleet", "numberfleet", "linearfleet", "effortfleet", "quotafleet")


# defaults for likelihood components
penalty_likelihood <-
    structure(list(
        name = "penalty",
        weight = 1,
        type = "penalty",
        data = data.frame(switch = "default", power = 2,
                          lower = 1000, upper = 1000)),
        class = c("gadget_penalty_likelihood", "list"))

understocking_likelihood <-
    structure(list(
        name = "understocking",
        weight = 1,
        type = "understocking",
        powercoeff = NULL
    ), class = c("gadget_understocking_likelihood", "list"))

catchdistribution_likelihood <-
    structure(list(
        name = "",
        weight = 1,
        type = "catchdistribution",
        data = "",
        `function` = "sumofsquares",
        lag = NULL,
        sigma = NULL,
        param = NULL,
        aggregationlevel = NULL,
        overconsumption = NULL,
        epsilon = NULL,
        areaaggfile = "",
        ageaggfile = "",
        lenaggfile = "",
        fleetnames = "",
        stocknames = ""
    ), class = c("gadget_catchdistribution_likelihood", "list"))

catchstatistics_likelihood <-
    structure(list(
        name = "",
        weight = 1,
        type = "catchstatistics",
        data = "",
        `function` = "lengthcalcstddev",
        overconsumption = 0,
        areaaggfile = "",
        lenaggfile = NULL,
        ageaggfile = "",
        fleetnames = "",
        stocknames = ""
    ), class = c("gadget_catchstatistics_likelihood", "list"))

stockdistribution_likelihood <-
    structure(list(
        name = "",
        weight = 1,
        type = "stockdistribution",
        data = "",
        `function` = "sumofsquares",
        aggregationlevel = NULL,
        overconsumption = NULL,
        epsilon = NULL,
        areaaggfile = "",
        ageaggfile = "",
        lenaggfile = "",
        fleetnames = "",
        stocknames = ""
    ), class = c("gadget_stockdistribution_likelihood", "list"))

surveyindices_likelihood <-
    structure(list(
        name = "",
        weight = 1,
        type = "surveyindices",
        data = "",
        sitype = "",
        biomass = NULL
    ), class = c("gadget_surveyindices_likelihood", "list"))

si_lengths <-
    modifyList(surveyindices_likelihood,
               list(sitype = "lengths",
                    areaaggfile = "",
                    lenaggfile = "",
                    stocknames = ""))

si_ages <-
    modifyList(surveyindices_likelihood,
               list(sitype = "ages",
                    areaggfile = "",
                    lenaggfile = "",
                    stocknames = ""))

si_fleets <-
    modifyList(surveyindices_likelihood,
               list(sitype = "fleets",
                    areaggfile = "",
                    lenaggfile = "",
                    fleetnames = "",
                    stocknames = ""))

si_acoustic <-
    modifyList(surveyindices_likelihood,
               list(sitype = "acoustic",
                    areaaggfile = "",
                    surveynames = "",
                    stocknames = ""))

si_effort <-
    modifyList(surveyindices_likelihood,
               list(sitype = "effort",
                    areaaggfile = "",
                    fleetnames = "",
                    stocknames = ""))

surveydistribution_likelihood <-
    structure(list(
        name = "",
        weight = 1,
        type = "surveydistribution",
        data = "",
        areaaggfile = "",
        lenaggfile = "",
        ageaggfile = "",
        stocknames = "",
        fittype = "",
        parameters = "",
        suitability_parameters = "",
        epsilon = 1,
        likelihoodtype = "multinomial"
    ), class = c("gadget_surveydistribution_likelihood", "list"))

stomachcontent_likelihood <-
    structure(list(
        name = "",
        weight = 1,
        type = "stomachcontent",
        `function` = "scsimple",
        data = "",
        epsilon = NULL,
        areaaggfile = "",
        predatornames = "",
        predatorlengths = "",
        lenaggfile = "",
        preyaggfile = ""
    ), class = c("gadget_stomachconent_likelihood", "list"))

recaptures_likelihood <-
    structure(list(
        name = "",
        weight = 1,
        type = "recaptures",
        data = "",
        `function` = "poisson",
        areaaggfile = "",
        lenaggfile = "",
        fleetnames = ""
    ), class = c("gadget_recaptures_likelihood", "list"))

recstatistics_likelihood <-
    structure(list(
        name = "",
        weight = 1,
        type = "recstatistics",
        data = "",
        `function` = "lengthcalcstddev",
        areaaggfile = "",
        fleetnames = ""
    ), class = c("gadget_recstatistics_likelihood", "list"))

migrationpenalty_likelihood <-
    structure(list(
        name = "",
        weight = 1,
        type = "migrationpenalty",
        stockname = "",
        powercoeffs = c(2, 1)
    ), class = c("gadget_migrationpenalty_likelihood", "list"))

migrationproportion_likelihood <-
    structure(list(
        name = "",
        weight = 1,
        type = "migrationproportion",
        data = "",
        `function` = "sumofsquares",
        biomass = NULL,
        areaaggfile = "",
        stocknames = ""
    ), class = c("gadget_migrationproportion_likelhood", "list"))

catchinkilos_likelihood <-
    structure(list(
        name = "",
        weight = 1,
        type = "catchinkilos",
        data = "",
        `function` = "sumofsquares",
        aggregationlevel = NULL,
        epsilon = NULL,
        areaaggfile = "",
        fleetnames = "",
        stocknames = ""
    ), class = c("gadget_catchinkilos_likelihood", "list"))


# optinfofile components
hooke_optinfo <-
    list(
        hookeiter = 50000,
        hookeeps = 1e-04,
        rho = 0.5,
        lambda = 0
    )

simann_optinfo <-
    list(
        simanniter = 3e05,
        scale = 1,
        simanneps = 1e-03,
        t = 3e06,
        rt = 0.85,
        nt = 2,
        ns = 5,
        vm = 1,
        cstep = 2,
        lratio = 0.3,
        uratio = 0.7,
        check = 4
    )

bfgs_optinfo <-
    list(
        bfgsiter = 50000,
        bfgseps = 0.01,
        sigma = 0.01,
        beta = 0.3,
        gradacc = 1e-06,
        gradstep = 0.5,
        gradeps = 1e-10
    )

pso_optinfo <-
    list(
        goal = 1e-06,
        psoiter = 1e09
    )

de_optinfo <-
    list(
        goal = 1e-06,
        iter = 1e09
    )


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

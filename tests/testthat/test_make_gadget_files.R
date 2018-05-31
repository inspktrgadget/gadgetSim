context("Test functions to make Gadget files")

test_that("make_gadget_mainfile produces correct output", {
    main_test <-
        structure(list(
            timefile = "foo",
            areafile = "bar",
            printfiles = "; Required comment",
            stockfiles = "baz",
            tagfiles = "",
            otherfoodfiles = "",
            fleetfiles = "",
            likelihoodfiles = "likelihood"
        ), class = c("gadget_main", "list"))
    expect_equal(make_gadget_mainfile(timefile = "foo", areafile = "bar",
                                      stockfiles = "baz", likelihoodfiles = "likelihood"),
                 main_test)
    expect_error(make_gadget_mainfile(stock = "cod"))
})

test_that("make_gadget_timefile produces correct output under different scenarios", {
    quarterly_time <-
        list(firstyear = 1985, firststep = 1, lastyear = 2015, laststep = 4,
             notimesteps = c(4,3,3,3,3))
    class(quarterly_time) <- c("gadget_time", "list")
    biannual_time <-
        modifyList(quarterly_time,
                   list(laststep = 2, notimesteps = c(2, 6, 6)))
    annual_time <-
        modifyList(quarterly_time,
                   list(laststep = 1, notimesteps = c(1,12)))
    monthly_time <-
        modifyList(quarterly_time,
                   list(laststep = 12, notimesteps = c(12, rep(1,12))))
    custom_time <-
        modifyList(quarterly_time,
                   list(notimesteps = c(4, 2, 4, 4, 2)))
    expect_equal(make_gadget_timefile(1985, 2015, "quarterly"), quarterly_time)
    expect_equal(make_gadget_timefile(1985, 2015, "annually"), annual_time)
    expect_equal(make_gadget_timefile(1985, 2015, "biannual"), biannual_time)
    expect_equal(make_gadget_timefile(1985, 2015, "monthly"), monthly_time)
    expect_equal(make_gadget_timefile(1985, 2015, timesteps = c(4, 2, 4, 4, 2)), custom_time)
})

test_that("make_gadget_areafile returns the correct output", {
    temp_data <-
        expand.grid(year = 1:5, step = 1:4,
                    area = 1, mean = 3)
    area_list <-
        list(areas = 1, size = 100, temperature = temp_data)
    class(area_list) <- c("gadget_area", "list")
    temp2_data <-
        expand.grid(year = 1:2, step = 1:4,
                    area = 1:2, mean = 3)
    two_areas <-
        list(areas = 1:2, size = c(100, 150), temperature = temp2_data)
    class(two_areas) <- c("gadget_area", "list")
    expect_equal(make_gadget_areafile(1, 100, temp_data), area_list)
    expect_equal(make_gadget_areafile(1:2, c(100, 150), temp2_data), two_areas)

})

test_that("make_gadget_stockfile returns the correct output", {
    minage <- 1
    maxage <- 10
    minlength <- 1
    maxlength <- 100
    dl <- 1
    stock_info <-
        list(stockname = "cod",
             livesonareas = 1,
             minage = minage,
             maxage = maxage,
             minlength = minlength,
             maxlength = maxlength,
             dl = dl)
    alpha <- 0.0001
    beta <- 3
    stock_growth <-
        list(growthfunction = "lengthvbsimple",
             growthparameters = c(to_gadget_formula(quote(cod.linf)),
                                  to_gadget_formula(quote(cod.k)),
                                  alpha, beta))
    stock_m <- rep(0.3, 10)
    stock_initcond <-
        normalparamfile(year = 1,
                        area = 1,
                        age.factor = 10,
                        area.factor = 10,
                        mean = vb_formula("cod", 1:10),
                        stddev = 1:10,
                        alpha = alpha,
                        beta = beta)
    stock_spawnfile <- make_gadget_spawnfile("cod", 1985, 2015)
    reflength <- minlength:maxlength
    refwgt <- data.frame(length = reflength,
                         weight = alpha * (reflength) ^ beta)
    lenaggfile <- attr(make_stock_lenaggfile(dots2list(stock = stock_info)), "lenaggfile")
    make_stockfile_test <-
        structure(list(
            stockname = "cod",
            livesonareas = 1,
            minage = minage,
            maxage = maxage,
            minlength = minlength,
            maxlength = maxlength,
            dl = dl,
            refweightfile = "Modelfiles/cod.refweightfile",
            growthandeatlengths = "Aggfiles/cod.stock.len.agg",
            doesgrow = 1,
            growthfunction = "lengthvbsimple",
            growthparameters = c("#cod.linf", "#cod.k", 0.0001, 3),
            beta = "(* #cod.bbin.mult #cod.bbin)",
            maxlengthgroupgrowth = 15,
            naturalmortality = stock_m,
            iseaten = 0,
            doeseat = 0,
            initialconditions = "",
            minage = 1,
            maxage = 10,
            minlength = 1,
            maxlength = 100,
            dl = 1,
            normalparamfile = "Modelfiles/cod.init.normalparamfile",
            doesmigrate = 0,
            doesmature = 0,
            doesmove = 0,
            doesrenew = 0,
            doesspawn = 1,
            spawnfile = "Modelfiles/cod.spawnfile",
            doesstray = 0
        ),
        refweightfile = structure(refwgt, filename = "Modelfiles/cod.refweightfile"),
        growthandeatlengths = structure(lenaggfile, filename = "Aggfiles/cod.stock.len.agg"),
        initialconditions = structure(stock_initcond,
                                      filename = "Modelfiles/cod.init.normalparamfile"),
        spawning = structure(stock_spawnfile, filename = "Modelfiles/cod.spawnfile"),
        class = c("gadget_stock", "list"))
    expect_equal(make_gadget_stockfile(stock = stock_info,
                                       growth = stock_growth,
                                       naturalmortality = stock_m,
                                       initialconditions = stock_initcond,
                                       spawning = stock_spawnfile),
                 make_stockfile_test)
})

test_that("make_gadget_fleet produces the correct output", {
    base_data <- expand.grid(year = 1:10, steps = 1:4, area = 1, fleetname = "comm")
    base_data$amount <- sample(1e5:1e6, nrow(base_data), replace = TRUE)
    btm_fleet <-
      list(type = "totalfleet",
           suitability = exponentiall50_suit_formula("comm", "cod"),
           amount = base_data)
    fleet_test <-
        structure(list(
            totalfleet = "comm",
            livesonareas = 1,
            multiplicative = 1,
            suitability = "cod\tfunction\tnewexponentiall50\t#cod.comm.alpha\t#cod.comm.l50",
            amount = "Data/fleet.comm.data"),
            amount = structure(base_data, filename = "Data/fleet.comm.data"),
            class = c("gadget_fleet", "list"))
    fleet_test <- structure(list(fleet_test), class = c("gadget_fleets", "list"))
    expect_equal(make_gadget_fleet(comm = btm_fleet), fleet_test)
})

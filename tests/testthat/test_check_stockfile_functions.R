context("Testing check_stockfile_* functions")

# setup basic stock information
minage <- 1
maxage <- 10
minlength <- 1
maxlength <- 100
dl <- 1
alpha <- 1e-04
beta <- 3
reflength <- seq(minlength, maxlength, dl)
stock_info <-
    list(stockname = "cod", livesonareas = 1, minage = 1, maxage = 10,
         minlength = minlength, maxlength = maxlength, dl = dl)

# setup refweightfile
stock_refwgt <-
    data.frame(length = reflength,
               weight = alpha * reflength ^ beta)

# setup growth
stock_growth <-
    list(growthfunction = "lengthvbsimple",
         growthparameters =
             c(to_gadget_formula(quote(cod.linf)), to_gadget_formula(quote(cod.k)),
               alpha, beta))
# setup naturalmortality
stock_m <- rep(0.15, 10)

# setup initial conditions
init_data <-
    normalparamfile(age = seq(minage, maxage, 1),
                    area = 1,
                    age.factor = "#cod.init.age",
                    area.factor = "#cod.init.area",
                    mean = vb_formula("cod", minage:maxage),
                    sd = 1:10,
                    alpha = alpha,
                    beta = beta)
stock_initcond <- list(normalparamfile = init_data)

# setup spawning
stock_spawnfile <-
    make_gadget_spawnfile(
        stockname = "cod",
        start_year = 1,
        end_year = 10,
        recruitment = ricker_formula("cod")
    )

dots <- dots2list(stock = stock_info,
                  refweightfile = stock_refwgt,
                  growth = stock_growth,
                  initialconditions = stock_initcond,
                  doesspawn = stock_spawnfile)

lenaggfile <- make_stock_lenaggfile(dots2list(stock = stock_info))

dummy_df <- data.frame(a = 1, b = 1, c = 1)

test_that("check_stockfile_* funs produce errors when appropriate", {
    # check_stock_info
    stockname_err <-
        dots2list(stock = list(liveonareas = 1))
    stock_len_err <-
        dots2list(stock = list(stockname = "cod", livesonareas = 1))
    stock_age_err <-
        dots2list(stock = list(stockname = "cod",
                               livesonareas = 1,
                               minlen = 1, maxlength = 100, dl = 1))
    expect_error(check_stock_info(stockname_err))
    expect_error(check_stock_info(stock_len_err))
    expect_error(check_stock_info(stock_age_err))
    # check_stock_refweightfile
    refwgt_err <-
        dots2list(refweightfile = c(1,2,3,4))
    expect_error(check_stock_refweightfile(refwgt_err))
    refwgt_warn <-
        dots2list(stock = stock_info,
                  growth = list(growthfunction = "lengthvb"))
    expect_warning(check_stock_refweightfile(refwgt_warn))
    # check_stock_grw_eat_len
    grw_eat_len_err <-
        dots2list(growthandeatlengths = c(1,2,3,4))
    expect_error(check_stock_grw_eat_len(grw_eat_len_err))
    # check_stock_m
    expect_message(check_stock_m(dots2list(stock = stock_info)))
    # check_stock_predator
    expect_error(check_stock_predator(dots2list(doeseat = 1)))
    # check_stock_initcond
    expect_error(check_stock_initcond(dots2list(initcond = dummy_df)))
    expect_error(check_stock_initcond(dots2list(stock = stock_info, growth = stock_growth)))
    # check_stock_migration
    expect_error(check_stock_migration(dots2list(migration = dummy_df)))
    # check_stock_spawning
    expect_error(check_stock_spawning(dots2list(stock = stock_info, spawning = dummy_df)))
})



test_that("check_stockfile_* funs produce the correct output", {
    # check_stock_info
    stock_info_test <-
        dots2list(stock = stock_info)
    expect_equal(check_stock_info(stock_info_test), stock_info)
    # make_stock_lenaggfile
    reflen <- seq(minlength, maxlength, dl)
    lenaggfile_test <-
        structure("Aggfiles/cod.stock.len.agg",
                  lenaggfile = data.frame(name = paste0("len", reflen[-1]),
                                          lower = reflen[-length(reflen)],
                                          upper = reflen[-1],
                                          stringsAsFactors = FALSE))
    expect_equal(make_stock_lenaggfile(dots2list(stock = stock_info)), lenaggfile_test)
    # check_stock_refweightfile
    refwgt_test <-
        structure(list(
            refweightfile = "Modelfiles/cod.refweightfile"),
            refweightfile = structure(stock_refwgt, filename = "Modelfiles/cod.refweightfile"))
    expect_equal(check_stock_refweightfile(dots2list(stock = stock_info,
                                                     refweightfile = stock_refwgt)),
                 refwgt_test)
    # check_stock_refweightfile default
    expect_equal(check_stock_refweightfile(dots2list(stock = stock_info,
                                                     growth = stock_growth)),
                 refwgt_test)
    # check_stock_grw_eat_len
    grw_eat_len_test <-
        structure(list(
            growthandeatlengths = "Aggfiles/cod.stock.len.agg"),
            lenaggfile = structure(attr(lenaggfile_test, "lenaggfile"),
                                        filename = "Aggfiles/cod.stock.len.agg"))
    expect_equal(check_stock_grw_eat_len(dots2list(stock = stock_info),
                                         lenaggfile = lenaggfile_test),
                 grw_eat_len_test)
    # check_stock_growth
    growth_test <-
        c(list(doesgrow = 1), stock_growth,
          list(beta = "(* #cod.bbin.mult #cod.bbin)", maxlengthgroupgrowth = 15))
    expect_equal(check_stock_growth(dots2list(stock = stock_info)), list(doesgrow = 0))
    expect_equal(check_stock_growth(dots2list(stock = stock_info, growth = stock_growth)),
                 growth_test)
    # check_stock_m
    m_test <-
        list(naturalmortality = rep(0.2, (maxage - minage + 1)))
    expect_equal(check_stock_m(dots2list(stock = stock_info)), m_test)
    expect_equal(check_stock_m(dots2list(stock = stock_info,
                                         naturalmortality = stock_m)),
                 list(naturalmortality = stock_m))
    # check_stock_iseaten
    iseaten_test <-
        structure(list(
            iseaten = 1,
            preylengths = lenaggfile[1],
            energycontent = 1),
            lenaggfile = structure(attr(lenaggfile, "lenaggfile"),
                                   filename = "Aggfiles/cod.stock.len.agg"))
    preylenaggfile <- data.frame(name = "len1", lower = 1, upper = 2)
    iseaten_test2 <-
        structure(list(
            iseaten = 1,
            preylengths = "Aggfiles/cod.preylengths",
            energycontent = 1),
            preylengths = structure(preylenaggfile, filename = "Aggfiles/cod.preylengths"))
    iseaten_test3 <- modifyList(iseaten_test, list(energycontent = 20))
    iseaten_test4 <- modifyList(iseaten_test2, list(energycontent = 20))
    expect_equal(check_stock_iseaten(dots2list(stock = stock_info, iseaten = 1), lenaggfile),
                 iseaten_test)
    expect_equal(check_stock_iseaten(dots2list(stock = stock_info,
                                               iseaten = list(iseaten = 1,
                                                              preylengths = preylenaggfile))),
                 iseaten_test2)
    expect_equal(check_stock_iseaten(dots2list(stock = stock_info,
                                               iseaten = list(iseaten = 1,
                                                              energycontent = 20)), lenaggfile),
                 iseaten_test3)
    expect_equal(check_stock_iseaten(dots2list(stock = stock_info,
                                               iseaten = list(iseaten = 1,
                                                             preylengths = preylenaggfile,
                                                             energycontent = 20))),
                 iseaten_test4)
    expect_equal(check_stock_iseaten(dots2list(stock = stock_info)), list(iseaten = 0))
    # check_stock_predator
    predator_test <-
        list(suitability = "constant", preference = 6,
             maxconsumption = 1, halffeedingvalue = 0.5)
    expect_equal(check_stock_predator(dots2list(doeseat = predator_test)),
                 c(list(doeseat = 1), predator_test))
    # check_stock_initcond
    initcond_test <-
        structure(list(
            initialconditions = "",
            minage = minage,
            maxage = maxage,
            minlength = minlength,
            maxlength = maxlength,
            dl = dl,
            normalparamfile = "Modelfiles/cod.init.normalparamfile"
            ), normalparamfile = structure(stock_initcond$normalparamfile,
                                           filename = "Modelfiles/cod.init.normalparamfile"))
    expect_equal(check_stock_initcond(dots2list(stock = stock_info,
                                                growth = stock_growth,
                                                initialconditions = stock_initcond)),
                 initcond_test)
    initcond_test2 <-
        list(
            initialconditions = "",
            minage = 0,
            maxage = 5,
            minlength = 10,
            maxlength = 50,
            dl = 5,
            sdev = 2.5,
            normalparamfile = stock_initcond$normalparamfile)
    init_test2 <-
        structure(list(
            initialconditions = "",
            minage = 0,
            maxage = 5,
            minlength = 10,
            maxlength = 50,
            dl = 5,
            sdev = 2.5,
            normalparamfile = "Modelfiles/cod.init.normalparamfile"
        ), normalparamfile = structure(stock_initcond$normalparamfile,
                                       filename = "Modelfiles/cod.init.normalparamfile"))
    expect_equal(check_stock_initcond(dots2list(stock = stock_info,
                                                growth = stock_growth,
                                                initialconditions = initcond_test2)),
                 init_test2)
    # you need to set up tests for the following functions
    # check_stock_migration
    # check_stock_maturity
    # check_stock_movement
    # check_stock_renewal
    renew_test <-
        list(
            doesrenew = 1,
            minlength = minlength,
            maxlength = maxlength,
            dl = dl,
            normalparamfile = "Modelfiles/cod.rec.normalparamfile")
    renew_dat <- data.frame(a = 1, b = 2)
    class(renew_dat) <- c("normalparamfile", "data.frame")
    attr(renew_test, "normalparamfile") <- structure(renew_dat,
                                                     filename = "Modelfiles/cod.rec.normalparamfile")
    expect_equal(check_stock_renewal(dots2list(stock = stock_info,
                                               renewal = normalparamfile(a = 1, b = 2))),
                 renew_test)
    # check_stock_spawning
    spawn_test <-
        list(doesspawn = 1,
             spawnfile = "Modelfiles/cod.spawnfile")
    attr(spawn_test, "spawnfile") <- structure(stock_spawnfile,
                                               filename = "Modelfiles/cod.spawnfile")
    expect_equal(check_stock_spawning(dots2list(stock = stock_info, spawning = stock_spawnfile)),
                 spawn_test)
    # check_stock_straying
    stray_test <-
        list(doesstray = 1,
             strayfile = "Modelfiles/cod.strayfile")
    attr(stray_test, "strayfile") <-
        structure(list(a = 1, b = 2), filename = "Modelfiles/cod.strayfile")
    expect_equal(check_stock_straying(dots2list(stock = stock_info, straying = list(a = 1, b = 2))),
                 stray_test)
})

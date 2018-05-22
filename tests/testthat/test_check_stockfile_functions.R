context("Testing check_stockfile_* functions")

stockname <- "cod"
stock_minage <- 1
stock_maxage <- 10
stock_minlength <- 1
stock_maxlength <- 100
stock_dl <- 1

stock_info <-
    list(stockname = stockname, livesonareas = 1,
         minage = stock_minage, maxage = stock_maxage,
         minlength = stock_minlength, maxlength = stock_maxlength, dl = stock_dl)
stock_growth <-
    list(doesgrow = 1,
         growthfunction = "lengthvbsimple",
         growthparameters = c("#cod.linf", "#cod.k", 0.0001, 3))
stock_m <-
    list(naturalmortality = rep(0.2, (stock_maxage - stock_minage + 1)))
stock_initcond <-
    normalparamfile(year = 1:10,
                    area = 1,
                    age.factor = 10,
                    area.factor = 10,
                    mean = vb_formula("cod", 1:10),
                    stddev = 1:10,
                    alpha = 0.0001,
                    beta = 3)
stock_init <-
    list(initialconditions = stock_initcond)
stock_spawning <-
    list(doesspawn =1,
         spawnfile = make_gadget_spawnfile(stockname, 1, 10))


test_that("check_stockfile_* functions produce errors when they should", {
    # check growth
    nogrowth <- c(stock_info, list(doesgrow = 1))
    expect_error(check_stock_growth(nogrowth))
    # check initial conditions
    init_cond_err <- c(stock_info, list(initialconditions = c(1,2,3,4)))
    expect_error(check_stock_initcond(init_cond_err))
    init_cond_df <- c(stock_info, list(initialconditions = data.frame(a = 1, b = 2)))
    expect_error(check_stock_initcond(init_cond_df))
    expect_error(check_stock_initcond(stock_info))
    # check migration
    no_migration <- c(stock_info, list(doesmigrate = 1))
    expect_error(check_stock_migration(no_migration))
    # check maturity
    no_mat_fun <- c(stock_info, list(doesmature = 1))
    no_mat_file <- modifyList(no_mat_fun, list(maturityfunction = "newconstant"))
    expect_error(check_stock_maturity(no_mat_fun))
    expect_error(check_stock_maturity(no_mat_file))
    # check movement
    no_move <- c(stock_info, list(doesmove = 1))
    no_tr_step <- c(stock_info,
                    list(doesmove = 1,
                         transitionstockandratios = data.frame(stock = c("cod", ratio = 0.5))))
    expect_error(check_stock_movement(no_move))
    expect_error(check_stock_movement(no_tr_step))
    # check renewal
    no_renewal_data <- c(stock_info, list(doesrenew = 1))
    no_usefule_renewal_data <- c(stock_info, list(doesrenew = 1, dl = 1))
    expect_error(check_stock_renewal(no_renewal_data))
    expect_error(check_stock_renewal(no_usefule_renewal_data))
    # check spawning
    no_spawn_data <- c(stock_info, list(doesspawn = 1))
    expect_error(check_stock_spawning(no_spawn_data))
    # check straying
    no_stray_data <- c(stock_info, list(doesstray = 1))
    expect_error(check_stock_straying(no_stray_data))

})

test_that("check_stockfile_* functions produce the correct output", {

})

context("Reading gadget files")

path <- system.file(gad_mod_dir, package = "gadgetSim")
main <- read_gadget_main(path = path)

test_that("read_gadget_main appropriately reads Gadget mainfiles", {
    expected_main <-
        structure(list(
            timefile = "time",
            areafile = "Modelfiles/area",
            printfiles = character(0),
            stockfiles = c("cod0", "cod"),
            fleetfiles = "Modelfiles/fleet",
            likelihoodfiles = "likelihood"
        ), class = c("gadget.main", "list"))
    expect_equal(main, expected_main)
})

test_that("read_gadget_stockfiles is of the appropriate length and class", {
    stocks <- read_gadget_stockfiles(main = main, path = path)
    expect_equal(length(stocks), 2)
    expect_is(stocks, c("gadget.stocks", "list"))
    expect_is(stocks[[1]], c("gadget.stock", "list"))
    expect_is(stocks[[2]], c("gadget.stock", "list"))
})

test_that("read_gadget_fleet returns the appropriate structure", {
    fleets <- read_gadget_fleet(main = main, path = path)
    expect_equal(length(fleets), 2)
    expect_is(fleets, "list")
    expect_equal(names(fleets), c("fleet", "prey"))
    expect_equal(fleets$fleet$fleet, c("spr", "aut", "comm"))
})

test_that("read_gadget_likelihood returns the appropriate number and type of likelihood types", {
    lik <- read_gadget_likelihood(main = main, path = path)
    expect_equal(names(lik), c("penalty", "understocking", "catchdistribution", "surveyindices"))
    expect_is(lik$catchdistribution, "data.frame")
    expect_is(lik$surveyindices, "data.frame")
    expect_equal(nrow(lik$catchdistribution), 10)
    expect_equal(nrow(lik$surveyindices), 11)
})

test_that("read_gadget_stock_std appropriately reads files structures for StockStdPrinter", {
    cod_std <- read_gadget_stock_std(output_dir = "WGTS/out.fit", file = "cod.std", path = path)
    stock_std_names <-
        c("year", "step", "area", "age", "number", "length",
          "weight", "length.sd", "consumed", "biomass")
    expect_equal(names(cod_std$cod.std), stock_std_names)
    expect_is(cod_std, "list")
    expect_is(cod_std$cod.std, "data.frame")
    expect_equal(nrow(cod_std$cod.std), 1672)
})

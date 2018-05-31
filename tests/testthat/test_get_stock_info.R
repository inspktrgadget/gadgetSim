context("Testing get_stock_info functions")

path <- system.file(gad_mod_dir, package = "gadgetSim")
main <- read_gadget_main(path = path)
stocks <- read_gadget_stockfiles(main = main, path = path)

test_that("get_stock_* functions return the appropriate values", {
    expect_equal(get_stock_anything(stocks, "dl"), 1)
    expect_equal(get_stock_anything(stocks, "stockname"), c("cod0", "cod"))
    expect_equal(get_stock_ages(stocks$cod0), c(0,1))
    expect_equal(get_stock_ages(stocks$cod), 2:12)
    expect_equal(get_stock_areas(stocks), 1)
    expect_equal(get_stocknames(stocks$cod), "cod")
    expect_equal(get_stock_lengths(stocks$cod), 1:150)
})

test_that("get_stock_anything returns the appropriate class", {
    expect_is(get_stock_anything(stocks, "dl"), "numeric")
    expect_is(get_stock_anything(stocks, "naturalmortality"), "numeric")
    expect_is(get_stock_anything(stocks, "normalparamfile"), "character")
    expect_is(get_stock_anything(stocks, "growthparameters"), "character")
})

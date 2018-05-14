context("Testing printfile functions")

path <- system.file(gad_mod_dir, package = "gadgetSim")
main <- read_gadget_main(path = path)
stocks <- read_gadget_stockfiles(main = main, path = path)
stock_nms <- get_stocknames(stocks$cod)

cod_stock_std <- update_printfile(stock_std, list(stockname = stock_nms))
cod_stock_full <- update_printfile(stock_full, list(stocknames = stock_nms))
cod_stock <- update_printfile(stock, list(stocknames = stock_nms))
cod_predator <- update_printfile(predator, list(predatornames = stock_nms))
cod_predator_over <- update_printfile(predator_over, list(predatornames = stock_nms))
cod_prey_over <- update_printfile(prey_over, list(preynames = stock_nms))
cod_stock_prey_full <- update_printfile(stock_prey_full, list(preyname = stock_nms))
cod_stock_prey <- update_printfile(stock_prey, list(preynames = stock_nms))
cod_predator_prey <- update_printfile(predator_prey, list(predatornames = stock_nms))
cod_likelihood <- update_printfile(likelihood, list(likelihood = "catchdistribution"))
cod_likelihoodsummary <- update_printfile(likelihoodsummary, list(printfile = "blah"))

test_that("update_printfile returns the appropriate class", {
    expect_is(cod_stock_std, c("stock_std", "list"))
    expect_is(cod_stock_full, c("stock_full", "list"))
    expect_is(cod_stock, c("stock", "list"))
    expect_is(cod_predator, c("predator", "list"))
    expect_is(cod_predator_over, c("predator_over", "list"))
    expect_is(cod_prey_over, c("prey_over", "list"))
    expect_is(cod_stock_prey_full, c("stock_prey_full", "list"))
    expect_is(cod_stock_prey, c("stock_prey", "list"))
    expect_is(cod_predator_prey, c("predator_prey", "list"))
    expect_is(cod_likelihood, c("likelihood", "list"))
    expect_is(cod_likelihoodsummary, c("likelihoodsummary", "list"))
})

##-------------------------------------------------------------------------------------
# having trouble figuring out a way to effectively test the make_aggfiles.* functions
# and makeAggfileHelper functions


test_that("printfile components are appropriately checked for aggfiles", {
    expect_equal(isNULL_aggfiles(cod_stock_std), logical(0))
    expect_equal(isNULL_aggfiles(cod_stock_full), logical(0))
    expect_equal(isNULL_aggfiles(cod_stock), rep(TRUE, 3))
    expect_equal(isNULL_aggfiles(cod_predator), rep(TRUE, 3))
    expect_equal(isNULL_aggfiles(cod_predator_over), rep(TRUE, 2))
    expect_equal(isNULL_aggfiles(cod_prey_over), rep(TRUE, 2))
    expect_equal(isNULL_aggfiles(cod_stock_full), logical(0))
    expect_equal(isNULL_aggfiles(cod_stock_prey), rep(TRUE, 3))
    expect_equal(isNULL_aggfiles(cod_predator_prey), rep(TRUE, 3))
    expect_equal(isNULL_aggfiles(cod_likelihood), logical(0))
    expect_equal(isNULL_aggfiles(cod_likelihoodsummary), logical(0))

})


test_that("printfile_name_check returns errors when names are missing from a printfile component", {
    expect_error(printfile_name_check(list(stocknames = "cod")))
    expect_error(printfile_name_check(cod_stock_std))
    expect_error(printfile_name_check(update_printfile(stock_std, list(stocknames = "cod"))))
    expect_error(printfile_name_check(update_printfile(stock, list(stockname = "cod"))))
    expect_error(printfile_name_check(stock_std))
})

test_that("update_printfile_dirs returns correct values and stops function evaluation if needed", {
    std <- update_printfile_dirs(cod_stock_std, printfile = "printfile", print_dir = "output")
    std2 <- update_printfile_dirs(cod_stock_std, printfile = "printfile")
    expect_equal(std$printfile, "output/printfile")
    expect_equal(std2$printfile, "printfile")
    pred <- modifyList(cod_predator, list(
                       areaaggfile = "cod.area.agg",
                       predlenaggfile = "cod.len.agg",
                       preylenaggfile = "all.preylen.agg"))
    pred <- update_printfile_dirs(pred, printfile = "printfile", print_dir = "output",
                                  aggfile_dir = "aggfiles")
    expect_equal(pred$areaaggfile, "aggfiles/cod.area.agg")
    expect_equal(pred$predlenaggfile, "aggfiles/cod.len.agg")
    expect_equal(pred$preylenaggfile, "aggfiles/all.preylen.agg")
    expect_equal(pred$printfile, "output/printfile")
    expect_error(update_printfile_dirs(cod_stock_std, print_dir = "output", aggfile_dir = "aggfiles"))
})

format_std <- modifyList(cod_stock_std,
                         list(printatstart = 0, precision = NULL, printfile = "printfile"))
format_comps <-
    lapply(seq_along(format_std), function(x, nms) {
        paste0(c(nms[x], format_std[[x]]), collapse = "\t")
    }, nms = names(format_std))
format_check <- paste(c(comp_lab, unlist(format_comps)), sep = "\n", collapse = "\n")

test_that("format_printfile returns correct values", {
    expect_equal(format_printfile(format_std), format_check)
})

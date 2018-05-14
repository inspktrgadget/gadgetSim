context("Testing selectivity functions")

lengths <- 1:100

test_that("selectivity functions produce errors when they are supposed to", {
    expect_error(constant_selectivity(lengths, alpha = 1.01))
    expect_error(straightline_selectivity(lengths, alpha = 0.1, beta = 0.1))
    expect_error(logistic_selectivity(lengths, alpha = 0.2, beta = 50, max_prop = 1.2))
})

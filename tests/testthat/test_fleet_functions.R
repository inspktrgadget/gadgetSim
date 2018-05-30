context("Testing fleet_functions")

lengths <- 1:100

test_that("selectivity functions produce errors when they are supposed to", {
    expect_error(constant_selectivity(lengths, alpha = 1.01))
    expect_error(straightline_selectivity(lengths, alpha = 0.1, beta = 0.1))
    expect_error(logistic_selectivity(lengths, alpha = 0.2, beta = 50, max_prop = 1.2))
})

test_that("*_suit_formula functions produce the correct output", {
    # constant_suit_formula
    const_suit_test <- "cod\tfunction\tconstant\t1"
    const_suit_test2 <- "cod\tfunction\tconstant\t#cod.comm.alpha"
    expect_equal(constant_suit_formula("comm", "cod", params = 1), const_suit_test)
    expect_equal(constant_suit_formula("comm", "cod"), const_suit_test2)
    # straightline_suit_formula
    straightline_test <- "cod\tfunction\tstraightline\t#cod.comm.alpha\t#cod.comm.beta"
    expect_equal(straightline_suit_formula("comm", "cod"), straightline_test)
    # exponential_suit_formula
    exponential_test <-
        paste(c("cod", "function", "exponential", "#cod.comm.alpha", "#cod.comm.beta",
                "#cod.comm.gamma", "#cod.comm.delta"), collapse = "\t")
    expect_equal(exponential_suit_formula("comm", "cod"), exponential_test)
    # exponentiall50_suit_formula
    expl50_test <-
        paste(c("cod", "function", "newexponentiall50", "#cod.comm.alpha", "#cod.comm.l50"),
              collapse = "\t")
    expect_equal(exponentiall50_suit_formula("comm", "cod"), expl50_test)
    # richards_suit_formula
    richards_test <-
        paste(c("cod", "function", "richards", paste0("#cod.comm.p", 0:4)), collapse = "\t")
    expect_equal(richards_suit_formula("comm", "cod"), richards_test)
    # andersen_suit_formula
    andersen_test <-
        paste(c("cod", "function", "andersen", paste0("#cod.comm.p", 0:4)), collapse = "\t")
    expect_equal(andersen_suit_formula("comm", "cod"), andersen_test)
    # andersenfleet_suit_formula
    andersenfleet_test <-
        paste(c("cod", "function", "andersenfleet", paste0("#cod.comm.p", 0:5)), collapse = "\t")
    expect_equal(andersenfleet_suit_formula("comm", "cod"), andersenfleet_test)
    # gamma_suit_formula
    gamma_test <-
        paste(c("cod", "function", "gamma", paste("#cod.comm", c("alpha", "beta", "gamma"), sep = ".")),
              collapse = "\t")
    expect_equal(gamma_suit_formula("comm", "cod"), gamma_test)
    # testing to make sure multiple stocks and numeric parameters work properly
    multi_stock_test <-
        c("cod0\tfunction\tnewexponentiall50\t#cod0.comm.alpha\t#cod0.comm.l50",
          "cod\tfunction\tnewexponentiall50\t#cod.comm.alpha\t#cod.comm.l50")
    expect_equal(exponentiall50_suit_formula("comm", stock = c("cod0", "cod")), multi_stock_test)
    numeric_params_test <-
        paste(c("cod", "function", "newexponentiall50", 1, "#cod.comm.l50"), collapse = "\t")
    expect_equal(exponentiall50_suit_formula("comm", "cod", params = list(1, "l50")),
                 numeric_params_test)
    double_test <-
        c("cod0\tfunction\tnewexponentiall50\t1\t#cod0.comm.l50",
          "cod\tfunction\tnewexponentiall50\t1\t#cod.comm.l50")
    expect_equal(exponentiall50_suit_formula("comm", stock = c("cod0", "cod"),
                                             params = list(1, "l50")),
                 double_test)
})

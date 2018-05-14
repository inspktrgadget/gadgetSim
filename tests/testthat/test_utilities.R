context("Testing utilities")

test_char_vec <- c("this", "is", "a", "test")
test_str_us <- paste0(test_char_vec, collapse = "_")
test_str_tab <- paste0(test_char_vec, collapse = "\t")
test_str_ws <- c("this   is\ta\t\t\ttest")

test_that("split functions appropriately splits characters", {
    expect_equal(split_(test_str_us, split = "_"), test_char_vec)
    expect_equal(split_(test_str_us, split = "_", ind = 3), test_char_vec[3])
    expect_equal(split_tab(test_str_tab), test_char_vec)
    expect_equal(split_ws(test_str_ws), test_char_vec)
    expect_equal(split_list_(test_str_us, split = "_"), list(test_char_vec))
    expect_equal(split_list_(test_str_us, split = "_", ind = 3), list(test_char_vec[3]))
    expect_equal(split_tab_list(test_str_tab), list(test_char_vec))
    expect_equal(split_ws_list(test_str_ws), list(test_char_vec))
})


test_that("get_index function works appropriately", {
    expect_equal(get_index(2, 2:10), 1)
    expect_equal(get_index("q", letters), 17)
})

test_that("check_path function does not add path when not present", {
    expect_equal(check_path(NULL), NULL)
    expect_equal(check_path("test"), "test")
})

test_that("check_path function adds path when it is present", {
    path <- "gadget_model"
    expect_equal(check_path("test"), "gadget_model/test")
    expect_equal(check_path(NULL), NULL)
})

test_that("gf2list appropriately converts various gadget files to lists", {
    path <- system.file(gad_mod_dir, package = "gadgetSim")
    gf <- readLines(paste(path, "likelihood", sep = "/"))
    expect_is(gf2list(gf), "list")
})

test_that("comments are stripped appropriately from character vectors", {
    path <- system.file(gad_mod_dir, package = "gadgetSim")
    char_vec <- readLines(paste(path, "main", sep = "/"))
    a <- char_vec[-1]
    b <- a
    b[3] <- "printfiles"
    expect_equal(strip_comments(char_vec), b)
    expect_equal(strip_comments(char_vec, keep_with = "printfiles"), a)
})

test_that("printfile types are correctly formatted", {
    expect_equal(get_pf_type(stock_std), "StockStd")
    expect_equal(get_pf_type(stock), "Stock")
    expect_equal(get_pf_type(predator_over), "PredatorOver")
    expect_equal(get_pf_type(likelihoodsummary), "Likelihoodsummary")
})

test_that("the appropriate structure or logical is returned from is_list_dots", {
    expect_false(is_list_dots(a = 1, b = 2))
    expect_true(is_list_dots(list(a = 1, b = 2)))
    expect_is(dots2list(a = 1, b = 2), "list")
    expect_is(dots2list(list(a = 1, b = 2)), "list")
})

test_that("is_list_element_null returns the appropriate logical for each NULL component", {
    test_list <- list(a = 1, b = 2, c = NULL, d = NULL)
    expect_equal(is_list_element_null(test_list), c(FALSE, FALSE, TRUE, TRUE))
    expect_equal(names(is_list_element_null(test_list, keep_names = TRUE)), c("a", "b", "c", "d"))
})

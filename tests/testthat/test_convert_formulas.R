context("Testing formula conversion functions")

test_that("to_gadget_formula correctly converts formulas to Gadget readable", {
    expect_equal(to_gadget_formula(quote(2+2)), "(+ 2 2)")
    expect_equal(to_gadget_formula(quote(2 + log(moo - 1))), "(+ 2 (log (- #moo 1)))")
})


context("downfactor")


test_that("base case works", {

  x = letters[1:5]

  expect_equal(downfactor(x, n = 2),
               factor(c("a", "b", "Other", "Other", "Other"),
                      c("a", "b", "Other")))

})

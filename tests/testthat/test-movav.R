context("movav")


test_that("base case works", {

  x = c(1, 2, 1, 2)

  expect_equal(movav(x, 2),
               c(1, 1.5, 1.5, 1.5))

  expect_equal(movav(x, 10),
               c(1, 1.5, 4/3, 1.5))


})

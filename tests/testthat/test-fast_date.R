context("fast_date")

test_that("base case works", {

  x = "2011-01-01"
  y = as.Date(x)
  expect_equal(fast_date(x), as.Date(x))
  expect_equal(fast_date(y), as.Date(y))
  expect_equal(fast_date(character(0)), as.Date(character(0)))
  expect_error(fast_date(1))

})

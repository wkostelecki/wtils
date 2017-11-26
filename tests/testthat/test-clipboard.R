
context("clipboard")

test_that("clipboard works", {
  expect_true(all(clipboard(clipboard(mtcars)) == mtcars))
})

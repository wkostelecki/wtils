context("safe_left_join")


test_that("base cases work", {

  y = data.frame(cyl = c(4, 6, 8), z = c(1, 2, 3))

  expect_equal(safe_left_join(mtcars, y, "cyl"),
               dplyr::left_join(mtcars, y, "cyl"))
  expect_error(safe_left_join(mtcars, y[c(1, 2), ], "cyl", verbose = FALSE))
  expect_error(safe_left_join(mtcars, y[c(1, 2, 2, 3), ], "cyl"))

})

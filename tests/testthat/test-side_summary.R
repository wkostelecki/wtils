

context("side_summary")

test_that("side_summary works", {
  Z = c("a", NA)
  summary = side_summary(data.frame(W = c(TRUE, FALSE),
                                    X = c(1L, NA),
                                    Y = c(1.5, NA),
                                    Z = Z))

  expect_equal(as.character(summary[["Column"]]), c("W", "X", "Y", "Z"))
  expect_equal(as.character(summary[["Class"]]),
               c("logical", "integer", "numeric", "factor"))
  expect_equal(summary[["Min"]], c(0, 1, 1.5, NA))
  expect_equal(summary[["Med"]], c(0.5, 1, 1.5, NA))
  expect_equal(summary[["Mean"]], c(0.5, 1, 1.5, NA))
  expect_equal(summary[["Max"]], c(1, 1, 1.5, NA))
  expect_equal(summary[["NAs"]], c(0, 1, 1, 1))

  summary = side_summary(data.frame(X = c(1L, NA),
                                    Y = c(1.5, NA),
                                    Z = Z),
                         extra = "all")

  expect_equal(summary[["Unique"]], c(2, 2, 2))
  expect_true(exists("Sample", summary))


})

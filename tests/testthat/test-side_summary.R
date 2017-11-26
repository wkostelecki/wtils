

context("side_summary")

test_that("side_summary works", {
  Z = c("a", NA)
  summary = side_summary(data.frame(X = c(1L, NA),
                                    Y = c(1.5, NA),
                                    Z = Z))

  expect_equal(as.character(summary$Column), c("X", "Y", "Z"))
  expect_equal(summary$Min, c("1", "1.5", NA))
  expect_equal(summary$Med, c("1", "1.5", NA))
  expect_equal(summary$Mean, c("1", "1.5", NA))
  expect_equal(summary$Max, c("1", "1.5", NA))
  expect_equal(summary$NAs, c("1", "1", "1"))

})

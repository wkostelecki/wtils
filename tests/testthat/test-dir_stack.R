context("dir_stack")


test_that("csv directory loads", {
  path = file.path(system.file(package = "wtils"), "extdata/dir_stack")
  df = dir_stack(path)
  expect_equal(names(df), c("a", "b", "c", "source_file"))
  expect_equal(df$a, c(1, 2, NA_real_, NA_real_))
})


test_that("failures work", {
  path = file.path(system.file(package = "wtils"), "extdata")
  expect_error(dir_stack(path))

})

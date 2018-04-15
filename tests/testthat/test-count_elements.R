
context("count_elements")

test_that("count elements", {

  text = c("a,b,c", 'a,",,b",c')
  expect_equal(count_elements(text), c(3, 3))
  expect_equal(count_elements(text, quote = ''), c(3, 5))

  expect_equal(count_elements('a,b,"c'), 3)
  expect_equal(count_elements('a,b,"c', quote = ''), 3)
  expect_equal(count_elements('a,"b,c"'), 2)
  expect_equal(count_elements('a,b,","'), 3)
  expect_equal(count_elements('a,b,","', quote = ''), 4)
  expect_equal(count_elements('a,","",b",c'), 3)

  text = c("a, b,\"c", 'a,b,""', 'a,b,"', 'a,b,""","', '""a,""b, ""c')

})

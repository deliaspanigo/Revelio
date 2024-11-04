
library(testthat)
test_that('miFuncion multiplica correctamente', {
  expect_equal(miFuncion(2), 4)
  expect_equal(miFuncion(3), 6)
})

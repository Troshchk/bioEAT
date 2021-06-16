library(bioEAT)

test_that("outersect is correct", {

  expect_equal(outersect(1:5,4:7), c(1,2,3,6,7))

})


test_that("emplty inputs throws error", {

  expect_error(outersect(1:5,), regexp = "argument \"y\" is missing, with no default")
  expect_error(outersect(,1:5), regexp = "argument \"x\" is missing, with no default")

})

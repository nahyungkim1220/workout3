library(testthat)

context("Testing Checkers")

test_that("check_prob works as expected", {
  x <- 1
expect_true(check_prob(0.5))
expect_equal(length(check_prob(0.5)),length(x))
expect_error(check_prob(2), 'invalid prob value')
})

test_that("check_trials works as expected", {
  expect_true(check_trials(5))
  expect_error(check_trials(-1),"invalid trials value")
  expect_error(check_trials(5.5), "invalid trials value")
})

test_that("check_success works as expected", {
  expect_true(check_success(3,5))
  expect_error(check_success(5, 3), "success cannot be greater than trials")
  expect_error(check_success(-1,5), "success should be made up of nonnegative values")
})

library(testthat)

context("Testing Summary Measures")

test_that("aux_mean works as expected", {
  expect_equal(aux_mean(1,0.5) ,0.5)
  expect_equal(aux_mean(10,0.3),3)
  expect_equal(aux_mean(2,0.9),1.8)
})

test_that("aux_variance works as expected", {
  expect_equal(aux_variance(6, 0.7), 1.26)
  expect_equal(aux_variance(5,0.3), 1.05)
  expect_equal(aux_variance(20,0.1), 1.8)
})

test_that("aux_mode works as expected", {
  expect_equal(aux_mode(5,0.3), 1)
  expect_equal(aux_mode(20,0.1), 2)
  expect_equal(aux_mode(10,0.3), 3)
})

test_that("aux_skewness works as expected", {
  expect_equal(aux_skewness(5,0.8), -0.6708204)
  expect_equal(aux_skewness(4, 0.2), 0.75)
  expect_equal(aux_skewness(20,0.1), 0.5962848)
})

test_that("aux_kurtosis works as expected", {
  expect_equal(aux_kurtosis(5,0.8), 0.05)
  expect_equal(aux_kurtosis(4, 0.2), 0.0625)
  expect_equal(aux_kurtosis(2,0.5), -1)
 })


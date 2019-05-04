library(testthat)

context("Testing Binomals")

test_that("bin_choose works as expected", {
  expect_equal(bin_choose(10,3), 120)
  expect_equal(bin_choose(5,3), 10)
  expect_error(bin_choose(2,4), "k cannot be greater than n")
})

test_that("bin_probability works as expected", {
  expect_equal(bin_probability(success = 3, trials = 5, prob = 0.5), 0.3125)
  expect_equal(bin_probability(success = 1, trials = 5, prob = 0.1), 0.32805)
  expect_error(bin_probability(success = 5, trials = 3, prob = 0.5), "success cannot be greater than trials")
})

test_that("bin_distribution works as expected", {
trials <- 5
prob <-  0.5
test_dat <- data.frame("success" = 0:trials, "probability" = 0:trials)
for (i in 1:nrow(test_dat))
  test_dat[i,2] <- bin_probability(success = i -1, trials = trials, prob = prob)
    class(test_dat) <- c("bindis", "data.frame")

  expect_identical(bin_distribution(trials = trials, prob = prob), test_dat)
  expect_is(bin_distribution(trials = trials, prob = prob), c("bindis", "data.frame"))
  expect_identical(dim(bin_distribution(trials = trials, prob = prob)), dim(test_dat))
})

test_that("bin_cumulative works as expected", {
  trials <- 5
  prob <-  0.5
  test_dat2 <- bin_distribution(trials = trials, prob = prob)
  test_dat2$cumulative <- 0: trials
  test_dat2[1,3] <- test_dat2[1,2]
  for (i in 2:nrow(test_dat2))
    test_dat2[i,3] <- test_dat2[i,2] + test_dat2[i-1,3]
  class(test_dat2) <- c("bincum", "data.frame")

  expect_identical(bin_cumulative(trials = trials, prob = prob), test_dat2)
  expect_is(bin_cumulative(trials = trials, prob = prob), c("bincum", "data.frame"))
  expect_identical(dim(bin_cumulative(trials = trials, prob = prob)), dim(test_dat2))
})

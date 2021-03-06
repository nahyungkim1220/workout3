## ---- echo = FALSE, message = FALSE--------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>")
library(binomial)
library(ggplot2)

## ------------------------------------------------------------------------
bin1 <- bin_variable(trials = 10, p = 0.3)
bin1

## ------------------------------------------------------------------------
binsum1 <- summary(bin1)
binsum1

## ------------------------------------------------------------------------
bin_mean(10, 0.3)

## ------------------------------------------------------------------------
bin_variance(10, 0.3)

## ------------------------------------------------------------------------
bin_mode(10, 0.3)

## ------------------------------------------------------------------------
bin_skewness(10, 0.3)

## ------------------------------------------------------------------------
bin_kurtosis(10, 0.3)

## ------------------------------------------------------------------------
bin_probability(success = 2, trials = 5, prob = 0.5)

## ------------------------------------------------------------------------
dis <- bin_distribution(trials = 5, prob = 0.5)

## ------------------------------------------------------------------------
plot(dis)

## ------------------------------------------------------------------------
dis2 <- bin_cumulative(trials = 5, prob = 0.5)

## ------------------------------------------------------------------------
plot(dis2)

## ------------------------------------------------------------------------
bin_choose(n = 5, k = 2)
bin_choose(5, 0)
bin_choose(5, 1:3)


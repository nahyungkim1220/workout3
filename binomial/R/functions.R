# test if inputted 'prob' is valid (i.e. 0 ≤ p ≤ 1).
check_prob <- function(prob) {
  if (prob < 0 | prob > 1) {
    stop("invalid prob value")
  }
  TRUE
}

# test if inputted trials is valid (i.e. n is a non-negative integer).
check_trials <- function(trials) {
  if (trials %% 1 != 0 | trials < 0) {
    stop('invalid trials value')
  }
  TRUE
}

# test if inputted success is valid (i.e. 0 ≤ k ≤ n)
check_success <- function(success,trials) {
  if (any(success > trials)) {
    stop('success cannot be greater than trials')
  }
  if (any(success < 0)) {
    stop('success should be made up of nonnegative values')
  }
  if (any(success %% 1 != 0)) {
    stop('success should be made up of integers')
  }
    TRUE
}

# returns mean
aux_mean <- function(trials, prob) {
  return(trials*prob)
}

# returns variance
aux_variance <- function(trials, prob) {
  x = trials*prob
  f = 1-prob
  v = x*f
  return(v)
}

aux_variance(20,0.1)

# returns mode
aux_mode <- function(trials, prob) {
  n = trials
  p = prob
  x = n*p
  if((n * p + p) %% 1 != 0) {
    (n * p+ p) - ((n * p + p) %% 1)
  } else if ((n * p + p) %% 1 == 0) {
    c((n * p + p), (n * p + p - 1))
  }
}

# returns skewedness - measure of asymmetry in the distribution
aux_skewness <- function(trials, prob) {
  n = trials
  p = prob
  x = n*p
  f = 1-prob
  s = (1-2*p) / sqrt(x*f)
  return(s)
}

# returns kurtosis - measure of "tailedness" in the distribution
aux_kurtosis <- function(trials,prob) {
  n = trials
  p = prob
  x = n*p
  f = 1-prob
  k = (1-6*p*f) / (x*f)
  return(k)
}

#' @title bin_choose
#' @description calculates the number of combinations in which k successes can occur in n trials.
#' @param n number of trials
#' @param k number of successes (must not be greater than n)
#' @return computed number of combinations of success
#' @export
#' @examples
#' bin_choose(5,0)
bin_choose <- function(n, k) {
  f = factorial(n)/(factorial(k)*factorial(n-k))
  ifelse (k > n, stop("k cannot be greater than n"), f)
}

#' @title bin_probability
#' @description calculates the probability of getting x number of successes in y trials.
#' @param success number of success
#' @param trials number of trials
#' @param prob probability
#' @return computed probability
#' @export
#' @examples
#' bin_probability(success = 2, trials = 5, prob = 0.5)
bin_probability <- function(success, trials, prob) {
  check_trials(trials)
  check_prob(prob)
  check_success(success,trials)
  n = trials
  p = prob
  k = success
  f = 1-prob
  res <- (bin_choose(n,k)*p^k)*f^(n-k)
  return(res)
}

#' @title bin_distribution
#' @description calculates the probability of getting x number of successes in y trials.
#' @param trials number of trials
#' @param prob probability
#' @return computed probability
#' @export
#' @examples
#' bin_distribution(trials = 5, prob = 0.5)
bin_distribution <- function(trials, prob) {
  success = 0:trials
  bindis <- data.frame("success" = 0:trials, "probability" = 0:trials)
  for (i in 1:length(success)) {
    bindis$success[i] <- success[i]
    bindis$probability[i] <- bin_probability(success[i], trials, prob)
  }
 class(bindis) <- c("bindis", "data.frame")
 bindis
}

#' @export
plot.bindis <- function(x, ...) {
ggplot(x, aes(x = success, y = probability)) +
    geom_histogram(stat = "identity") +
    theme(panel.background = element_blank())
}

#' @title bin_cumulative
#' @description return a data frame with the probability distribution and cumulative probabilities.
#' @param trials number of trials
#' @param prob probability
#' @return data frame
#' @export
#' @examples
#' bin_cumulative(trials = 5, prob = 0.5)
bin_cumulative <- function(trials, prob) {
  bincum <- bin_distribution(trials, prob)
  bincum$cumulative <- cumsum(bincum$probability)
  class(bincum) <- c("bincum", "data.frame")
  bincum
}

#' @export
plot.bincum <- function(x, ...) {
  plot(x$success, x$cumulative, type = "o", xlab = "successes", ylab = "probability")
}

#' @title bin_variable
#' @description returns the binomial random variable object
#' @param trials number of trials
#' @param prob probability
#' @return list
#' @export
#' @examples
#' bin_variable(trials = 10, prob = 0.3)
bin_variable <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  binvar <- list(
    trials = trials,
    prob = prob
  )
  class(binvar) <- "binvar"
  binvar
}

#' @export
print.binvar <- function(x, ...) {
  cat('"Binomial variable" \n\n')
  cat('Parameters \n')
  cat("- number of trials:", x$trials, "\n")
  cat("- prob of success:", x$prob, "\n")
  invisible(x)
}

#' @export
summary.binvar <- function(x, ...) {
  obj <- list(trials = x$trials,
                prob = x$prob,
                mean = aux_mean(x$trials, x$prob),
                mode = aux_mode(x$trials, x$prob),
                variance = aux_variance(x$trials, x$prob),
                skewness = aux_skewness(x$trials, x$prob),
                kurtosis = aux_kurtosis(x$trials, x$prob))
  class(obj) <- "summary.binvar"
  obj
}

#' @export
print.summary.binvar <- function(x, ...) {
cat('"Summary Binomial" \n\n')
cat('Parameters \n')
cat("- number of trials:", x$trials, "\n")
cat("- prob of success:", x$prob, "\n\n")
cat('Measures \n')
cat("- mean:", x$mean, "\n")
cat("- variance:", x$variance, "\n")
cat("- mode:", x$mode, "\n")
cat("- skewness:", x$skewness, "\n")
cat("- kurtosis", x$kurtosis, "\n")
invisible(x)
}


#' @title bin_mean
#' @description calculates mean
#' @param trials number of trials
#' @param prob probability
#' @return mean
#' @export
#' @examples
#' bin_mean(10, 0.3)
bin_mean <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  aux_mean(trials, prob)
}

#' @title bin_variance
#' @description calculates variance
#' @param trials number of trials
#' @param prob probability
#' @return variance
#' @export
#' @examples
#' bin_variance(10, 0.3)
bin_variance <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  aux_variance(trials, prob)
}

#' @title bin_mode
#' @description calculates mode
#' @param trials number of trials
#' @param prob probability
#' @return mode
#' @export
#' @examples
#' bin_mode(10, 0.3)
bin_mode <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  aux_mode(trials, prob)
}

#' @title bin_skewness
#' @description calculates skewness of probability distribution
#' @param trials number of trials
#' @param prob probability
#' @return skewness value
#' @export
#' @examples
#' bin_skewness(10, 0.3)
bin_skewness <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  aux_skewness(trials, prob)
}

#' @title bin_kurtosis
#' @description calculates kurtosis
#' @param trials number of trials
#' @param prob probability
#' @return kurtosis value
#' @export
#' @examples
#' bin_kurtosis(10, 0.3)
bin_kurtosis <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  aux_kurtosis(trials, prob)
}

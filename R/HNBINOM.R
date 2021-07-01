#' @name HNBINOM
#' @aliases dhnbinom
#' @aliases phnbinom
#' @aliases rhnbinom
#' @title Hurdle Model for Negative Binomial Distribution
#' @description Density, distribution function, quantile function and random generation for the hurdle model
#' using negative binomial distribution with parameters \code{theta}, \code{size}, and \code{prob}.
#' @param x,q vector of quantiles
#' @param p vector of probabilities
#' @param n number of observations
#' @param theta zero generating parameter (probability of zeros)
#' @param size number of successes (dispersion parameter)
#' @param prob probability of success in each trial. 0 < prob < 1
#' @param log,log.p logical; if TRUE, probabilities \code{p} are given as \code{log(p)}.
#' @param lower.tail logical; if TRUE (default), probabilities are \code{P[X \%leq x]}, otherwise,
#' \code{P[X > x]}.
#' @return \code{dhnbinom} gives the density, \code{phnbinom} gives the distribution function,
#' \code{qhnbinom} gives the quantile function, and \code{rhnbinom} generates random deviates.
#' @import stats

#' @rdname HNBINOM
#' @export

dhnbinom <-
function(x, theta = 0.5, size = 1, prob = 0.5, log = FALSE) {
  tt <- (1 - theta) * stats::dnbinom(x, size = size, prob = prob) / (1 - pnbinom(0, size = size, prob = prob))
  tt[x == 0L] <- rep(theta, length(tt))[x == 0L]
  if (log) {
    return(tt)
  } else {
    tt
  }
}

#' @rdname HNBINOM
#' @export

phnbinom <-
function(q, theta = 0.5, size = 1, prob = 0.5, lower.tail = TRUE, log.p = FALSE) {
  tt <- pmax(0, pnbinom(q, size = size, prob = prob) - stats::dnbinom(0L, size = size, prob = prob)) /
    pnbinom(0, size = size, prob = prob, lower.tail = FALSE) * (1 - theta)
  zindex <- tt == 0L
  tt[zindex] <- 0
  tt <- tt + theta
  tt[q < 0] <- 0
  tt
}

#' @rdname HNBINOM
#' @export

rhnbinom <- function(n, theta = 0.5, size = 1, prob = 0.5) {
  zero   <- rbinom(n, 1, theta)
  sum_zero <- sum(zero)
  sum_non_zero <- n-sum_zero
  z_trun <- sample(1:999999, sum_non_zero, replace = TRUE, prob = stats::dnbinom(1:999999, size = size, prob = prob))
  output <- c(rep(0,sum_zero),z_trun)
  return(output)
}

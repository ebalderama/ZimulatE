#' @name HNBINOM
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
#' @param lower.tail logical; if TRUE (default), probabilities are \code{P[X \leq x]}, otherwise,
#' \code{P[X > x]}.
#' @return \code{dhnbinom} gives the density, \code{phnbinom} gives the distribution function,
#' \code{qhnbinom} gives the quantile function, and \code{rhnbinom} generates random deviates.
#' @export dhnbinom phnbinom qhnbinom rhnbinom
#' @import hurdlr BMS

dhnbinom <-
function(x, theta = 0.5, size = 1, prob = 0.5, log = FALSE) {
  zindex <- x == 0
  x[zindex] <- theta
  x[!zindex] <- (1 - theta) * dnbinom(x[!zindex], size = size, prob = prob) / (1 - dnbinom(0, size = size, prob = prob))
  
  if (log) {
    return(x)
  } else {
    x
  }
}

# Issue: possible that phnbinom returns a negative probability ----------
# phnbinom <-
# function(q, theta = 0.5, size = 1, prob = 1, lower.tail = TRUE, log.p = FALSE) {
#   #hypergeometric2F1(12,1,5,0.5, log = FALSE)
#   numer <- (-1 + theta) * ( 1 - prob**size - (1 - prob)**(1+q) * prob**size * choose(size + q, -1 + size) * f21hyper(1, 1 + size + q, 2 + q, 1 - prob))
#   denom <- -1 + prob**size
#   numer / denom
# }



# qhnbinom <-
# function(p, theta = 0.5, size = 1, prob = 1, lower.tail = TRUE, log.p = FALSE) {
#   return(0)
# }

rhnbinom <- function(n, theta = 0.5, size = 1,prob = 1) {
  zero   <- rbinom(n,1,theta)
  sum_zero <- sum(zero)
  sum_non_zero <- n-sum_zero
  z_trun<-sample(1:999999,sum_non_zero,replace = T, prob = dnbinom(1:999999,size = size, prob = prob))
  output <- c(rep(0,sum_zero),z_trun)
  return(output)
}

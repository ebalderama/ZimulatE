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
  tt <- rep(0, length(x))
  zindex <- x == 0
  tt[zindex] <- theta
  tt[!zindex] <- (1 - theta) * dnbinom(tt[!zindex], size = size, prob = prob) / (1 - dnbinom(0, size = size, prob = prob))
  
  if (log) {
    return(tt)
  } else {
    tt
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

phnbinom <-
function(q, theta = 0.5, size = 1, mu = 1, lower.tail = TRUE, log.p = FALSE) {
  tt <- pmax(0, pnbinom(q, size = size, mu = mu) - dnbinom(0L, size = size, mu = mu)) /
    pnbinom(0, size = size, mu = mu, lower.tail = FALSE) *
    theta
  zindex <- tt == 0L
  tt[zindex] <- 0
  tt <- tt + (1 - theta)
  tt[q < 0] <- 0
  tt
}


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

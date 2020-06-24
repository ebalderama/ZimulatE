#' @name HNBINOM
#' @title Hurdle Model for Negative Binomial Distribution
#' @description Density, distribution function, quantile function and random generation for the hurdle model
#' using negative binomial distribution with parameters \code{theta}, \code{size}, and \code{mu}.
#' @param x,q vector of quantiles
#' @param p vector of probabilities
#' @param n number of observations
#' @param theta zero generating parameter (probability of zeros)
#' @param size number of successes (dispersion parameter)
#' @param mu mean parameter (alternative parameterization)
#' @param log,log.p logical; if TRUE, probabilities \code{p} are given as \code{log(p)}.
#' @param lower.tail logical; if TRUE (default), probabilities are \code{P[X \leq x]}, otherwise,
#' \code{P[X > x]}.
#' @return \code{dhnbinom} gives the density, \code{phnbinom} gives the distribution function,
#' \code{qhnbinom} gives the quantile function, and \code{rhnbinom} generates random deviates.
#' @export dhnbinom phnbinom qhnbinom rhnbinom
#' @import hurdlr

dhnbinom <- function(x, theta = 0.5, size = 1, mu = 1, log = FALSE) {
  return(0)
}

phnbinom <- function(q, theta = 0.5, size = 1, mu = 1, lower.tail = TRUE, log.p = FALSE) {
  return(0)
}

qhnbinom <- function(p, theta = 0.5, size = 1, mu = 1, lower.tail = TRUE, log.p = FALSE) {
  return(0)
}

rhnbinom <- function(n, theta = 0.5, size = 1,mu = 1){
  
  zero   <- rbinom(n,1,theta)
  sum_zero <- sum(zero)
  sum_non_zero <- n-sum_zero
  z_trun<-sample(1:999999,sum_non_zero,replace = T, prob = dnbinom(1:999999,size = size, mu = mu))
  output <- c(rep(0,sum_zero),z_trun)
  return(output)
}
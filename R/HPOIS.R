#' @name HPOIS
#' @title Hurdle Model for Poisson Distribution
#' @description Density, distribution function, quantile function and random generation for the hurdle model
#' using Poisson distribution with parameters \code{theta} and \code{lambda}.
#' @param x,q vector of quantiles
#' @param p vector of probabilities
#' @param n number of observations
#' @param theta zero-generating parameter (probability of zeros)
#' @param lamda expected Poisson count
#' @param log,log.p logical; if TRUE, probabilities \code{p} are given as \code{log(p)}.
#' @param lower.tail logical; if TRUE (default), probabilities are \code{P[X \leq x]}, otherwise,
#' \code{P[X > x]}.
#' @return \code{dhpois} gives the density, \code{phpois} gives the distribution function,
#' \code{qhpois} gives the quantile function, and \code{rhpois} generates random deviates.
#' @export dhpois phpois qhpois rhpois
#' @import hurdlr pracma

dhpois <-
function(x, theta = 0.5, lambda = 1, log = FALSE) {
  zindex <- x == 0
  x[zindex] <- theta
  x[!zindex] <- (1 - theta) * dpois(x[!zindex], lambda) / (1 - dpois(0, lambda))
    
  if (log) {
    return(x)
  } else {
    x
  }
}

incgam2 <-
function(x, a) {
  # reverses the order of parameters to match with Mathematica
  incgam(a, x)
}

cdf_ztpois <-
function(x, theta, lambda) {
  # use sapply, since incgam2 is not vectorized
  numer <- (-1 + theta) * (gamma(1 + x) - exp(lambda) * sapply(1+x, incgam2, a = lambda))
  denom <- (-1 + exp(lambda) ) * gamma(1 + x)
  numer / denom
}

phpois <-
function(q, theta = 0.5, lambda = 1, lower.tail = TRUE, log.p = FALSE) {
  zindex <- q == 0
  q[zindex] <- 0
  q[!zindex] <- cdf_ztpois(q[!zindex], theta, lambda)
  q
}

#-------------- Maybe there's no closed-form expression? -------------
#qhpois <-
#function(p, theta = 0.5, lambda = 1, lower.tail = TRUE, log.p = FALSE) {
#  return(0)
#}

rhpois <-
function(n, theta=0.5, lambda=1){
  
  zero <- rbinom(n, 1, theta)
  sum_zero <- sum(zero == 1)
  sum_non_zero <- n - sum_zero
  
  z_trun <- sample(1:999999, sum_non_zero, replace = TRUE, prob = dpois(1:999999,lambda))
  
  output <- c(rep(0,sum_zero),z_trun)
  return(output)
}
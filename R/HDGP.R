#' @name HDGP
#' @title Hurdle Model for Generalized Pareto Distribution
#' @description Density, distribution function, quantile function and random generation for the hurdle model of the
#' generalized Pareto distribution with parameters \code{theta}, \code{mu}, \code{scale}, and \code{shape}.
#' @param x,q vector of quantiles
#' @param p vector of probabilities
#' @param n number of observations
#' @param theta zero generating parameter (probability of zeros)
#' @param mu location parameter
#' @param scale (non-negative) scale parameter
#' @param shape shape parameter
#' @param log,log.p logical; if TRUE, probabilities \code{p} are given as \code{log(p)}.
#' @param lower.tail logical; if TRUE (default), probabilities are \code{P[X \leq x]}, otherwise,
#' \code{P[X > x]}.
#' @return \code{dhdgp} gives the density, \code{phdgp} gives the distribution function,
#' \code{qhdgp} gives the quantile function, and \code{rhdgp} generates random deviates.
#' @export dhdgp phdgp qhdgp rhdgp
#' @import hurdlr

dhdgp <-
function(x, theta = 0.5, mu=0, scale=1, shape=1, log = FALSE) {
  zindex <- x == 0
  x[zindex] <- theta
  x[!zindex] <- (1 - theta) * ddgp(x[!zindex], mu, scale, shape)
  x
}

phdgp <-
function(q, theta = 0.5, mu=0, scale=1, shape=1, lower.tail = TRUE, log.p = FALSE) {
  zindex <- q == 0
  q[zindex] <- 0
  q[!zindex] <- (1 - theta) * pdgp(x[!zindex], mu, scale, shape)
  q
}

# -------- May not exist ------------- 

#qhdgp <-
#function(p, theta = 0.5, mu=0, scale=1, shape=1, lower.tail = TRUE, log.p = FALSE) {
#  return(0)
#}

rhdgp <- function(n, theta = 0.5, mu = 0, scale = 1, shape = 1){
  
  zero <- rbinom(n, 1, theta)
  sum_zero <- sum(zero == 1)
  sum_non_zero <- n - sum_zero
  z_trun <- rdgp(sum_non_zero, mu = 0.5, scale, shape)
  
  output <- c(rep(0, sum_zero), z_trun)
  output
}
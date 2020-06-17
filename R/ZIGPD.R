#' @name ZIGPD
#' @title Zero-inflated Generalized Pareto Distribution
#' @description Density, distribution function, quantile function and random generation for the zero-inflated
#' generalized Pareto distribution with parameters \code{theta}, \code{mu}, \code{sigma}, and \code{xi}.
#' @param x,q vector of quantiles
#' @param p vector of probabilities
#' @param n number of observations
#' @param theta zero-inflation parameter (probability of zeros)
#' @param mu location parameter
#' @param sigma (non-negative) scale parameter
#' @param xi shape parameter
#' @param log,log.p logical; if TRUE, probabilities \code{p} are given as \code{log(p)}.
#' @param lower.tail logical; if TRUE (default), probabilities are \code{P[X ??? x]}, otherwise,
#' \code{P[X > x]}.
#' @return \code{dzigpd} gives the density, \code{pzigpd} gives the distribution function,
#' \code{qzigpd} gives the quantile function, and \code{rzigpd} generates random deviates.
#' @export dzigpd pzigpd qzigpd rzigpd
#' @import stats
#' @import hurdlr

dzigpd <-
function(x, theta, mu = 1, sigma = 1, xi = 1, log = FALSE) {
  return(0)
}

pzigpd <-
function(q, theta, mu = 1, sigma = 1, xi = 1, lower.tail = TRUE, log.p = FALSE) {
  return(0)
}

qzigpd <-
function(p, theta, mu = 1, sigma = 1, xi = 1, lower.tail = TRUE, log.p = FALSE) {
  return(0)
}

rzigpd <-
function(n, theta, mu = 1, sigma = 1, xi = 1) {
  zero   <- rbinom(n,1,theta)
  y      <- rgpd(n,mu,sigma,xi)
  output  <- ifelse(zero==1,0,y)
  return(output)
}
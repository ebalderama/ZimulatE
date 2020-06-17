#' @name ZIP
#' @title Zero-inflated Poisson Distribution
#' @description Density, distribution function, quantile function and random generation for the zero-inflated Poisson
#' distribution with parameters \code{theta} and \code{lambda}.
#' @param x,q vector of quantiles
#' @param p vector of probabilities
#' @param n number of observations
#' @param theta zero-inflation parameter (probability of zeros)
#' @param lambda expected Poisson count
#' @param log,log.p logical; if TRUE, probabilities \code{p} are given as \code{log(p)}.
#' @param lower.tail logical; if TRUE (default), probabilities are \code{P[X ??? x]}, otherwise,
#' \code{P[X > x]}.
#' @return \code{dzip} gives the density, \code{pzip} gives the distribution function,
#' \code{qzip} gives the quantile function, and \code{rzip} generates random deviates.
#' @export dzip pzip qzip rzip
#' @import stats

dzip <-
function(x, theta = 0.5, lambda = 1, log = FALSE) {
  return(0)
}

pzip <-
function(q, theta = 0.5, lambda = 1, lower.tail = TRUE, log.p = FALSE) {
  return(0)
}

qzip <-
function(p, theta = 0.5, lambda = 1, lower.tail = TRUE, log.p = FALSE) {
    return(0)
}

rzip <-
function(n, theta = 0.5, lambda = 1){
  zero   <- rbinom(n, 1, theta)
  y      <- rpois(n, lambda)
  output <- ifelse(zero == 1,0 ,y)
  return(output)
}
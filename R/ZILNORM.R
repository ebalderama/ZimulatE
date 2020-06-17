#' @name ZILNORM
#' @title Zero-inflated Log-normal Distribution
#' @description Density, distribution function, quantile function and random generation for the zero-inflated
#' log-normal distribution with parameters \code{theta}, \code{meanlog}, and \code{sdlog}.
#' @param x,q vector of quantiles
#' @param p vector of probabilities
#' @param n number of observations
#' @param theta zero-inflation parameter (probability of zeros)
#' @param meanlog mean (log scale)
#' @param sdlog standard deviation (log scale)
#' @param log,log.p logical; if TRUE, probabilities \code{p} are given as \code{log(p)}.
#' @param lower.tail logical; if TRUE (default), probabilities are \code{P[X ??? x]}, otherwise,
#' \code{P[X > x]}.
#' @return \code{dzilnorm} gives the density, \code{pzilnorm} gives the distribution function,
#' \code{qzilnorm} gives the quantile function, and \code{rzilnorm} generates random deviates.
#' @export dzilnorm pzilnorm qzilnorm rzilnorm
#' @import stats

dzilnorm <-
function(x, theta, mu = 1, sigma = 1, xi = 1) {
  return(0)
}

pzilnorm <-
function(q, theta, mu = 1, sigma = 1, xi = 1, lower.tail = TRUE, log.p = FALSE) {
  return(0)
}

qzilnorm <-
function(p, theta, mu = 1, sigma = 1, xi = 1, lower.tail = TRUE, log.p = FALSE) {
  return(0)
}

rzilnorm <-
function(n, theta, meanlog = 0, sdlog = 1){
  zero   <- rbinom(n, 1, theta)
  y      <- rlnorm(n, meanlog, sdlog)
  output  <- ifelse(zero == 1, 0, y)
  return(output)
}
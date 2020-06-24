#' @name ZINB
#' @title Zero-inflated Negative Binomial Distribution
#' @description Density, distribution function, quantile function and random generation for the zero-inflated
#' negative binomial distribution with parameters \code{theta}, \code{size}, and \code{mu}.
#' @param x,q vector of quantiles
#' @param p vector of probabilities
#' @param n number of observations
#' @param theta zero-inflation parameter (probability of zeros)
#' @param size number of successes (dispersion parameter)
#' @param mu mean parameter (alternative parameterization)
#' @param log,log.p logical; if TRUE, probabilities \code{p} are given as \code{log(p)}.
#' @param lower.tail logical; if TRUE (default), probabilities are \code{P[X \leq x]}, otherwise,
#' \code{P[X > x]}.
#' @return \code{dzinb} gives the density, \code{pzinb} gives the distribution function,
#' \code{qzinb} gives the quantile function, and \code{rzinb} generates random deviates.
#' @export dzinb pzinb qzinb rzinb
#' @import stats

dzinb <-
function(x, theta = 0.5, size = 1, mu = 1, log = FALSE) {
  
  d <- theta * (x == 0) + (1 - theta) * dnbinom(x, size = size, mu = mu)
  
  if (log) {
    d <- theta * (x == 0) + (1 - theta) * dnbinom(x, size = size, mu = mu, log = TRUE)
  }
  
  return(d)
}

pzinb <-
function(q, theta = 0.5, size = 1, mu = 1, lower.tail = TRUE, log.p = FALSE) {
  p <- theta * (q) + (1 - theta) * pnbinom(x, size = size, mu = mu)
  
  if (lower.tail) {
    p <- 1 - p
  }
  
  if (log.p == TRUE) {
    p <- log(p)
  }
  
  return(p)
}

qzinb <-
function(p, theta = 0.5, size = 1, mu = 1, lower.tail = TRUE, log.p = FALSE) {
  
  if (lower.tail == FALSE) {
    p <- 1 - p
  }
  
  
}

rzinb <-
function(n, theta = 0.5, size = 1, mu = 1){
  zero   <- rbinom(n, 1, theta)
  y      <- rnbinom(n, size, mu = mu)
  output  <- ifelse(zero == 1, 0, y)
  return(output)
}
#' @name ZIGPD
#' @title Zero-inflated Generalized Pareto Distribution
#' @description Density, distribution function, quantile function and random generation for the zero-inflated
#' discrete generalized Pareto distribution with parameters \code{theta}, \code{mu}, \code{sigma}, and \code{xi}.
#' @param x,q vector of quantiles
#' @param p vector of probabilities
#' @param n number of observations
#' @param theta zero-inflation parameter (probability of zeros)
#' @param mu location parameter
#' @param sigma (non-negative) scale parameter
#' @param xi shape parameter
#' @param log,log.p logical; if TRUE, probabilities \code{p} are given as \code{log(p)}.
#' @param lower.tail logical; if TRUE (default), probabilities are \code{P[X \leq x]}, otherwise,
#' \code{P[X > x]}.
#' @return \code{dzigpd} gives the density, \code{pzigpd} gives the distribution function,
#' \code{qzigpd} gives the quantile function, and \code{rzigpd} generates random deviates.
#' @export dzigpd pzigpd qzigpd rzigpd
#' @import hurdlr

##### TODO Change order and parameter names ####

dzigpd <-
function(x, theta = 0.5, alpha=1, lambda=1, mu=0, log = FALSE) {
  zeros <- theta * (x == 0)
  d <- zeros + (1 - theta) * ddgpd(x, alpha, lambda, mu)
  if (log) {
    log(d)
  } else {
    d
  }
}

pzigpd <-
function(q, theta = 0.5, alpha=1, lambda=1, mu=0, lower.tail = TRUE, log.p = FALSE) {
  p <- theta + (1 - theta) * pdgpd(q, alpha, lambda, mu)
  
  if (lower.tail == FALSE) {
    p <- 1 - p
  }
  
  if (log.p) {
    p <- log(p)
  }
  
  p
}

qzigpd <-
function(p, theta = 0.5, alpha=1, lambda=1, mu=0, lower.tail = TRUE, log.p = FALSE) {
  # does not yet handle multiple values of theta
  
  if (lower.tail == FALSE) {
    p <- 1 - p
  }
  
  # assuming log(p) is given, convert to p by exp(log(p))
  if (log.p == TRUE) {
    p <- exp(p)
  }
  pindex <- theta < p
  res <- rep(NA_real_, length(p))
  res[pindex] <- qdgpd((p[pindex] - theta) / (1 - theta), alpha, lambda, mu)
  res[is.na(res)] <- 0
  res
}

rzigpd <-
function(n, theta = 0.5, alpha=1, lambda=1, mu=0) {
  zero   <- rbinom(n, 1, theta)
  y      <- rdgpd(n, alpha, lambda, mu)
  output  <- ifelse(zero==1, 0, y)
  output
}
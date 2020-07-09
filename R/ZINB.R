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
  zeros <- theta * (x == 0)
  d <- zeros + (1 - theta) * dnbinom(x, size = size, mu = mu)
  
  if (log) {
    log(d)
  }
  else {
    d
  }
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
  
  p
}

qzinb <-
function(p, theta = 0.5, size = 1, mu = 1, lower.tail = TRUE, log.p = FALSE) {
  
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
  res[pindex] <- qnbinom((p[pindex] - theta) / (1 - theta), size, mu = mu)
  res[is.na(res)] <- 0
  res
  
}

rzinb <-
function(n, theta = 0.5, size = 1, mu = 1){
  zero   <- rbinom(n, 1, theta)
  y      <- rnbinom(n, size, mu = mu)
  output  <- ifelse(zero == 1, 0, y)
  output
}
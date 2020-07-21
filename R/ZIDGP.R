#' @name ZIDGPD
#' @title Zero-inflated Discrete Generalized Pareto Distribution
#' @description Density, distribution function, quantile function and random generation for the zero-inflated
#' discrete generalized Pareto distribution with parameters \code{theta}, \code{mu}, \code{scale}, and \code{shape}.
#' @param x,q vector of quantiles
#' @param p vector of probabilities
#' @param n number of observations
#' @param theta zero-inflation parameter (probability of zeros)
#' @param mu location parameter
#' @param scale scale parameter
#' @param shape shape parameter
#' @param log,log.p logical; if TRUE, probabilities \code{p} are given as \code{log(p)}.
#' @param lower.tail logical; if TRUE (default), probabilities are \code{P[X \leq x]}, otherwise,
#' \code{P[X > x]}.
#' @return \code{dzigp} gives the density, \code{pzigp} gives the distribution function,
#' \code{qzigp} gives the quantile function, and \code{rzigp} generates random deviates.
#' @export dzigp pzigp qzigp rzigp

##### TODO Change order and parameter names in description ####

dzidgp <-
function(x, theta = 0.5, mu = 0, scale = 1, shape = 1, log = FALSE) {
  zeros <- theta * (x == 0)
  d <- zeros + (1 - theta) * ddgp(x, mu, scale, shape)
  if (log) {
    log(d)
  } else {
    d
  }
}

pzidgp <-
function(q, theta = 0.5, mu = 0, scale = 1, shape = 1, lower.tail = TRUE, log.p = FALSE) {
  p <- theta + (1 - theta) * pdgp(q, mu, scale, shape)
  
  if (lower.tail == FALSE) {
    p <- 1 - p
  }
  
  if (log.p) {
    p <- log(p)
  }
  
  p
}

qzidgp <-
function(p, theta = 0.5, mu = 0, scale = 1, shape = 1, lower.tail = TRUE, log.p = FALSE) {
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
  res[pindex] <- qdgp((p[pindex] - theta) / (1 - theta), mu, scale, shape)
  res[is.na(res)] <- 0
  res
}

rzidgp <-
function(n, theta = 0.5, mu = 0, scale = 1, shape = 1) {
  zero   <- rbinom(n, 1, theta)
  y      <- rdgp(n, mu, scale, shape)
  output  <- ifelse(zero==1, 0, y)
  output
}
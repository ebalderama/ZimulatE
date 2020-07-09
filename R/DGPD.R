#' @name DGPD
#' @title Discrete Generalized Pareto Distribution
#' @description Density, distribution function, quantile function and random generation for the discrete
#' generalized Pareto distribution with parameters \code{alpha}, \code{lambda}, and \code{mu}.
#' @param x,q vector of quantiles
#' @param p vector of probabilities
#' @param n number of observations
#' @param alpha shape parameter
#' @param lambda scale parameter
#' @param mu location parameter
#' @param log,log.p logical; if TRUE, probabilities \code{p} are given as \code{log(p)}.
#' @param lower.tail logical; if TRUE (default), probabilities are \code{P[X \leq x]}, otherwise,
#' \code{P[X > x]}.
#' @return \code{ddgpd} gives the density, \code{pdgpd} gives the distribution function,
#' \code{qdgpd} gives the quantile function, and \code{rdgpd} generates random deviates.
#' @export ddgpd pdgpd qdgpd rdgpd

ddgpd <-
function(x, alpha=1, lambda=1, mu=0, log = FALSE) {
  # suport is x = mu, mu + 1, mu + 2, ...
  support <- x >= mu
  res <- rep(NA_integer_, length(x))
  res[support] <- (1 + lambda * (x[support] - mu))**(-alpha) - (1 + lambda * (x[support] - mu + 1))**-alpha
  res[is.na(res)] <- 0
  res
}

pdgpd <-
function(q, alpha=1, lambda=1, mu=0, lower.tail = TRUE, log.p = FALSE) {
  
  # find where values of q are less than mu
  zindex <- q < mu
  
  p <- 1 - (1 + lambda * (q - mu + 1))**-alpha
  
  # replace with 0
  p[zindex] <- 0
  
  if (lower.tail == FALSE) {
    p <- 1 - p
  }
  
  return(p)
}

qdgpd <-
function(p, alpha=1, lambda=1, mu=0, lower.tail = TRUE, log.p = FALSE) {
  if (lower.tail == FALSE) {
    p <- 1 - p
  }
  if (log.p == TRUE) {
    p <- exp(p)
  }
  ceiling( ((1-p)**(-1/alpha) - 1) / lambda - 1 + mu )
}

rdgpd <-
function(n, alpha=1, lambda=1, mu=0) {
  sample(1:999999, n ,replace = T, prob = ddgpd(1:999999, alpha, lambda, mu))
}

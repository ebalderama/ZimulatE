#' @name DGP
#' @aliases ddgp
#' @aliases pdgp
#' @aliases qdgp
#' @aliases rdgp
#' @title Discrete Generalized Pareto Distribution
#' @description Density, distribution function, quantile function and random generation for the discrete
#' generalized Pareto distribution with parameters \code{mu}, \code{scale}, and \code{shape}.
#' @param x,q vector of quantiles
#' @param p vector of probabilities
#' @param n number of observations
#' @param mu location parameter
#' @param scale scale parameter
#' @param shape shape parameter
#' @param log,log.p logical; if TRUE, probabilities \code{p} are given as \code{log(p)}.
#' @param lower.tail logical; if TRUE (default), probabilities are \code{P[X \%leq x]}, otherwise,
#' \code{P[X > x]}.
#' @return \code{ddgp} gives the density, \code{pdgp} gives the distribution function,
#' \code{qdgp} gives the quantile function, and \code{rdgp} generates random deviates.

#' @rdname DGP
#' @export

ddgp <-
function(x, mu=0, scale=1, shape=1) {
  # suport is x = mu, mu + 1, mu + 2, ...
  support <- x >= mu
  res <- rep(NA_integer_, length(x))
  res[support] <- (1 + scale * (x[support] - mu))**(-shape) - (1 + scale * (x[support] - mu + 1))**(-shape)
  res[is.na(res)] <- 0
  res
}

#' @rdname DGP
#' @export

pdgp <-
function(q, mu=0, scale=1, shape=1, lower.tail = TRUE, log.p = FALSE) {
  
  # find where values of q are less than mu
  zindex <- q < mu
  
  p <- 1 - (1 + scale * (q - mu + 1))**-shape
  
  # replace with 0
  p[zindex] <- 0
  
  if (lower.tail == FALSE) {
    p <- 1 - p
  }
  
  return(p)
}

#' @rdname DGP
#' @export

qdgp <-
function(p, mu=0, scale=1, shape=1, lower.tail = TRUE, log.p = FALSE) {
  if (lower.tail == FALSE) {
    p <- 1 - p
  }
  if (log.p == TRUE) {
    p <- exp(p)
  }
  ceiling( ((1-p)**(-1/shape) - 1) / scale - 1 + mu )
}

#' @rdname DGP
#' @export

rdgp <-
function(n, mu=0, scale=1, shape=1) {
  sample(mu:(mu+999999), n ,replace = TRUE, prob = ddgp(mu:(mu+999999), mu = mu, scale = scale, shape = shape))
}
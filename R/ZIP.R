#' @name ZIP
#' @aliases dzip
#' @aliases pzip
#' @aliases qzip
#' @aliases rzip
#' @title Zero-inflated Poisson Distribution
#' @description Density, distribution function, quantile function and random generation for the zero-inflated Poisson
#' distribution with parameters \code{theta} and \code{lambda}.
#' @param x,q vector of quantiles
#' @param p vector of probabilities
#' @param n number of observations
#' @param theta zero-inflation parameter (probability of zeros)
#' @param lambda expected Poisson count
#' @param log,log.p logical; if TRUE, probabilities \code{p} are given as \code{log(p)}.
#' @param lower.tail logical; if TRUE (default), probabilities are \code{P[X \%leq x]}, otherwise,
#' \code{P[X > x]}.
#' @return \code{dzip} gives the density, \code{pzip} gives the distribution function,
#' \code{qzip} gives the quantile function, and \code{rzip} generates random deviates.
#' @import stats

#' @rdname ZIP
#' @export

dzip <-
function(x, theta = 0.5, lambda = 1, log = FALSE) {
  tt <- rep(0, length(x))
  zindex <- theta * (x == 0)
  tt <- zindex + (1 - theta) * dpois(x, lambda)
  if (log) {
    log(tt)
  } else {
    tt
  }
}

#' @rdname ZIP
#' @export

pzip <-
function(q, theta = 0.5, lambda = 1, lower.tail = TRUE, log.p = FALSE) {
  tt <- rep(0, length(q))
  tt <- theta + (1 - theta) * ppois(q, lambda)
  
  if (lower.tail == FALSE) {
    tt <- 1 - tt
  }
  
  if (log.p) {
    tt <- log(tt)
  }
  
  tt
}

#' @rdname ZIP
#' @export

qzip <-
function(p, theta = 0.5, lambda = 1, lower.tail = TRUE, log.p = FALSE) {
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
    res[pindex] <- qpois((p[pindex] - theta) / (1 - theta), lambda)
    res[is.na(res)] <- 0
    res
}

#' @rdname ZIP
#' @export

rzip <-
function(n, theta = 0.5, lambda = 1){
  zero   <- rbinom(n, 1, theta)
  y      <- rpois(n, lambda)
  output <- ifelse(zero == 1, 0, y)
  output
}

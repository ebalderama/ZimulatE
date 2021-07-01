#' @name HDGP
#' @aliases dhdgp
#' @aliases phdgp
#' @aliases rhdgp
#' @title Hurdle Model for Generalized Pareto Distribution
#' @description Density, distribution function, quantile function and random generation for the hurdle model of the
#' generalized Pareto distribution with parameters \code{theta}, \code{mu}, \code{scale}, and \code{shape}.
#' @param x,q vector of quantiles
#' @param p vector of probabilities
#' @param n number of observations
#' @param theta zero generating parameter (probability of zeros)
#' @param mu location parameter
#' @param scale (non-negative) scale parameter
#' @param shape shape parameter
#' @param log,log.p logical; if TRUE, probabilities \code{p} are given as \code{log(p)}.
#' @param lower.tail logical; if TRUE (default), probabilities are \code{P[X \%leq x]}, otherwise,
#' \code{P[X > x]}.
#' @return \code{dhdgp} gives the density, \code{phdgp} gives the distribution function,
#' \code{qhdgp} gives the quantile function, and \code{rhdgp} generates random deviates.
#' @examples 
#' dhdgp(c(0, 1, 2))
#' dhdgp(24, theta = 0.8, mu = 1) 
#' @import stats


#' @rdname HDGP
#' @export

dhdgp <-
function(x, theta = 0.5, mu=0, scale=1, shape=1, log = FALSE) {
  tt <- rep(0, length(x))
  tt <- (1 - theta) * ddgp(x, mu = mu, scale = scale, shape = shape) / pdgp(0, mu = mu, scale = scale, shape = shape)
  
  tt[x == 0L] <- rep(theta, length(tt))[x == 0L]
  tt
}

#' @rdname HDGP
#' @export

phdgp <-
function(q, theta = 0.5, mu=0, scale=1, shape=1, lower.tail = TRUE, log.p = FALSE) {
  tt <- pmax(0, pdgp(q, mu = mu, scale = scale, shape = shape) - ddgp(0L, mu = mu, scale = scale, shape = shape)) /
    pdgp(0, mu = mu, scale = scale, shape = shape, lower.tail = FALSE) *
    (1 - theta)
  zindex <- tt == 0L
  tt[zindex] <- 0
  tt <- tt + theta
  tt[q < 0] <- 0
  tt
}

#' @rdname HDGP
#' @export

rhdgp <- function(n, theta = 0.5, mu = 0, scale = 1, shape = 1){
  
  zero <- rbinom(n, 1, theta)
  sum_zero <- sum(zero == 1)
  sum_non_zero <- n - sum_zero
  # z_trun <- rdgp(sum_non_zero, mu = mu, scale, shape)
  z_trun <- sample(1:999999, sum_non_zero, replace = TRUE, prob = ddgp(1:999999, mu = mu, scale = scale, shape = shape))
  
  output <- c(rep(0, sum_zero), z_trun)
  output
}
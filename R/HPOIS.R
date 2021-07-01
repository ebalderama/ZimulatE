#' @name HPOIS
#' @aliases dhpois
#' @aliases phpois
#' @aliases rhpois
#' @title Hurdle Model for Poisson Distribution
#' @description Density, distribution function, quantile function and random generation for the hurdle model
#' using Poisson distribution with parameters \code{theta} and \code{lambda}.
#' @param x,q vector of quantiles
#' @param p vector of probabilities
#' @param n number of observations
#' @param theta zero-generating parameter (probability of zeros)
#' @param lambda expected Poisson count
#' @param log,log.p logical; if TRUE, probabilities \code{p} are given as \code{log(p)}.
#' @param lower.tail logical; if TRUE (default), probabilities are \code{P[X \%leq x]}, otherwise,
#' \code{P[X > x]}.
#' @return \code{dhpois} gives the density, \code{phpois} gives the distribution function,
#' \code{qhpois} gives the quantile function, and \code{rhpois} generates random deviates.
#' @import stats

#' @rdname HPOIS
#' @export

dhpois <-
function(x, theta = 0.5, lambda = 1, log = FALSE) {
  tt <- rep(0, length(x))
  tt <- (1 - theta) * dpois(x, lambda = lambda) / ppois(0, lambda = lambda, lower.tail = FALSE)
  tt[x == 0L] <- rep(theta, length(tt))[x == 0L]
}

#' @rdname HPOIS
#' @export

phpois <-
function(q, theta = 0.5, lambda = 1, lower.tail = TRUE, log.p = FALSE) {
  tt <- pmax(0, ppois(q, lambda = lambda) - dpois(0L, lambda = lambda)) /
        ppois(0, lambda = lambda, lower.tail = FALSE) *
        (1 - theta)
  zindex <- tt == 0L
  tt[zindex] <- 0
  tt <- tt + theta
  tt[q < 0] <- 0
  tt
}

#' @rdname HPOIS
#' @export

rhpois <-
function(n, theta=0.5, lambda=1){
  
  zero <- rbinom(n, 1, theta)
  sum_zero <- sum(zero == 1)
  sum_non_zero <- n - sum_zero
  
  z_trun <- sample(1:999999, sum_non_zero, replace = TRUE, prob = dpois(1:999999,lambda))
  
  output <- c(rep(0,sum_zero), z_trun)
  return(output)
}

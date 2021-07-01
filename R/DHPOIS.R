#' @name DHPOIS
#' @aliases ddhpoisdgp
#' @aliases pdhpoisdgp
#' @aliases rdhpoisdgp
#' @title Double-Hurdle Poisson
#' @description Density, distribution function, quantile function and random generation for the double-hurdle
#' Poisson distribution with parameters \code{theta}, \code{lambda}, \code{theta_1}, \code{mu},
#' \code{scale}, \code{shape}, and \code{threshold}.
#' @param x,q vector of quantiles
#' @param p vector of probabilities
#' @param n number of observations
#' @param theta zero-generating parameter (probability of zeros)
#' @param lambda expected Poisson count
#' @param theta_1 probability of a count at or above \code{mu}, conditional on non-zero count
#' @param scale scale parameter
#' @param shape shape parameter
#' @param threshold second hurdle
#' @param log,log.p logical; if TRUE, probabilities \code{p} are given as \code{log(p)}.
#' @param lower.tail logical; if TRUE (default), probabilities are \code{P[X \%leq x]}, otherwise,
#' \code{P[X > x]}.
#' @return \code{ddhpoisdgp} gives the density, \code{pdhpoisdgp} gives the distribution function,
#' \code{qdhpoisdgp} gives the quantile function, and \code{rdhpoisdgp} generates random deviates.
#' @import stats

#' @rdname DHPOIS
#' @export

ddhpoisdgp <-
  function(x, theta = 0.5, threshold = 3, lambda = 1, theta_1 = 0.1, scale=1, shape=1){
    tt <- numeric(length(x))
    tt[x == 0] <- theta
    tt[x >= 1 & x < threshold] <- 
      (1 - theta) * (1 - theta_1) * stats::dpois(x[x >= 1 & x < threshold], lambda = lambda) / (stats::ppois(threshold - 1, lambda = lambda) - stats::ppois(0, lambda = 1))
    tt[x >= threshold] <- (1 - theta) * theta_1 * ddgp(x[x >= threshold], mu = threshold, scale = scale, shape = shape)
    return(tt)
  }

#' @rdname DHPOIS
#' @export

pdhpoisdgp <- function(q, theta = 0.5, lambda = 1, threshold = 3, theta_1 = 0.1, scale = 1, shape = 1, lower.tail = TRUE, log.p = FALSE) {
  tt <- rep(0, length(q))
  tt <- cumsum(ddhpoisdgp(q, theta = theta, threshold = threshold, lambda = lambda, theta_1 = theta_1, scale = scale, shape = shape))
  tt
}

#' @rdname DHPOIS
#' @export

rdhpoisdgp <-
function(n, theta = 0.5, lambda = 1, theta_1 = 0.1, scale=1, shape=1, threshold = 3){
  
  # theta_1 : given a postive value, there is a 0.95 prob. it is from the pois.
  
  zero   <- rbinom(n, 1, theta)
  sum_zero <- sum(zero == 1)
  middle <- rbinom(n-sum_zero, 1, theta_1)
  
  n_middle <- sum(middle == 1)
  n_last <- n - sum_zero - n_middle #ok
  
  #z_trun <- rztpois(n_middle, lambda = lambda)
  z_trun <- sample(1:(threshold-1), n_middle, replace = TRUE, 
                 prob = (stats::dpois(1:(threshold-1),lambda)/(stats::ppois(threshold-1,lambda) - stats::ppois(0,lambda))))
  
  y2 <- rdgp(n_last, mu = threshold, scale = scale, shape = shape)
  
  output <- c(rep(0, sum_zero), z_trun, y2)
  output
}

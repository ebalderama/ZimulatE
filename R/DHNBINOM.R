#' @name DHNBINOM
#' @title Double-Hurdle Model for Negative Binomial Distribution
#' @description Density, distribution function, quantile function and random generation for the hurdle model
#' using negative binomial distribution with parameters \code{theta}, \code{size}, and \code{prob}.
#' @param x,q vector of quantiles
#' @param p vector of probabilities
#' @param n number of observations
#' @param theta zero generating parameter (probability of zeros)
#' @param theta_1 probability of a count at or above \code{mu}, conditional on non-zero count
#' @param size number of successes (dispersion parameter)
#' @param prob probability of success in each trial. 0 < prob < 1
#' @param threshold second hurdle
#' @param log,log.p logical; if TRUE, probabilities \code{p} are given as \code{log(p)}.
#' @param lower.tail logical; if TRUE (default), probabilities are \code{P[X \leq x]}, otherwise,
#' \code{P[X > x]}.
#' @return \code{dhnbinom} gives the density, \code{phnbinom} gives the distribution function,
#' \code{qhnbinom} gives the quantile function, and \code{rhnbinom} generates random deviates.
#' @export ddhnbinom pdhnbinom qdhnbinom rdhnbinom
#' @import hurdlr BMS

ddhnbinom <-
  function(x, theta = 0.5, theta_1 = 0.5, size = 0.9, mu = 0.9, scale = 1, shape = 1, threshold, log = FALSE) {
    tt <- numeric(length(x))
    tt[x == 0] <- theta
    tt[x >= 1 & x < threshold] <-
      (1 - theta) * (1 - theta_1) * ZimulatE::dtrunc(x[x >= 1 & x < threshold], spec = "nbinom", a = 1, b = threshold, size = size, mu = mu)
    tt[x >= threshold] <- (1 - theta) * theta_1 * ddgp(x[x >= threshold], mu = threshold, scale = scale, shape = shape)
    tt
  }

pdhnbinom <-
  function(q, theta = 0.5, theta_1 = 0.95, size = 0.9, mu = 0.9, scale = 1, shape = 1, lower.tail = TRUE, log.p = FALSE, threshold) {
    tt <- rep(0, length(q))
    middle <- pmax(0, pnbinom(q[q <= threshold - 1], size = size, mu = mu) - dnbinom(0L, size = size, mu = mu)) /
      (pnbinom(threshold, size = size, mu = mu) - pnbinom(1, size = size, mu = mu) ) *
      (1 - theta) * (1 - theta_1)
    tt[q <= threshold - 1] <- middle + theta
    tt[q >= threshold] <- (1 - theta) * (theta_1) * (pdgp(q[q >= threshold], mu = threshold, scale = scale, shape = shape)) + theta
    # tt <- (1 - theta) * (theta_1) * pmax(0, pdgp(q, mu = threshold, scale = scale, shape = shape) - ddgp(0, mu = threshold, scale = scale, shape = shape))
    # tt[q > 0 & q <= threshold - 1] <- tt[q > 0 && q <= threshold - 1] + middle[q > 0 && q <= threshold - 1]
    tt
}

# pdhpoisdgp <-
#   function(q, theta = 0.5, lambda = 1, theta_1 = 0.95, scale = 1, shape = 1, lower.tail = TRUE, log.p = FALSE, threshold=3) {
#     tt <- rep(0, length(q))
#     middle <- pmax(0, ppois(q[q <= threshold - 1], lambda = lambda) - dpois(0L, lambda = lambda)) /
#       (ppois(threshold, lambda = lambda) - ppois(1, lambda = lambda) ) *
#       (1 - theta) * (1 - theta_1)
#     tt[q <= threshold - 1] <- middle
#     tt[q >= threshold] <- (1 - theta) * (theta_1) * (pdgp(q[q >= threshold], mu = threshold, scale = scale, shape = shape))
#     tt <- tt + theta
#     tt
#   }

# qdhnbinom <-
# function(p, theta = 0.5, size = 1, prob = 1, lower.tail = TRUE, log.p = FALSE) {
#   return(0)
# }

rdhnbinom <- function(n, theta=0.5,theta_1=0.5, size = 1, mu_1 = 1, sigma = 1, xi = 1, threshold=Inf){
  zero <- rbinom(n,1,theta)
  sum_zero<-sum(zero==1)
  middle<-rbinom(n - sum_zero,1,theta_1)
  n_middle<-sum(middle==1)
  n_last<- n - sum_zero - n_middle
  
  z_trun<-sample(1:(threshold-1),n_middle,replace = T, 
                 prob = (dnbinom(1:(threshold-1),size,mu=mu_1)/(pnbinom(threshold-1,size,mu=mu_1) - pnbinom(0,size,mu=mu_1))))
  
  y2 <- rgpd(n_last, mu=threshold, sigma, xi)
  
  output <- c(rep(0,sum_zero), z_trun, y2)
  return(output)
}

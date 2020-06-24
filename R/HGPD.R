#' @name HGPD
#' @title Hurdle Model for Generalized Pareto Distribution
#' @description Density, distribution function, quantile function and random generation for the hurdle model of the
#' generalized Pareto distribution with parameters \code{theta}, \code{mu}, \code{sigma}, and \code{xi}.
#' @param x,q vector of quantiles
#' @param p vector of probabilities
#' @param n number of observations
#' @param theta zero generating parameter (probability of zeros)
#' @param mu location parameter
#' @param sigma (non-negative) scale parameter
#' @param xi shape parameter
#' @param log,log.p logical; if TRUE, probabilities \code{p} are given as \code{log(p)}.
#' @param lower.tail logical; if TRUE (default), probabilities are \code{P[X \leq x]}, otherwise,
#' \code{P[X > x]}.
#' @return \code{dhgpd} gives the density, \code{phgpd} gives the distribution function,
#' \code{qhgpd} gives the quantile function, and \code{rhgpd} generates random deviates.
#' @export dhgpd phgpd qhgpd rhgpd
#' @import hurdlr

dhgpd <-
function(x, theta, mu = 1, sigma = 1, xi = 1, log = FALSE) {
  return(0)
}

phgpd <-
function(q, theta, mu = 1, sigma = 1, xi = 1, lower.tail = TRUE, log.p = FALSE) {
  return(0)
}

qhgpd <-
function(p, theta, mu = 1, sigma = 1, xi = 1, lower.tail = TRUE, log.p = FALSE) {
  return(0)
}

#------ TODO: Change to discrete -------------
#GPD:(continuous)
rhgpd <- function(n,theta=0.5, sigma = 1, xi = 1){
  
  zero   <- rbinom(n,1,theta)
  sum_zero<-sum(zero==1)
  sum_non_zero<- n-sum_zero
  z_trun<-rgpd(sum_non_zero, mu=0.5, sigma, xi)
  
  output <- c(rep(0,sum_zero),z_trun)
  return(output)
}
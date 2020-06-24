#' @name HLNORM
#' @title Hurdle Model for Log-normal Distribution
#' @description Density, distribution function, quantile function and random generation for the hurdle model of
#' log-normal distribution with parameters \code{theta}, \code{meanlog}, and \code{sdlog}.
#' @param x,q vector of quantiles
#' @param p vector of probabilities
#' @param n number of observations
#' @param theta zero generating parameter (probability of zeros)
#' @param meanlog mean (log scale)
#' @param sdlog standard deviation (log scale)
#' @param log,log.p logical; if TRUE, probabilities \code{p} are given as \code{log(p)}.
#' @param lower.tail logical; if TRUE (default), probabilities are \code{P[X ??? x]}, otherwise,
#' \code{P[X > x]}.
#' @return \code{dhlnorm} gives the density, \code{phlnorm} gives the distribution function,
#' \code{qhlnorm} gives the quantile function, and \code{rhlnorm} generates random deviates.
#' @export dhlnorm phlnorm qhlnorm rhlnorm
#' @import stats

dhlnorm <-
function(x, theta, mu = 1, sigma = 1, xi = 1) {
  return(0)
}

phlnorm <-
function(q, theta, mu = 1, sigma = 1, xi = 1, lower.tail = TRUE, log.p = FALSE) {
  return(0)
}

qhlnorm <-
function(p, theta, mu = 1, sigma = 1, xi = 1, lower.tail = TRUE, log.p = FALSE) {
  return(0)
}

# ---------- TODO: Change to discrete ------------
rhlnorm <-
function(n, theta = 0.5, meanlog = 0, sdlog = 1){
  
  zero   <- rbinom(n,1,theta)
  sum_zero<-sum(zero==1)
  sum_non_zero<- n-sum_zero
  z_trun<-rlnorm(sum_non_zero, meanlog, sdlog)
  
  output <- c(rep(0,sum_zero),z_trun)
  return(output)
}
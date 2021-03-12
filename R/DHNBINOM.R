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
#' @param log,log.p logical; if TRUE, probabilities \code{p} are given as \code{log(p)}.
#' @param lower.tail logical; if TRUE (default), probabilities are \code{P[X \leq x]}, otherwise,
#' \code{P[X > x]}.
#' @return \code{dhnbinom} gives the density, \code{phnbinom} gives the distribution function,
#' \code{qhnbinom} gives the quantile function, and \code{rhnbinom} generates random deviates.
#' @export ddhnbinom pdhnbinom qdhnbinom rdhnbinom
#' @import hurdlr BMS

ddhnbinom <-
  function(x, theta = 0.5, theta_1 = 0.5, size = 1, mu = 1, log = FALSE) {
    tt <- (1 - theta) * dnbinom(x, size = size, mu = mu) / (1 - pnbinom(0, size = size, mu = mu))
    
    
    tt[zindex] <- theta
    if (log) {
      return(tt)
    } else {
      tt
    }
  }

# Issue: possible that phnbinom returns a negative probability ----------
# phnbinom <-
# function(q, theta = 0.5, size = 1, prob = 1, lower.tail = TRUE, log.p = FALSE) {
#   #hypergeometric2F1(12,1,5,0.5, log = FALSE)
#   numer <- (-1 + theta) * ( 1 - prob**size - (1 - prob)**(1+q) * prob**size * choose(size + q, -1 + size) * f21hyper(1, 1 + size + q, 2 + q, 1 - prob))
#   denom <- -1 + prob**size
#   numer / denom
# }

pdhnbinom <-
  function(q, theta = 0.5, theta_1 = 0.5, size = 1, mu = 1, lower.tail = TRUE, log.p = FALSE) {
    tt <- pmax(0, pnbinom(q, size = size, mu = mu) - dnbinom(0L, size = size, mu = mu)) /
      pnbinom(0, size = size, mu = mu, lower.tail = FALSE) *
      theta
    zindex <- tt == 0L
    tt[zindex] <- 0
    tt <- tt + (1 - theta)
    tt[q < 0] <- 0
    tt
  }


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

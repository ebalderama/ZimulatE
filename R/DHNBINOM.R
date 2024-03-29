#' @name DHNBINOM
#' @aliases ddhnbinom
#' @aliases pdhnbinom
#' @aliases rdhnbinom
#' @title Double-Hurdle Model for Negative Binomial Distribution
#' @description Density, distribution function, quantile function and random generation for the hurdle model
#' using negative binomial distribution with parameters \code{theta}, \code{size}, and \code{prob}.
#' @param x,q vector of quantiles
#' @param p vector of probabilities
#' @param n number of observations
#' @param theta zero generating parameter (probability of zeros)
#' @param theta_1 probability of a count at or above \code{mu}, conditional on non-zero count
#' @param size number of successes (dispersion parameter)
#' @param mu probability of success in each trial. 0 < prob < 1
#' @param scale scale parameter
#' @param shape shape parameter
#' @param threshold second hurdle
#' @param log,log.p logical; if TRUE, probabilities \code{p} are given as \code{log(p)}.
#' @param lower.tail logical; if TRUE (default), probabilities are \code{P[X \%leq x]}, otherwise,
#' \code{P[X > x]}.
#' @return \code{dhnbinom} gives the density, \code{phnbinom} gives the distribution function,
#' \code{qhnbinom} gives the quantile function, and \code{rhnbinom} generates random deviates.
#' @import stats


#' @rdname DHNBINOM
#' @export

ddhnbinom <-
  function(x, theta = 0.5, theta_1 = 0.1, size = 0.9, mu = 0.9, scale = 1, shape = 1, threshold = 3, log = FALSE) {
    tt <- numeric(length(x))
    tt[x == 0] <- theta
    tt[x >= 1 & x < threshold] <-
      (1 - theta) * (1 - theta_1) * stats::dnbinom(x[x >= 1 & x < threshold], size = size, mu = mu) / (stats::pnbinom(threshold - 1, size = size, mu = mu) - stats::pnbinom(0, size = size, mu = mu))
    tt[x >= threshold] <- (1 - theta) * theta_1 * ddgp(x[x >= threshold], mu = threshold, scale = scale, shape = shape)
    tt
}

#' @rdname DHNBINOM
#' @export

pdhnbinom <-
  function(q, theta = 0.5, theta_1 = 0.1, size = 0.9, mu = 0.9, scale = 1, shape = 1, lower.tail = TRUE, log.p = FALSE, threshold) {
    tt <- rep(0, length(q))
    middle <- pmax(0, stats::pnbinom(q[q <= threshold - 1], size = size, mu = mu) - stats::dnbinom(0L, size = size, mu = mu)) /
      (stats::pnbinom(threshold, size = size, mu = mu) - stats::pnbinom(1, size = size, mu = mu) ) *
      (1 - theta) * (1 - theta_1)
    tt[q <= threshold - 1] <- middle + theta
    tt[q >= threshold] <- (1 - theta) * (theta_1) * (pdgp(q[q >= threshold], mu = threshold, scale = scale, shape = shape)) + theta
    tt
}

#' @rdname DHNBINOM
#' @export

rdhnbinom <- function(n, theta=0.5,theta_1=0.1, size = 0.9, mu = 0.9, scale = 1, shape = 1, threshold=3){
  zero <- rbinom(n,1,theta)
  sum_zero<-sum(zero==1)
  middle<-rbinom(n - sum_zero,1,theta_1)
  n_middle<-sum(middle==1)
  n_last<- n - sum_zero - n_middle
  
  z_trun<-sample(1:(threshold-1), n_middle,replace = TRUE, 
                 prob = (stats::dnbinom(1:(threshold-1),size,mu=mu)/(stats::pnbinom(threshold-1,size,mu=mu) - stats::pnbinom(0,size,mu=mu))))
  
  y2 <- rdgp(n_last, mu=threshold, scale, shape)
  
  output <- c(rep(0,sum_zero), z_trun, y2)
  return(output)
}

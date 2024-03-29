% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ZIDGP.R
\name{ZIDGP}
\alias{ZIDGP}
\alias{dzidgp}
\alias{pzidgp}
\alias{qzidgp}
\alias{rzidgp}
\title{Zero-inflated Discrete Generalized Pareto Distribution}
\usage{
dzidgp(x, theta = 0.5, mu = 0, scale = 1, shape = 1, log = FALSE)

pzidgp(
  q,
  theta = 0.5,
  mu = 0,
  scale = 1,
  shape = 1,
  lower.tail = TRUE,
  log.p = FALSE
)

qzidgp(
  p,
  theta = 0.5,
  mu = 0,
  scale = 1,
  shape = 1,
  lower.tail = TRUE,
  log.p = FALSE
)

rzidgp(n, theta = 0.5, mu = 0, scale = 1, shape = 1)
}
\arguments{
\item{x, q}{vector of quantiles}

\item{theta}{zero-inflation parameter (probability of zeros)}

\item{mu}{location parameter}

\item{scale}{scale parameter}

\item{shape}{shape parameter}

\item{log, log.p}{logical; if TRUE, probabilities \code{p} are given as \code{log(p)}.}

\item{lower.tail}{logical; if TRUE (default), probabilities are \code{P[X \%leq x]}, otherwise,
\code{P[X > x]}.}

\item{p}{vector of probabilities}

\item{n}{number of observations}
}
\value{
\code{dzigp} gives the density, \code{pzigp} gives the distribution function,
\code{qzigp} gives the quantile function, and \code{rzigp} generates random deviates.
}
\description{
Density, distribution function, quantile function and random generation for the zero-inflated
discrete generalized Pareto distribution with parameters \code{theta}, \code{mu}, \code{scale}, and \code{shape}.
}

library(testthat)
library(ZimulatE)

test_check("ZimulatE")

test_that("Zero-Inflated Poisson", {
  theta <- 0.5
  lambda <- 1
  n <- 999999
  
  # support for X ~ ZIP is the set of natural numbers
  expect_equal(dzip(-1), 0)
  
  # # quantiles
  expect_equal(qzip(0), 0)
  expect_equal(qzip(1), Inf)
  # expect_equal(quantile(dist, 0.61), stats::qbinom(0.61, 1, 0.4))
  # 
  # # pdf
  # expect_equal(density(dist, 0), stats::dbinom(0, 1, 0.4))
  # expect_equal(density(dist, 1), stats::dbinom(1, 1, 0.4))
  # 
  # # cdf
  # expect_equal(cdf(dist, 0), stats::pbinom(0, 1, 0.4))
  # expect_equal(cdf(dist, 1), stats::pbinom(1, 1, 0.4))
  # 
  # # F(Finv(a)) ~= a
  # expect_equal(cdf(dist, quantile(dist, 0.6)), 0.6, tolerance = 1e-3)
  
  # mean and variance
  expect_equal(mean(rzip(n, theta, lambda)), (1-theta)*lambda, tolerance = 1e-3)
  expect_equal(var(rzip(n, theta, lambda)), lambda * (1 - theta) * (1 + theta * lambda), tolerance = 1e-3)
})

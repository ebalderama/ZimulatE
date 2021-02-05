library(testthat)
library(ZimulatE)

test_that("Zero-Inflated Negative Binomial", {
  theta = 0.5 # zero-inflation parameter
  size = 1 # dispersion parameter
  mu = 1 # mean parameter
  n <- 99
  
  # support for X ~ ZINB is nonnegative integers
  expect_equal(dzinb(0), 0.75)
  expect_equal(dzinb(-1), 0)
  expect_equal(dzinb(-0.5), 0)
  expect_equal(dzinb(c(-2, -1.5, -1, 3)), rep(0, 4))
  
  # quantiles
  expect_equal(qzinb(0), 0)
  expect_equal(qzinb(1), Inf)
  
  # pdf
  # expect_equal(dzinb(), )

  # cdf
  # expect_equal(cdf(dist, 0), stats::pbinom(0, 1, 0.4))
  # expect_equal(cdf(dist, 1), stats::pbinom(1, 1, 0.4))
  # 
  # # F(Finv(a)) ~= a
  # expect_equal(cdf(dist, quantile(dist, 0.6)), 0.6, tolerance = 1e-3)
  
  # mean and variance
  expect_equal(mean(rzinb(n, theta, size, mu)), (1-theta)*mu, tolerance = 1e-2)
  expect_equal(var(rzinb(n, theta, size, mu)), (1 - theta) * mu * (1 + mu * (theta + size)), tolerance = 1e-2)
})
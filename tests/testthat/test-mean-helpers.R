library(testthat)


test_that("unirootSlim finds roots", {
  root <- unirootSlim(function(x) x^2 - 4, interval = c(0, 3))
  expect_equal(root, 2, tolerance = 1e-6)
})


test_that("weightByMean and weightByMeanLinear adjust means", {
  w <- rep(1, 4)
  v <- c(0, 1, 2, 3)

  w1 <- weightByMean(w, v, mean.target = 2.0)
  m1 <- sum(w1 * v) / sum(w1)
  expect_equal(m1, 2.0, tolerance = 1e-3)

  w2 <- weightByMeanLinear(w, v, mean.target = 2.5)
  m2 <- sum(w2 * v) / sum(w2)
  expect_equal(m2, 2.5, tolerance = 1e-3)
})


test_that("weightEfficiency and calc_efficiency behave sensibly", {
  w <- rep(1, 5)
  expect_equal(weightEfficiency(w, w), 100, tolerance = 1e-6)
  expect_equal(calc_efficiency(w), 100, tolerance = 1e-6)
})

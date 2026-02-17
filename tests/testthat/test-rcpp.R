library(testthat)


test_that("Rcpp wrappers are available when shared library is loaded", {
  skip_if_not(is.loaded("_drake_anyIsNA"))

  expect_true(anyIsNA(c(1, NA)))
  expect_false(anyIsNA(c(1, 2, 3)))

  w <- rep(1, 4)
  v <- c(0, 1, 2, 3)
  w2 <- CWeightByMeanLinear(w, v, 2.5)
  m2 <- sum(w2 * v) / sum(w2)
  expect_equal(m2, 2.5, tolerance = 1e-3)

  hf <- HiFunction(
    k = 1.1,
    hivw = c(1),
    lovw = c(1),
    hidiff = c(1),
    lodiff = c(1),
    loweight = c(1),
    hiweight = c(1),
    meantarget = 1,
    hilength = 1,
    lolength = 1
  )
  lf <- LoFunction(
    k = 1.1,
    hivw = c(1),
    lovw = c(1),
    hidiff = c(1),
    lodiff = c(1),
    loweight = c(1),
    hiweight = c(1),
    meantarget = 1,
    hilength = 1,
    lolength = 1
  )

  expect_true(is.numeric(hf))
  expect_true(is.numeric(lf))

  hz <- HiZero(
    a = 1,
    b = 2,
    t = 1e-8,
    hivw = c(1),
    lovw = c(1),
    hidiff = c(1),
    lodiff = c(1),
    loweight = c(1),
    hiweight = c(1),
    meantarget = 1,
    hilength = 1,
    lolength = 1
  )

  lz <- LoZero(
    a = 1,
    b = 2,
    t = 1e-8,
    hivw = c(1),
    lovw = c(1),
    hidiff = c(1),
    lodiff = c(1),
    loweight = c(1),
    hiweight = c(1),
    meantarget = 1,
    hilength = 1,
    lolength = 1
  )

  expect_true(is.numeric(hz))
  expect_true(is.numeric(lz))
})

library(testthat)


test_that("Rcpp wrappers are available when shared library is loaded", {
  skip_if_not(is.loaded("_drake_anyIsNA"))

  expect_true(anyIsNA(c(1, NA)))
  expect_false(anyIsNA(c(1, 2, 3)))

  basis <- CBuildGaussianBasis(c(0, 1), c(0, 0.5, 1), 0.5)
  expect_equal(dim(basis), c(2, 3))

  cont_weights <- CWeightByContinuousBasis(
    weights = c(1, 2),
    basis = basis,
    match_index = c(1L, 3L),
    target_y = c(0.3, 0.4, 0.3)
  )
  expect_length(cont_weights, 2)
  expect_true(all(is.finite(cont_weights)))

  cont_diff <- CContinuousBasisDiff(
    weights = c(1, 2),
    basis = basis,
    target_y = c(0.3, 0.4, 0.3)
  )
  expect_true(is.numeric(cont_diff))
  expect_true(is.finite(cont_diff))

  cont_gauss_weights <- CWeightByContinuousGaussian(
    x = c(0, 1),
    weights = c(1, 2),
    xout = c(0, 0.5, 1),
    bw = 0.5,
    match_index = c(1L, 3L),
    target_y = c(0.3, 0.4, 0.3)
  )
  expect_length(cont_gauss_weights, 2)
  expect_true(all(is.finite(cont_gauss_weights)))

  cont_gauss_diff <- CContinuousGaussianDiff(
    x = c(0, 1),
    weights = c(1, 2),
    xout = c(0, 0.5, 1),
    bw = 0.5,
    target_y = c(0.3, 0.4, 0.3)
  )
  expect_true(is.numeric(cont_gauss_diff))
  expect_true(is.finite(cont_gauss_diff))

  discrete_many <- CWeightByDiscreteMany(
    codes_list = list(c(1L, 2L, 1L), c(1L, 1L, 2L)),
    weights = c(1, 1, 1),
    targets_list = list(c(0.4, 0.6), c(0.7, 0.3))
  )
  expect_length(discrete_many, 3)
  expect_true(all(is.finite(discrete_many)))

  discrete_many_diff <- CMaxAbsDiscreteDiffMany(
    codes_list = list(c(1L, 2L, 1L)),
    weights = c(1, 1, 1),
    targets_list = list(c(0.5, 0.5))
  )
  expect_true(is.numeric(discrete_many_diff))

  split_rows <- CSplitRowsByCode(c(2L, NA_integer_, 1L, 2L), 3L)
  expect_length(split_rows, 3)
  expect_equal(split_rows[[1]], 3L)
  expect_equal(split_rows[[2]], c(1L, 4L))
  expect_equal(split_rows[[3]], integer(0))

  clamped <- CClampWeights(c(0.01, 1, 100, NA_real_), 10, 0.1)
  expect_equal(clamped[1:3], c(0.1, 1, 10))
  expect_true(is.na(clamped[[4]]))

  density_applied <- CApplyDensityTarget(
    weights = c(1, 2),
    match_index = c(1L, 2L),
    sample_y = c(4, 1),
    target_y = c(0.5, 0.5)
  )
  expect_equal(density_applied, c(0.625, 5), tolerance = 1e-8)

  density_diff <- CContinuousDiffFromDensity(
    sample_y = c(4, 1),
    target_y = c(0.5, 0.5)
  )
  expect_equal(density_diff, 0.6, tolerance = 1e-8)

  subset_many <- CWeightByDiscreteSubsetMany(
    target_code_list = list(c(1L, 2L, 1L, 2L)),
    strata_code_list = list(c(1L, 1L, 2L, 2L)),
    weights = c(1, 1, 1, 1),
    targets_by_strata_list = list(matrix(c(0.6, 0.4, 0.3, 0.7), nrow = 2))
  )
  expect_length(subset_many, 4)
  expect_true(all(is.finite(subset_many)))

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

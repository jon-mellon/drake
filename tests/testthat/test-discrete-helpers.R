library(testthat)


test_that("wttabSlim aggregates weights", {
  x <- c("A", "B", "A")
  w <- c(1, 2, 3)
  res <- wttabSlim(x, weights = w, current.levels = c("A", "B"))
  expect_equal(as.numeric(res), c(4, 2))
})


test_that("fixDiscreteOrder aligns target ordering", {
  df <- data.frame(var = factor(c("b", "a", "b"), levels = c("b", "a")))
  targets <- list(var = c(a = 0.6, b = 0.4))
  out <- fixDiscreteOrder(df, "var", targets)
  expect_equal(names(out$var), c("b", "a"))

  bad_targets <- list(var = c(a = 0.5, c = 0.5))
  expect_error(fixDiscreteOrder(df, "var", bad_targets), "Sample values not in targets")
})

test_that("validateNormalizeDiscreteTargetVector normalizes under-sums", {
  result <- validateNormalizeDiscreteTargetVector(
    c(M = 0.2, F = 0.7),
    label = "gender",
    tol = 1e-4
  )

  expect_true(result$normalized)
  expect_equal(sum(result$target), 1, tolerance = 1e-12)
  expect_equal(unname(result$target), c(2 / 9, 7 / 9), tolerance = 1e-12)
})

test_that("validateNormalizeDiscreteTargetVector errors on over-sums", {
  expect_error(
    validateNormalizeDiscreteTargetVector(c(M = 0.8, F = 0.4), label = "gender", tol = 1e-4),
    "sum to more than 1"
  )
})

test_that("validateNormalizeDiscreteTargetVector leaves near-1 sums unchanged", {
  target <- c(M = 0.5, F = 0.49995)
  result <- validateNormalizeDiscreteTargetVector(target, label = "gender", tol = 1e-4)

  expect_false(result$normalized)
  expect_equal(result$target, target)
})

test_that("normalizeDiscreteTargets warns and normalizes affected entries", {
  targets <- list(
    gender = c(M = 0.2, F = 0.7),
    region = c(North = 0.5, South = 0.49995)
  )

  expect_warning({
    out <- normalizeDiscreteTargets(targets, tol = 1e-4)
  }, "Normalized discrete.targets")

  expect_equal(sum(out$gender), 1, tolerance = 1e-12)
  expect_equal(out$region, targets$region)
})

test_that("normalizeDiscreteTargetSubset warns and normalizes affected entries", {
  subset.targets <- list(
    gender = list(
      region = list(
        North = c(M = 0.2, F = 0.7),
        South = c(M = 0.6, F = 0.4)
      )
    )
  )

  expect_warning({
    out <- normalizeDiscreteTargetSubset(subset.targets, tol = 1e-4)
  }, "Normalized discrete.target.subset")

  expect_equal(sum(out$gender$region$North), 1, tolerance = 1e-12)
  expect_equal(out$gender$region$South, subset.targets$gender$region$South)
})

test_that("normalizeDiscreteTargetSubset errors on over-sums", {
  bad.subset <- list(
    gender = list(
      region = list(
        North = c(M = 0.8, F = 0.4)
      )
    )
  )

  expect_error(
    normalizeDiscreteTargetSubset(bad.subset, tol = 1e-4),
    "sum to more than 1"
  )
})


test_that("weightByDiscrete applies marginal ratios", {
  df <- data.frame(
    x = factor(c("A", "B", "A"), levels = c("A", "B")),
    weights = c(1, 1, 1)
  )
  targets <- list(x = c(A = 0.5, B = 0.5))
  out <- weightByDiscrete(
    sample = df,
    var = "x",
    init.weight = c(1, 1, 1),
    discrete.targets = targets,
    max.weights = 10,
    min.weights = 0.1,
    cap.every.var = FALSE,
    current.levels = levels(df$x)
  )
  expect_equal(unname(out), c(0.75, 1.5, 0.75))
})


test_that("weightByDiscreteSubset returns finite weights", {
  df <- data.frame(
    gender = factor(c("M", "F", "M", "F"), levels = c("M", "F")),
    region = factor(c("North", "North", "South", "South"), levels = c("North", "South")),
    unique.id = 1:4,
    weights = rep(1, 4)
  )

  discrete.sub <- list(
    region = list(
      North = c(M = 0.4, F = 0.6),
      South = c(M = 0.6, F = 0.4)
    )
  )

  out <- weightByDiscreteSubset(
    sample = df,
    var = "gender",
    discrete.sub = discrete.sub,
    max.weights = 10,
    min.weights = 0.1,
    cap.every.var = FALSE,
    current.levels = levels(df$gender)
  )

  expect_length(out, 4)
  expect_true(all(is.finite(out)))
})


test_that("checkDiscrete currently errors due to missing discrete.levels", {
  df <- data.frame(x = factor(c("A", "B")), w = c(1, 1))
  targets <- list(x = c(A = 0.5, B = 0.5))
  expect_error(checkDiscrete(targets, df, weights = "w"))
})

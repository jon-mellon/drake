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

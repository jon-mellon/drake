library(testthat)


test_that("regularizeValuesSlim sorts x", {
  out <- regularizeValuesSlim(c(3, 1, 2), c(30, 10, 20), ties = mean)
  expect_equal(out$x, c(1, 2, 3))
  expect_equal(out$y, c(10, 20, 30))
})


test_that("approxSlim interpolates values", {
  out <- approxSlim(x = 1:3, y = 1:3, xout = c(1.5, 2.5))
  expect_equal(out, c(1.5, 2.5), tolerance = 1e-6)
})


test_that("densitySlim returns density object", {
  x <- rnorm(50)
  out <- densitySlim(x, n = 64, from = -3, to = 3, weights = rep(1, length(x)), bw = 1)
  expect_true(inherits(out, "density"))
  expect_equal(length(out$x), 64)
})


test_that("continuous helpers run with densitySlim", {
  df <- data.frame(x = c(1, 2, 3, 4), weights = c(1, 1, 1, 1), unique.id = 1:4)
  target <- density(df$x, n = 10)

  supp <- createContinuousSupplement(df, "x", target)
  expect_true(is.list(supp))

  w1 <- weightContinuousOnce(df, "x", target, dens.matches = supp[["x"]])
  expect_length(w1, nrow(df))
  expect_true(all(is.finite(w1)))

  w2 <- weightByContinuous(
    sample = df,
    var = "x",
    con.target = target,
    max.weights = 10,
    min.weights = 0.1,
    cap.every.var = FALSE,
    con.supp = supp
  )
  expect_length(w2, nrow(df))

  diff <- checkOneContinuous(df, "x", target, weights = "weights")
  expect_true(is.finite(diff))
})


test_that("checkContinuous works with stratified targets", {
  df <- data.frame(
    x = c(1, 2, 3, 4),
    group = factor(c("A", "A", "B", "B")),
    weights = c(1, 1, 1, 1)
  )

  target <- list(
    group = list(
      A = density(df$x[df$group == "A"], n = 10),
      B = density(df$x[df$group == "B"], n = 10)
    )
  )

  diff <- checkContinuous(df, "x", target, weights = "weights")
  expect_true(is.finite(diff))
})

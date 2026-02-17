library(testthat)


test_that("createContinuousTargets returns density list", {
  vars <- list(a = rnorm(20), b = rnorm(30))
  targets <- createContinuousTargets(vars, max = 5, min = -5, bw = 1)
  expect_true(is.list(targets))
  expect_true(all(vapply(targets, inherits, logical(1), "density")))
})


test_that("labelDataset normalizes label classes", {
  df <- data.frame(x = c(1, 2, 3))
  attr(df$x, "labels") <- structure(c(one = 1), class = "character")
  out <- labelDataset(df)
  expect_true(class(attr(out$x, "labels")) %in% c(typeof(out$x), "numeric"))
})


test_that("s2arraySlim and sapSlim simplify results", {
  lst <- list(c(1, 2), c(3, 4))
  out <- s2arraySlim(lst, higher = TRUE)
  expect_equal(dim(out), c(2, 2))

  out2 <- sapSlim(list(1:2, 3:4), sum)
  expect_equal(out2, c(3, 7))
})


test_that("tapSlim splits by factor", {
  x <- c(1, 2, 3, 4)
  idx <- factor(c("A", "A", "B", "B"))
  out <- tapSlim(x, idx, sum)
  expect_equal(out, c(A = 3, B = 7))
})


test_that("rakeTable matches margins approximately", {
  mat <- matrix(c(10, 20, 30, 40), nrow = 2, byrow = TRUE)
  col.targets <- c(50, 50)
  row.targets <- c(40, 60)
  out <- rakeTable(mat, col.targets, row.targets, iterations = 50)
  expect_equal(rowSums(out), row.targets, tolerance = 1e-6)
  expect_equal(colSums(out), col.targets, tolerance = 1e-6)
})

library(testthat)


test_that("drake produces weights for discrete targets", {
  w <- drake(
    sample = sample_small,
    continuous.targets = NULL,
    discrete.targets = discrete_targets,
    maxit = 50,
    check.convergence.every = 5
  )

  expect_type(w, "double")
  expect_length(w, nrow(sample_small))
  expect_true(all(is.finite(w)))
  expect_equal(sum(w), nrow(sample_small), tolerance = 1e-6)
})


test_that("drake respects subset and errors on invalid targets", {
  expect_warning({
    w <- drake(
      sample = sample_small,
      continuous.targets = NULL,
      discrete.targets = discrete_targets,
      subset = c(TRUE, TRUE, FALSE, TRUE),
      maxit = 200,
      check.convergence.every = 1
    )
  }, NA)
  expect_true(is.na(w[3]))
  expect_equal(sum(w, na.rm = TRUE), 3, tolerance = 1e-6)

  bad_targets <- list(gender = c(M = 0.7, F = 0.6))
  expect_error(
    drake(sample_small, continuous.targets = NULL, discrete.targets = bad_targets),
    "sum to more than 1"
  )
})

test_that("drake converges with longer maxit", {
  expect_warning({
    w <- drake(
      sample = sample_small,
      continuous.targets = NULL,
      discrete.targets = discrete_targets,
      maxit = 200,
      check.convergence.every = 1
    )
  }, NA)

  expect_length(w, nrow(sample_small))
  expect_true(all(is.finite(w)))
})


test_that("drakeClose returns weights", {
  expect_warning({
    w <- drakeClose(
      sample = sample_small,
      continuous.targets = list(),
      discrete.targets = discrete_targets,
      maxit = 200,
      max.discrete.diff = 0.1,
      max.con.diff = 0.1
    )
  }, NA)
  expect_length(w, nrow(sample_small))
  expect_true(all(is.finite(w)))
})

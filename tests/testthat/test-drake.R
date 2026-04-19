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

test_that("drake warns and normalizes under-summing top-level targets", {
  under_targets <- list(gender = c(M = 0.2, F = 0.7))

  expect_warning({
    w <- drake(
      sample = sample_small,
      continuous.targets = NULL,
      discrete.targets = under_targets,
      maxit = 200,
      check.convergence.every = 1
    )
  }, "Normalized discrete.targets")

  observed <- prop.table(tapply(w, sample_small$gender, sum))
  expected <- under_targets$gender / sum(under_targets$gender)

  expect_equal(
    as.numeric(observed[names(expected)]),
    as.numeric(expected),
    tolerance = 1e-6
  )
})

test_that("drake does not warn for top-level sums within tolerance", {
  near_targets <- list(gender = c(M = 0.5, F = 0.49995))

  expect_warning({
    w <- drake(
      sample = sample_small,
      continuous.targets = NULL,
      discrete.targets = near_targets,
      maxit = 200,
      check.convergence.every = 1
    )
  }, NA)

  expect_true(all(is.finite(w)))
})

test_that("drake warns and normalizes under-summing subset targets", {
  sample_subset <- data.frame(
    gender = factor(c("M", "F", "M", "F"), levels = c("M", "F")),
    region = factor(c("North", "North", "South", "South"), levels = c("North", "South"))
  )

  subset_targets <- list(
    gender = list(
      region = list(
        North = c(M = 0.2, F = 0.7),
        South = c(M = 0.6, F = 0.4)
      )
    )
  )

  expect_warning({
    w <- drake(
      sample = sample_subset,
      continuous.targets = NULL,
      discrete.targets = list(region = c(North = 0.5, South = 0.5)),
      discrete.target.subset = subset_targets,
      maxit = 200,
      check.convergence.every = 1
    )
  }, "Normalized discrete.target.subset")

  north <- sample_subset$region == "North"
  observed.north <- prop.table(tapply(w[north], sample_subset$gender[north], sum))
  expected.north <- subset_targets$gender$region$North /
    sum(subset_targets$gender$region$North)

  expect_equal(
    as.numeric(observed.north[names(expected.north)]),
    as.numeric(expected.north),
    tolerance = 1e-6
  )
})

test_that("drake errors for over-summing subset targets", {
  sample_subset <- data.frame(
    gender = factor(c("M", "F", "M", "F"), levels = c("M", "F")),
    region = factor(c("North", "North", "South", "South"), levels = c("North", "South"))
  )

  bad_subset_targets <- list(
    gender = list(
      region = list(
        North = c(M = 0.8, F = 0.4),
        South = c(M = 0.6, F = 0.4)
      )
    )
  )

  expect_error(
    drake(
      sample = sample_subset,
      continuous.targets = NULL,
      discrete.targets = list(region = c(North = 0.5, South = 0.5)),
      discrete.target.subset = bad_subset_targets
    ),
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

test_that("drake checks mean-target convergence instead of running to maxit", {
  target.mean <- mean(sample_small$age) + 2

  expect_warning({
    w <- drake(
      sample = sample_small,
      continuous.targets = NULL,
      discrete.targets = list(),
      mean.targets = list(age = target.mean),
      maxit = 50,
      check.convergence.every = 1
    )
  }, NA)

  achieved.mean <- stats::weighted.mean(sample_small$age, w)
  expect_equal(achieved.mean, target.mean, tolerance = 1e-3)
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

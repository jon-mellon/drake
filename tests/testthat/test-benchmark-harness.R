library(testthat)


test_that("drake benchmark cases cover the expected target types", {
  cases <- drakeBenchmarkCases(profile = "quick", seed = 123)

  expect_true(all(c(
    "discrete_only",
    "continuous_only",
    "mean_only",
    "discrete_subset",
    "mixed_targets",
    "stratified_continuous",
    "selection_weighted"
  ) %in% names(cases)))

  tags <- lapply(cases, function(x) x$tags)
  expect_true(any(vapply(tags, function(x) "discrete" %in% x, logical(1))))
  expect_true(any(vapply(tags, function(x) "continuous" %in% x, logical(1))))
  expect_true(any(vapply(tags, function(x) "mean" %in% x, logical(1))))
  expect_true(any(vapply(tags, function(x) "subset" %in% x, logical(1))))
  expect_true(any(vapply(tags, function(x) "selection" %in% x, logical(1))))
})


test_that("drake benchmark suite returns timing and outcome summaries", {
  cases <- drakeBenchmarkCases(profile = "quick", seed = 123)

  suite <- runDrakeBenchmarkSuite(
    cases = cases,
    case.names = c("discrete_only", "mixed_targets"),
    repetitions = 1,
    warmup = 0,
    gc.first = FALSE
  )

  expect_s3_class(suite, "drakeBenchmarkSuite")
  expect_equal(nrow(suite$summary), 2)
  expect_true(all(suite$summary$status == "success"))
  expect_true(all(is.finite(suite$summary$median_time_sec)))
  expect_true(all(suite$summary$finite_weights))
  expect_true(all(suite$summary$sum_error < 1e-6))
  expect_true(all(is.finite(suite$summary$efficiency_pct)))

  comparison <- compareDrakeBenchmarkSuites(suite, suite)
  expect_false(any(comparison$correctness_regression))
})

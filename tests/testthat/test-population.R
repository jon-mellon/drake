library(testthat)


test_that("getMortCountries returns mapping", {
  mapping <- getMortCountries()
  expect_true(is.character(mapping))
  expect_true("US" %in% names(mapping))
})


test_that("indexMortality returns expected structure", {
  out <- indexMortality(
    mortality.table = mortality_table_small,
    index.year = 2010,
    valid.years = 2008:2010,
    elec.gap = 2,
    noisy = FALSE
  )
  expect_true(all(c("mortality", "gender", "yob") %in% names(out)))
  expect_equal(length(unique(out$gender)), 3)
})


test_that("calculateMortality2 returns NA for unsupported countries", {
  df <- data.frame(age = c(30, 40), gender = c("Male", "Female"))
  mort.countries <- c(BR = NA)
  expect_warning(
    out <- calculateMortality2(
      data = df,
      country = "BR",
      elec.gap = 5,
      suffix = "",
      elec1.year = 2015,
      hmd.user = "",
      hmd.pw = "",
      mort.countries = mort.countries,
      noisy = FALSE
    ),
    "No mortality data"
  )
  expect_true(all(is.na(out)))
})


test_that("getExpectedMortality returns NA when mortality unavailable", {
  rate <- getExpectedMortality(
    country = "BR",
    date1 = as.Date("2020-01-01"),
    date2 = as.Date("2021-01-01"),
    min.age = 18,
    pops = pops_small,
    hmd.user = "",
    hmd.pw = "",
    noisy = FALSE
  )
  expect_true(is.na(rate))
})


test_that("calcAgeDensity2 produces density", {
  dens <- calcAgeDensity2(
    country = "BR",
    date = as.Date("2020-06-01"),
    bw.mult = 1.5,
    min.age = 18,
    max.age = 22,
    pops = pops_small,
    survey.n = 100
  )
  expect_true(inherits(dens, "density"))
  expect_true(min(dens$x) <= 18)
  expect_true(max(dens$x) >= 22)
})


test_that("calcPropNewEligUN2 returns proportion", {
  prop <- calcPropNewEligUN2(
    country = "BR",
    prev.date = as.Date("2019-01-01"),
    current.date = as.Date("2020-01-01"),
    min.age.current = 18,
    min.age.prev = 18,
    pops = pops_small
  )
  expect_true(is.numeric(prop))
  expect_true(prop >= 0 && prop <= 1)
})


test_that("getAgeDensities builds stratified targets", {
  df <- data.frame(
    age = rnorm(50, 45, 10),
    turnout = sample(c("Voted", "DNV"), 50, replace = TRUE),
    w = runif(50)
  )
  out <- getAgeDensities(df, weight.var = "w", var.suffix = "", target.suffix = "")
  expect_true(is.list(out))
  expect_true(length(out) == 1)
})


test_that("ageDensityUNWPP returns density", {
  dens <- ageDensityUNWPP(
    year = 2020,
    country = "BR",
    min.age = 18,
    max.age = 22,
    data.age.vec = sample(18:22, 50, replace = TRUE),
    pops = pops_small,
    bw.mult = 1.2
  )
  expect_true(inherits(dens, "density"))
})


test_that("popBySingleYear validates source argument", {
  expect_error(popBySingleYear(source = "bad", country = "BR"))
})

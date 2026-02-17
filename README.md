# drake

Density raking for survey weighting with discrete, continuous, and mean targets.

The intent of `drake` is to generate stable survey weights that align a sample to
known population margins while preserving continuous distributions (e.g., age)
using kernel density targets. The implementation emphasizes speed by caching
expensive density lookups and using a C++ root-finder for mean targets.

## Installation

```r
# install.packages("remotes")
remotes::install_github("jon-mellon/drake@main")
```

## Core Concepts

`drake()` expects:

- A `sample` data frame with all variables referenced by targets.
- `discrete.targets`: named vectors of target proportions for categorical
  variables.
- `continuous.targets`: density objects (or stratified lists of densities) for
  continuous variables.
- Optional `mean.targets`: target means for numeric variables.

Weights are iteratively adjusted to satisfy these constraints while applying
upper and lower caps to control extreme values.

## Minimal Example

```r
set.seed(1)

n <- 500
sample <- data.frame(
  age = rnorm(n, 45, 12),
  gender = sample(c("M", "F"), n, replace = TRUE),
  region = sample(c("North", "South"), n, replace = TRUE)
)

continuous.targets <- list(
  age = density(rnorm(5000, 44, 10))
)

discrete.targets <- list(
  gender = c(M = 0.48, F = 0.52),
  region = c(North = 0.4, South = 0.6)
)

weights <- drake(sample, continuous.targets, discrete.targets, maxit = 50)

round(prop.table(tapply(weights, sample$gender, sum)), 3)
round(prop.table(tapply(weights, sample$region, sum)), 3)
```

## Target Structures

Discrete targets

- A named list where each element is a named numeric vector.
- Names must match the levels in the sample.
- Values should sum to 1 (small rounding differences are allowed).

Continuous targets

- A named list where each element is a `density` object with `x` and `y`.
- For stratified raking, provide a nested list keyed by the stratifying
  variable and its levels.

Mean targets

- A named list of numeric means for variables in `sample`.

## Implementation Notes

The raking loop proceeds as follows:

1. Continuous targets are matched using the ratio of target density to the
   current weighted density. The density grid index mapping is cached to avoid
   repeated nearest-neighbor searches.
2. Discrete subset targets (if provided) are applied within each stratum.
3. Discrete margins are updated using iterative proportional fitting.
4. Mean targets are adjusted using a monotone weight transformation solved by
   a C++ root-finder.

After each full iteration, weights are rescaled to sum to the number of valid
rows and optionally capped. Convergence checks run every
`check.convergence.every` iterations.

## Mortality and Population Utilities

The package includes helper functions for population and mortality workflows:

- `popBySingleYear()` pulls population-by-age from UN WPP or HMD.
- `indexMortality()` and `calculateMortality2()` compute mortality indices from
  HMD tables.
- `getExpectedMortality()` estimates expected mortality between two dates.
- `calcAgeDensity2()` and `ageDensityUNWPP()` build age density targets.

These functions require access to external data sources and, for HMD, valid
credentials.

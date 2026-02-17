# Potential Issues

Review scope: static inspection of the current codebase only (no execution or tests).

**High**
- `ageDensityUNWPP()` constructs a full-population age vector by repeating each person. For real populations this can be tens of millions of entries and can exhaust memory. `R/drakehelpers.R`

**Medium**
- `discrete.target.subset` stratification variables can be dropped before use. The initial column subset only includes target variables, not the stratifying variable, which can cause a false “not in data” error even when the original sample had the column. `R/drake.R`
- Target proportions that sum to less than 1 are silently renormalized after the global weight rescale. This can make results differ from the supplied targets without warning. Only sums greater than 1 are rejected. `R/drake.R`, `R/drakehelpers.R`
- `initial.weights` and `subset` lengths are not validated. R will recycle shorter vectors silently, which can misalign weights and rows. `R/drake.R`
- `popBySingleYear(source = "hmd")` does not guard against unsupported countries. If the ISO2 code maps to `NA`, the function will call `readHMDweb()` with an invalid country code and error. `R/popBySingleYear.R`
- `calcPropNewEligUN2()` uses `approxfun()` without `rule = 2`. If `elig.time` falls outside the observed range, it will return `NA` rather than extrapolating or clamping. `R/calcPropNewEligUN2.R`
- Several helpers call internal, non-exported C routines (`.Internal`, `.External2`, `stats:::`). These are not part of R’s stable API and can break across R releases. `R/drakehelpers.R`

**Low**
- `selection.weights` is used as a logical flag in the argument list, but is overwritten with a numeric weight vector internally. This prevents passing a separate baseline vector and can be confusing for users. `R/drake.R`
- Zero initial weights are converted to `NA` and the corresponding rows are dropped from raking entirely. This is a strong assumption and may surprise users expecting explicit zero weights. `R/drake.R`
- `checkDiscrete()` references `discrete.levels`, which is not in scope, so the function errors if called. `R/drakehelpers.R`
- `tapSlim()` assumes `INDEX` is a factor but does not enforce it; `levels()` will be `NULL` for non-factor inputs and can lead to incorrect behavior. `R/tapSlim.R`
- `rakeTable()` only guards against `Inf` ratios. If both target and current margins are zero, `NaN` can propagate into the table. `R/rakeTable.R`
- `createContinuousTargets()` uses `n = length(min:max)`. Large or non-integer ranges can produce huge density grids or unintended lengths. `R/createContinuousTargets.R`
- `calcAgeDensity2()` uses rounding to allocate `survey.n`, so the synthetic age vector length may not equal `survey.n` exactly. `R/calcAgeDensity2.R`
- The country mapping includes `IL = "ISL"`, which appears to map Israel to Iceland’s HMD code. If unintentional, this yields incorrect mortality lookups. `R/getMortCountries.R`
- `densitySlim()` passes weights to `stats:::C_BinDist` without explicit validation. If `weights` is `NULL` or length-mismatched, this will error. `R/drakehelpers.R`
- `drakeClose()` prints the current max weight each iteration, which can be noisy in batch runs or tests. `R/drakeIdeal.R`

**Info**
- Compiled artifacts (`src/*.o`, `src/*.dll`) are tracked in the repo. These can cause portability issues across platforms and stale binaries during development. `src/`

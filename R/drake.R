#' Density raking with discrete, continuous, and mean targets
#'
#' @description
#' Adjusts survey weights so the weighted sample matches a set of
#' discrete proportions, continuous density targets, and/or mean targets.
#' This is the primary entry point for the density raking (drake) algorithm.
#'
#' @details
#' The algorithm iteratively re-weights the sample in a fixed sequence:
#'
#' 1. Continuous targets: weights are adjusted by the ratio of target
#'    densities to the current weighted density (kernel density estimate).
#'    A cached index mapping is used to avoid repeated nearest-neighbor
#'    searches on the density grid.
#' 2. Discrete subset targets: discrete margins that apply within a
#'    stratifying variable are adjusted next.
#' 3. Discrete targets: standard raking against marginal proportions.
#' 4. Mean targets: a monotone weight adjustment is solved using a
#'    root-finding routine (C++ implementation for speed).
#'
#' Discrete target vectors are validated before iteration. Vectors summing
#' above \code{1 + 1e-4} are rejected. Vectors summing below \code{1 - 1e-4}
#' are explicitly normalized to sum to 1 and trigger warnings listing affected
#' targets.
#'
#' After each full pass, weights are re-scaled to sum to the number of
#' valid rows. Optional capping constrains extreme weights. Convergence is
#' checked every \code{check.convergence.every} iterations by comparing
#' current weighted margins/densities to targets.
#'
#' @param sample A data frame containing the variables referenced by targets.
#' @param continuous.targets Optional list of continuous targets. Each element
#'   is either a \code{density} object or a nested list of \code{density}
#'   objects indexed by a stratifying variable.
#' @param discrete.targets Named list of numeric vectors giving target
#'   proportions for discrete variables. Each vector must be named with the
#'   target levels. Sums above \code{1 + 1e-4} error; sums below
#'   \code{1 - 1e-4} are normalized to sum to 1 with a warning.
#' @param discrete.target.subset Optional list specifying discrete targets
#'   within a stratum. Structure: \code{list(target_var = list(strata_var =
#'   list(level = c(target_level = proportion, ...), ...)))}. The same
#'   sum-validation/normalization rule as \code{discrete.targets} is applied
#'   to each stratum vector.
#' @param mean.targets Optional named list of target means for numeric
#'   variables.
#' @param max.weights Maximum allowed weight value (applied after each pass).
#' @param min.weights Minimum allowed weight value (applied after each pass).
#' @param maxit Maximum number of iterations.
#' @param initial.weights Optional initial weights (length \code{nrow(sample)}).
#' @param max.discrete.diff Convergence threshold for discrete targets.
#' @param max.mean.diff Convergence threshold for mean targets.
#' @param max.con.diff Convergence threshold for continuous targets.
#' @param subset Logical vector indicating which rows are eligible for raking.
#'   Non-eligible rows return \code{NA} weights.
#' @param debug If \code{TRUE}, enter the debugger at the start.
#' @param cap.every.var If \code{TRUE}, applies weight caps after each variable.
#' @param check.convergence.every Frequency (iterations) to recompute
#'   convergence diagnostics.
#' @param extreme.weight.warning Proportion threshold for warnings when weights
#'   approach caps.
#' @param RR Optional lower bound for the ratio of final weights to
#'   \code{selection.weights}. Used only when \code{selection.weights} is
#'   \code{TRUE}.
#' @param selection.weights If \code{TRUE}, uses the initial weights as a
#'   baseline and enforces the \code{RR} lower bound on the weight ratio.
#'
#' @return A numeric vector of final weights aligned to the original
#'   \code{sample} order.
#' @examples
#' set.seed(1)
#' n <- 500
#' sample <- data.frame(
#'   age = rnorm(n, 45, 12),
#'   gender = sample(c("M", "F"), n, replace = TRUE),
#'   region = sample(c("North", "South"), n, replace = TRUE)
#' )
#' continuous.targets <- list(
#'   age = density(rnorm(5000, 44, 10))
#' )
#' discrete.targets <- list(
#'   gender = c(M = 0.48, F = 0.52),
#'   region = c(North = 0.4, South = 0.6)
#' )
#' w <- drake(sample, continuous.targets, discrete.targets, maxit = 50)
#' round(prop.table(tapply(w, sample$gender, sum)), 3)
#' @export
drake <- function(sample, continuous.targets = NULL, discrete.targets,
                  discrete.target.subset = NULL,
                  mean.targets = NULL,
                  max.weights = 25, min.weights = 1/max.weights,
                  maxit = 1000, initial.weights = rep(1, nrow(sample)),
                  max.discrete.diff = 0.0005,
                  max.mean.diff = 0.001,
                  max.con.diff = 0.01,
                  subset = rep(TRUE, nrow(sample)),
                  debug = FALSE,
                  cap.every.var = FALSE,
                  check.convergence.every = 100,
                  extreme.weight.warning = 0.01,
                  RR = NULL,
                  selection.weights = FALSE) {
  min.cap <- !is.null(RR) && selection.weights

  if(debug) {
    browser()
  }
  prepared <- prepareDrakeInputs(
    sample = sample,
    continuous.targets = continuous.targets,
    discrete.targets = discrete.targets,
    discrete.target.subset = discrete.target.subset,
    mean.targets = mean.targets,
    initial.weights = initial.weights,
    subset = subset
  )

  sample <- prepared$sample
  weights <- prepared$weights_start
  selection.base.weights <- prepared$selection.base.weights
  n.original <- prepared$n.original
  valid.idx <- prepared$valid.idx
  continuous.targets <- prepared$continuous.targets
  discrete.targets <- prepared$discrete.targets
  discrete.target.subset <- prepared$discrete.target.subset
  mean.targets <- prepared$mean.targets
  continuous.names <- prepared$continuous.names
  discrete.names <- prepared$discrete.names
  mean.names <- prepared$mean.names
  discrete.codes <- prepared$discrete.codes
  discrete.code.list <- prepared$discrete.code.list
  discrete.target.list <- prepared$discrete.target.list
  subset.target.code.list <- prepared$subset.target.code.list
  subset.strata.code.list <- prepared$subset.strata.code.list
  subset.target.matrix.list <- prepared$subset.target.matrix.list
  continuous.supplement <- prepared$continuous.supplement

  current.discrete.diff <- if(length(discrete.targets) > 0L) max.discrete.diff + 1 else 0
  current.con.diff <- if(length(continuous.targets) > 0L) max.con.diff + 1 else 0
  current.mean.diff <- if(length(mean.targets) > 0L) max.mean.diff + 1 else 0

  ii <- 1L
  tot.obs <- prepared$tot.obs

  while(ii < maxit &&
        ((current.discrete.diff > max.discrete.diff) ||
         (current.con.diff > max.con.diff) ||
         (current.mean.diff > max.mean.diff))) {

    ii <- ii + 1L
    for(var in continuous.names) {
      weights <- weightByContinuous(weights = weights, var = var, sample = sample, 
                                    con.target = continuous.targets[[var]], 
                                    max.weights = max.weights, 
                                    min.weights = min.weights, 
                                    cap.every.var = cap.every.var,
                                    con.supp = continuous.supplement[[var]])
    }

    if(length(subset.target.code.list) > 0L) {
      weights <- CWeightByDiscreteSubsetMany(
        target_code_list = subset.target.code.list,
        strata_code_list = subset.strata.code.list,
        weights = weights,
        targets_by_strata_list = subset.target.matrix.list,
        cap_every_var = cap.every.var,
        max_weight = max.weights,
        min_weight = min.weights
      )
    }

    if(length(discrete.code.list) > 0L) {
      weights <- CWeightByDiscreteMany(
        codes_list = discrete.code.list,
        weights = weights,
        targets_list = discrete.target.list,
        cap_every_var = cap.every.var,
        max_weight = max.weights,
        min_weight = min.weights
      )
    }

    for(var in mean.names) {
      weights <- CWeightByMeanLinear(weight = weights, 
                                     var = sample[[var]], 
                                     meantarget = mean.targets[[var]])
    }

    weights <- weights * (tot.obs / sum(weights))

    if(min.cap) {
      ratio <- weights / selection.base.weights
      ratio.bar <- mean(ratio)
      ratio.star <- ratio / ratio.bar
      too.low <- ratio.star<RR
      ratio.star[too.low] <- RR
      ratio.corrected <- ratio.star  * ratio.bar
      weights.corrected <- ratio.corrected  * selection.base.weights
      total.weight.added <- sum(weights.corrected - weights)
      other.weight.sum <- sum(weights[!too.low])
      corrected.other.weight.sum <- other.weight.sum - total.weight.added
      other.weight.correction <- corrected.other.weight.sum / other.weight.sum
      weights[!too.low] <- weights[!too.low] * other.weight.correction
      weights[too.low] <- weights.corrected[too.low]
    }

    weights <- clampWeights(weights, max.weights = max.weights, min.weights = min.weights)

    if((ii %% check.convergence.every) == 0L)  {
      if(length(continuous.targets) != 0L) {
        current.con.diff <- rep(NA_real_, length(continuous.names))
        names(current.con.diff) <- continuous.names
        for(con.t in continuous.names) {
          current.con.diff[con.t] <- checkContinuousPrepared(
            weights = weights,
            con.target = continuous.targets[[con.t]],
            con.supp = continuous.supplement[[con.t]]
          )
        }
        current.con.diff <- max(current.con.diff)
      }

      if(length(discrete.targets) != 0L) {
        current.discrete.diff <- CMaxAbsDiscreteDiffMany(discrete.code.list, weights, discrete.target.list)
      }

      if(length(mean.targets) != 0L) {
        current.mean.diff <- maxMeanTargetDiff(sample = sample, weights = weights, mean.targets = mean.targets)
      }
    }
  }

  if(ii == maxit &&
     ((current.discrete.diff > max.discrete.diff) ||
      (current.con.diff > max.con.diff) ||
      (current.mean.diff > max.mean.diff))) {
    warning("Maximum iterations reached without convergence.")
  }

  final.weights <- as.vector(weights / mean(weights))

  output.weights <- rep(NA_real_, n.original)
  output.weights[valid.idx] <- final.weights

  high.weight.share <- mean((final.weights / max.weights) > 0.98)
  low.weight.share <- mean((final.weights / min.weights) < 1.02)

  if(low.weight.share > extreme.weight.warning) {
    warning(low.weight.share * 100, "% of weights are close to lower weight limit")
  }
  if(high.weight.share > extreme.weight.warning) {
    warning(high.weight.share * 100, "% of weights are close to higher weight limit")
  }
  return(output.weights)
}

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
  discrete.targets <- normalizeDiscreteTargets(discrete.targets, tol = 1e-4)
  discrete.target.subset <- normalizeDiscreteTargetSubset(discrete.target.subset, tol = 1e-4)

  sample <- as.data.frame(sample, stringsAsFactors = FALSE)
  n.original <- nrow(sample)

  if(length(initial.weights) == 1L) {
    initial.weights <- rep(initial.weights, n.original)
  }
  if(length(subset) == 1L) {
    subset <- rep(subset, n.original)
  }
  if(length(initial.weights) != n.original) {
    stop("initial.weights must have length 1 or nrow(sample).")
  }
  if(length(subset) != n.original) {
    stop("subset must have length 1 or nrow(sample).")
  }

  var.names.cont <- names(continuous.targets)
  var.names.discrete <- names(discrete.targets)
  var.names.cont2 <- unlist(lapply(continuous.targets, function(x) names(x)))
  var.names.mean <- names(mean.targets)
  var.names.discrete.sub <- names(discrete.target.subset)

  if(any(var.names.cont2 %in% c("data.name", "bw"))) {
    var.names.cont2 <- NULL
  }
  var.names.comb <- unique(c(var.names.cont, var.names.discrete, var.names.cont2, var.names.mean))

  initial.weights[initial.weights == 0] <- NA

  sample <- sample[, unique(c(var.names.comb, var.names.discrete.sub)), drop = FALSE]

  valid.cases <- complete.cases(sample[, var.names.comb, drop = FALSE]) &
    subset & !is.na(initial.weights)
  valid.cases2 <- rep(TRUE, nrow(sample))

  for(kk in var.names.discrete.sub)   {
    strata <- names(discrete.target.subset[[kk]])
    for(strt in strata) {
      strt.parts <- names(discrete.target.subset[[kk]][[strt]])
      for(str.single in strt.parts) {
        valid.cases2[sample[, strt]==str.single & is.na(sample[, kk])] <- FALSE
      }
    }
  }
  valid.cases <- valid.cases & valid.cases2

  if(!any(valid.cases)) {
    stop("No valid cases remain after applying filters.")
  }

  valid.idx <- which(valid.cases)
  sample <- sample[valid.idx, , drop = FALSE]
  weights <- as.numeric(initial.weights[valid.idx])

  for(var in names(discrete.targets)) {
    discrete.targets <- fixDiscreteOrder(sample, var, discrete.targets)  
  }

  discrete.levels <- list()
  discrete.codes <- list()
  
  discrete.vars <- names(discrete.targets)
  if(!is.null(discrete.target.subset)) {
    dts.names <- names(discrete.target.subset)
    dts.names <- c(dts.names, unlist(lapply(dts.names, function(x) names(discrete.target.subset[[x]]))))
    discrete.vars <- unique(c(discrete.vars, dts.names))
  }
  
  if(any(!discrete.vars %in% colnames(sample))) {
    stop("Discrete var targets not in data: ", 
         paste(discrete.vars[!discrete.vars %in% colnames(sample)], collapse = ";"))
  }
  
  for(var in discrete.vars) {
    column <- sample[[var]]

    if(var %in% names(discrete.targets) && !is.null(names(discrete.targets[[var]]))) {
      target.levels <- names(discrete.targets[[var]])
    } else if(var %in% names(discrete.target.subset)) {
      target.levels <- unique(unlist(lapply(discrete.target.subset[[var]][[1]], names)))
    } else if(is.factor(column)) {
      target.levels <- levels(column)
    } else {
      target.levels <- sort(unique(as.character(column)))
    }

    if(is.numeric(column)) {
      column <- factor(as.character(column), levels = target.levels)
    } else {
      column <- factor(as.character(column), levels = target.levels)
    }

    sample[[var]] <- column
    discrete.levels[[var]] <- levels(column)
    discrete.codes[[var]] <- as.integer(column)
  }

  subset.target.matrices <- list()
  if(!is.null(discrete.target.subset)) {
    for(var in names(discrete.target.subset)) {
      subset.target.matrices[[var]] <- list()
      for(strata.var in names(discrete.target.subset[[var]])) {
        subset.target.matrices[[var]][[strata.var]] <- buildDiscreteSubsetTargetMatrix(
          discrete.sub = discrete.target.subset[[var]][[strata.var]],
          target.levels = discrete.levels[[var]],
          strata.levels = discrete.levels[[strata.var]]
        )
      }
    }
  }

  sample[, "weights"] <- weights
  continuous.supplement <- list()
  for(var in names(continuous.targets)) {
    continuous.supplement[[var]]  <- createContinuousSupplement(sample = sample, 
                                                                var = var, 
                                                                con.target = continuous.targets[[var]])
  }

  weights <- (weights * nrow(sample)) / sum(weights)
  selection.base.weights <- weights

  current.discrete.diff <- if(length(discrete.targets) > 0L) max.discrete.diff + 1 else 0
  current.con.diff <- if(length(continuous.targets) > 0L) max.con.diff + 1 else 0
  current.mean.diff <- if(length(mean.targets) > 0L) max.mean.diff + 1 else 0

  ii <- 1L
  tot.obs <- nrow(sample)

  while(ii < maxit &&
        ((current.discrete.diff > max.discrete.diff) ||
         (current.con.diff > max.con.diff) ||
         (current.mean.diff > max.mean.diff))) {

    ii <- ii + 1L
    sample[, "weights"] <- weights

    for(var in names(continuous.targets)) {
      weights <- weightByContinuous(var = var, sample = sample, 
                                    con.target = continuous.targets[[var]], 
                                    max.weights = max.weights, 
                                    min.weights = min.weights, 
                                    cap.every.var = cap.every.var,
                                    con.supp = continuous.supplement[[var]])
      sample[, "weights"] <- weights
    }

    for(var in names(discrete.target.subset)) {
      for(strata.var in names(discrete.target.subset[[var]])) {
        weights <- CWeightByDiscreteSubsetCodes(
          target_codes = discrete.codes[[var]],
          strata_codes = discrete.codes[[strata.var]],
          weights = weights,
          targets_by_strata = subset.target.matrices[[var]][[strata.var]]
        )
        if(cap.every.var) {
          weights <- clampWeights(weights, max.weights = max.weights, min.weights = min.weights)
        }
      }
    }

    for(var in names(discrete.targets)) {
      weights <- CWeightByDiscreteCodes(
        codes = discrete.codes[[var]],
        weights = weights,
        targets = discrete.targets[[var]]
      )
      if(cap.every.var) {
        weights <- clampWeights(weights, max.weights = max.weights, min.weights = min.weights)
      }
    }

    for(var in names(mean.targets)) {
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
        sample[, "weightprop"] <- weights / sum(weights)
        current.con.diff <- rep(NA_real_, length(names(continuous.targets)))
        names(current.con.diff) <- names(continuous.targets)
        for(con.t in names(continuous.targets)) {
          current.con.diff[con.t] <- checkContinuous(sample = sample, var = con.t, 
                                                     con.target = continuous.targets[[con.t]],
                                                     weights = "weightprop", debug = FALSE)
        }
        current.con.diff <- max(current.con.diff)
      }

      if(length(discrete.targets) != 0L) {
        current.discrete.diff <- max(vapply(names(discrete.targets), function(var) {
          CMaxAbsDiscreteDiff(discrete.codes[[var]], weights, discrete.targets[[var]])
        }, numeric(1)))
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

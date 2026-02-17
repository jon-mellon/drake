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
#'   target levels and sum to 1 (or slightly less due to rounding).
#' @param discrete.target.subset Optional list specifying discrete targets
#'   within a stratum. Structure: \code{list(target_var = list(strata_var =
#'   list(level = c(target_level = proportion, ...), ...)))}.
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
  if(!is.null(RR) && selection.weights) {
    min.cap <- TRUE
  } else {
    min.cap <- FALSE
  }
    
  if(debug) {
    browser()
  }
  tot.weights <- sapply(discrete.targets, sum)
  if(any(tot.weights>1.0001)) {
    stop("Following targets sum to more than 1: ", paste(names(which(tot.weights>1.0001)), collapse = ", "))
  }
  
  sample <- mellonMisc::dtf(sample)
  sample[, "unique.id"] <- 1:nrow(sample)
  sample.bk <- sample
  
  var.names.cont <- names(continuous.targets)
  var.names.discrete <- names(discrete.targets)
  var.names.cont2 <-  unlist(lapply(continuous.targets, function(x) names(x)) ) 
  var.names.mean <- names(mean.targets)
  var.names.discrete.sub <- names(discrete.target.subset)
  
  if(any(var.names.cont2 %in% c("data.name", "bw"))) {
    var.names.cont2 <- NULL
  }
  var.names.comb <- unique(c(var.names.cont, var.names.discrete,  var.names.cont2,var.names.mean,  "unique.id"))
  
  initial.weights[initial.weights==0] <- NA
  
  sample <- sample[, c(var.names.comb, var.names.discrete.sub), drop = FALSE]
  sample[, "weights"] <- initial.weights
  
  valid.cases <- complete.cases(sample[, var.names.comb, drop = FALSE]) &
    subset & !is.na(sample[, "weights"])
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
  
  sample <- sample[valid.cases, ]
  initial.weights <- as.vector(initial.weights[valid.cases])
  
  
  for(var in names(discrete.targets)) {
    discrete.targets <- fixDiscreteOrder(sample, var, discrete.targets)  
  }
  
  weight.change <- 1
  ii <- 1
  
  
  sample[, "weights"] <- (sample[, "weights"] * nrow(sample)) / sum(sample[, "weights"]) 
  
  selection.weights <- sample[, "weights"]
  
  current.discrete.diff <- max.discrete.diff + 1
  if(!is.null(continuous.targets)) {
    current.con.diff <- max.con.diff + 1  
  } else {
    current.con.diff <- 0
  }
  
  if(!is.null(mean.targets)) {
    current.mean.diff <- max.mean.diff + 1
  } else {
    current.mean.diff <- 0
  }
  max.wt <- max.weights / nrow(sample)
  min.wt <- min.weights / nrow(sample)
  discrete.levels <- list()
  
  discrete.vars <- names(discrete.targets)
  if(!is.null(discrete.target.subset)) {
    dts.names <- names(discrete.target.subset)
    dts.names <- c(dts.names, sapply(dts.names, function(x) names(discrete.target.subset[[x]])))
    discrete.vars <- unique(c(discrete.vars, dts.names))
  }
  
  if(any(!discrete.vars %in% colnames(sample))) {
    stop("Discrete var targets not in data: ", 
         paste(discrete.vars[!discrete.vars %in% colnames(sample)], collapse = ";"))
  }
  
  for(var in discrete.vars) {
    if(is.numeric(sample[, var])) {
      sample[, var] <- factor(as.character(sample[, var]), 
                              levels = unique(sample[, var])[order(unique(sample[, var]))])
    }
    
    
    if(is.null(names(discrete.targets[[var]]))) {
      sample[, var] <- factor(sample[, var], levels = unique(unlist(lapply(discrete.target.subset[[var]][[1]], names))))
    } else {
      if(is.character(sample[, var])) {
        sample[, var] <- factor(sample[, var], levels = names(discrete.targets[[var]]))
      }
    }
    discrete.levels[[var]] <- levels(sample[, var])
  }
  
  # Caching a costly matching step
  continuous.supplement <- list()
  for(var in names(continuous.targets)) {
    continuous.supplement[[var]]  <- createContinuousSupplement(sample = sample, 
                                                                var = var, 
                                                                con.target = continuous.targets[[var]])
    
  }
  
  tot.obs <- nrow(sample)
  
  while(ii < maxit & ((current.discrete.diff>max.discrete.diff) |
                      (current.con.diff>max.con.diff) | 
                      current.mean.diff > max.mean.diff )  ) {
    
    ii <- ii + 1
    start.weights <- sample[, "weights"]
    
    
    for(var in names(continuous.targets)) {
      sample[, "weights"] <- weightByContinuous(var = var, sample = sample, 
                                                con.target = continuous.targets[[var]], 
                                                max.weights = max.weights, 
                                                min.weights = min.weights, 
                                                cap.every.var = cap.every.var,
                                                con.supp = continuous.supplement[[var]])
    }
    
    for(var in names(discrete.target.subset)) {
      sample[, "weights"] <- weightByDiscreteSubset(sample = sample, var = var, 
                                                    discrete.sub = discrete.target.subset[[var]], 
                                                    max.weights = max.weights, min.weights = min.weights,
                                                    cap.every.var = cap.every.var, 
                                                    current.levels = discrete.levels[[var]])
    }
    
    for(var in names(discrete.targets)) {
      sample[, "weights"] <- weightByDiscrete(var = var, sample = sample, 
                                              init.weight = as.vector(sample[, "weights"]),
                                              discrete.targets= discrete.targets, 
                                              max.weights = max.weights, 
                                              min.weights = min.weights, 
                                              cap.every.var = cap.every.var,
                                              current.levels = discrete.levels[[var]])
    }
    
    for(var in names(mean.targets)) {
      sample[, "weights"] <-  CWeightByMeanLinear(weight = sample[, "weights"], 
                                                  var = sample[, var], 
                                                  meantarget = mean.targets[[var]])
    }
    
    tot.wt <- sum(sample[, "weights"])
    
    mult <- tot.obs/ tot.wt
    sample[, "weights"] <- sample[, "weights"] * mult
    
    
    if(min.cap) {
      ratio <- sample[, "weights"] / selection.weights
      ratio.bar <- mean(ratio)
      ratio.star <- ratio / ratio.bar
      too.low <- ratio.star<RR
      ratio.star[too.low] <- RR
      ratio.corrected <- ratio.star  * ratio.bar
      weights.corrected <- ratio.corrected  * selection.weights
      total.weight.added <- sum(weights.corrected - sample[, "weights"])
      other.weight.sum <- sum(sample[!too.low, "weights"])
      corrected.other.weight.sum <- other.weight.sum - total.weight.added
      other.weight.correction <- corrected.other.weight.sum / other.weight.sum
      sample[!too.low, "weights"] <- sample[!too.low, "weights"] * other.weight.correction
      sample[too.low, "weights"] <- weights.corrected[too.low]
    }
    
    sample[, "weights"][sample[, "weights"] > (max.weights)] <- max.weights
    sample[, "weights"][sample[, "weights"] < (min.weights)] <- min.weights
    
    # sample[, "weights"] <- prop.table(sample[, "weights"]) 
    # sample[, "weights"][sample[, "weights"] > (max.wt)] <- max.wt
    # sample[, "weights"][sample[, "weights"] < (min.wt)] <- min.wt
    
    
    
    
    if(length(continuous.targets) != 0 &&
       ((ii / check.convergence.every) == round(ii / check.convergence.every)))  {
      current.con.diff <- rep(NA, length(names(continuous.targets)))
      names(current.con.diff) <- names(continuous.targets)
      sample[, "weightprop"] <- prop.table(sample[, "weights"])
      for(con.t in names(continuous.targets)) {
        current.con.diff[con.t] <- checkContinuous(sample = sample, var = con.t, 
                                                   con.target = continuous.targets[[con.t]],
                                                   weights = "weightprop", debug = FALSE)
      }
      current.con.diff <- max(current.con.diff)
    } else {
      current.con.diff <- 0
    }
    
    if(length(discrete.targets) != 0 &&
       ((ii / check.convergence.every) == round(ii / check.convergence.every)))  {
      # current.values <- lapply(names(discrete.targets),
      #                          function(x) wttabSlim(sample[, x], weights = sample[, "weights"],
      #                                                current.levels = discrete.levels[[x]]))
      
      # temporary slow version:
      current.values <- lapply(names(discrete.targets),
                               function(x) 
                                 prop.table(questionr::wtd.table(sample[, x], weights = sample[, "weights"])))
      
      if(!is.null(discrete.targets)) {
        current.discrete.diff <- max(mapply(function(x,y) max(abs(x-y)), 
                                            discrete.targets, current.values))  
      } else {
        current.discrete.diff <- 0
      }
    }
  }
  if(maxit == ii) {
    warning("Maximum iterations reached without convergence.")
  }
  
  final.weights <- sample[, "weights"]
  
  # efficiency calculation
  
  final.weights <- final.weights/ mean(final.weights)
  
  final.weights <- as.vector(final.weights)
  
  output.weights <- final.weights[match(sample.bk[, "unique.id"], sample[, "unique.id"])]
  
  high.weight.props <- prop.table(table((output.weights / max.weights) > 0.98))
  low.weight.props <- prop.table(table((output.weights / min.weights) < 1.02))
  
  if(is.na(high.weight.props["TRUE"])){
    high.weight.props["TRUE"] <- 0
  }
  if(is.na(low.weight.props["TRUE"])){
    low.weight.props["TRUE"] <- 0
  }
  if(low.weight.props["TRUE"]>extreme.weight.warning) {
    warning(low.weight.props["TRUE"]*100, "% of weights are close to lower weight limit")
  }
  if(high.weight.props["TRUE"]>extreme.weight.warning) {
    warning(low.weight.props["TRUE"]*100, "% of weights are close to higher weight limit")
  }
  return(output.weights)
}

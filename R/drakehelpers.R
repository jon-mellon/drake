wttabSlim <-function (x, weights = NULL,current.levels) 
{
  result <- .Internal(rowsum_matrix(weights, as.character(x), current.levels, FALSE, current.levels))[,1]
  result[is.na(result)] <- 0
  # result <- as.table.default(result)
  return(result)
}

clampWeights <- function(weights, max.weights, min.weights) {
  weights[weights > max.weights] <- max.weights
  weights[weights < min.weights] <- min.weights
  weights
}

nearestDensityGridIndex <- function(x, grid) {
  if(length(grid) < 2L) {
    return(rep.int(1L, length(x)))
  }

  midpoints <- (grid[-1L] + grid[-length(grid)]) / 2
  as.integer(findInterval(x, vec = midpoints) + 1L)
}

prepareDensityKernel <- function(con.target) {
  n.user <- length(con.target$x)
  n <- max(n.user, 512L)
  if(n > 512L) {
    n <- 2^ceiling(log2(n))
  }

  from <- min(con.target$x)
  to <- max(con.target$x)
  lo <- from - 4 * con.target$bw
  up <- to + 4 * con.target$bw

  kords <- seq.int(0, 2 * (up - lo), length.out = 2L * n)
  kords[(n + 2L):(2L * n)] <- -kords[n:2L]
  kords <- dnorm(kords, sd = con.target$bw)

  xords <- seq.int(lo, up, length.out = n)
  xout <- seq.int(from, to, length.out = n.user)
  step <- (up - lo) / (n - 1L)
  interp.pos <- ((xout - lo) / step) + 1
  interp.pos <- pmax.int(1, pmin.int(interp.pos, n))
  interp.left <- pmax.int(1L, pmin.int(n - 1L, floor(interp.pos)))
  interp.right <- interp.left + 1L
  interp.frac <- interp.pos - interp.left

  list(
    bw = con.target$bw,
    n = n,
    lo = lo,
    up = up,
    xords = xords,
    xout = xout,
    kernel_fft = Conj(fft(kords)),
    interp.left = as.integer(interp.left),
    interp.right = as.integer(interp.right),
    interp.frac = interp.frac
  )
}

normalizeDensityTargetY <- function(con.target) {
  con.target$y / sum(con.target$y)
}

resolveContinuousSupplement <- function(dens.matches, con.target) {
  if(is.list(dens.matches) && !is.null(dens.matches$match.index)) {
    return(dens.matches)
  }

  target.y <- attr(dens.matches, "target.y")
  if(is.null(target.y)) {
    target.y <- normalizeDensityTargetY(con.target)
  }

  list(
    match.index = as.integer(dens.matches),
    target.y = target.y,
    density.prep = prepareDensityKernel(con.target)
  )
}

checkOneContinuousValues <- function(x, weights, con.target, con.supp = NULL) {
  supp <- if(is.null(con.supp)) {
    list(
      x.values = x,
      match.index = nearestDensityGridIndex(x, con.target$x),
      target.y = normalizeDensityTargetY(con.target),
      density.prep = prepareDensityKernel(con.target)
    )
  } else {
    resolveContinuousSupplement(con.supp, con.target)
  }

  if(is.null(supp$x.values)) {
    supp$x.values <- x
  }

  total.weight <- sum(weights)
  if(!is.finite(total.weight) || total.weight <= 0) {
    return(NA_real_)
  }

  weight.vec <- weights / total.weight
  sample.y <- densityPreparedY(supp$x.values, weight.vec, supp$density.prep)
  sample.y <- sample.y / sum(sample.y)
  sum(abs(supp$target.y - sample.y))
}

checkContinuousPrepared <- function(weights, con.target, con.supp) {
  if(inherits(con.target, "density")) {
    return(checkOneContinuousValues(
      x = con.supp[[1L]]$x.values,
      weights = weights,
      con.target = con.target,
      con.supp = con.supp[[1L]]
    ))
  }

  total.diff <- 0
  stratify.var <- names(con.target)

  for(strat in stratify.var) {
    stratify.values <- names(con.target[[strat]])
    stratify.values <- stratify.values[!is.na(stratify.values)]
    diffs <- rep(NA_real_, length(stratify.values))
    names(diffs) <- stratify.values

    for(kk in stratify.values) {
      supp <- con.supp[[strat]][[kk]]
      diffs[kk] <- checkOneContinuousValues(
        x = supp$x.values,
        weights = weights[supp$rows],
        con.target = con.target[[strat]][[kk]],
        con.supp = supp
      )
    }

    strat.max <- max(diffs, na.rm = TRUE)
    if(is.finite(strat.max) && strat.max > total.diff) {
      total.diff <- strat.max
    }
  }

  total.diff
}

buildDiscreteSubsetTargetMatrix <- function(discrete.sub, target.levels, strata.levels) {
  matrix.out <- matrix(0,
                       nrow = length(target.levels),
                       ncol = length(strata.levels),
                       dimnames = list(target.levels, strata.levels))

  for(strata.level in names(discrete.sub)) {
    matrix.out[names(discrete.sub[[strata.level]]), strata.level] <- discrete.sub[[strata.level]]
  }

  matrix.out
}

maxMeanTargetDiff <- function(sample, weights, mean.targets) {
  if(is.null(mean.targets) || length(mean.targets) == 0L) {
    return(0)
  }

  max(vapply(names(mean.targets), function(var) {
    abs(stats::weighted.mean(sample[[var]], weights) - mean.targets[[var]])
  }, numeric(1)))
}

.drakePreparedCache <- new.env(parent = emptyenv())
.drakeResultCache <- new.env(parent = emptyenv())

clearDrakePreparedCache <- function() {
  rm(list = ls(.drakePreparedCache, all.names = TRUE), envir = .drakePreparedCache)
  rm(list = ls(.drakeResultCache, all.names = TRUE), envir = .drakeResultCache)
  invisible(NULL)
}

getCachedDrakeResult <- function(cache.key = NULL) {
  if(is.null(cache.key) || identical(cache.key, "")) {
    return(NULL)
  }

  key <- as.character(cache.key)[1L]
  if(exists(key, envir = .drakeResultCache, inherits = FALSE)) {
    return(get(key, envir = .drakeResultCache, inherits = FALSE))
  }

  NULL
}

setCachedDrakeResult <- function(cache.key = NULL, result) {
  if(is.null(cache.key) || identical(cache.key, "")) {
    return(invisible(result))
  }

  key <- as.character(cache.key)[1L]
  assign(key, result, envir = .drakeResultCache)
  invisible(result)
}

prepareDrakeInputs <- function(sample,
                               continuous.targets,
                               discrete.targets,
                               discrete.target.subset,
                               mean.targets,
                               initial.weights,
                               subset) {
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

  continuous.names <- names(continuous.targets)
  discrete.names <- names(discrete.targets)
  var.names.cont <- continuous.names
  var.names.discrete <- discrete.names
  var.names.cont2 <- unlist(lapply(continuous.targets, function(x) names(x)))
  var.names.mean <- names(mean.targets)
  var.names.discrete.sub <- names(discrete.target.subset)
  mean.names <- names(mean.targets)
  discrete.sub.names <- names(discrete.target.subset)

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
        valid.cases2[sample[, strt] == str.single & is.na(sample[, kk])] <- FALSE
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

  for(var in discrete.names) {
    discrete.targets <- fixDiscreteOrder(sample, var, discrete.targets)
  }

  discrete.levels <- list()
  discrete.codes <- list()

  discrete.vars <- discrete.names
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

    column <- factor(as.character(column), levels = target.levels)
    sample[[var]] <- column
    discrete.levels[[var]] <- levels(column)
    discrete.codes[[var]] <- as.integer(column)
  }

  subset.target.matrices <- list()
  subset.target.code.list <- list()
  subset.strata.code.list <- list()
  subset.target.matrix.list <- list()
  if(!is.null(discrete.target.subset)) {
    for(var in names(discrete.target.subset)) {
      subset.target.matrices[[var]] <- list()
      for(strata.var in names(discrete.target.subset[[var]])) {
        subset.target.matrices[[var]][[strata.var]] <- buildDiscreteSubsetTargetMatrix(
          discrete.sub = discrete.target.subset[[var]][[strata.var]],
          target.levels = discrete.levels[[var]],
          strata.levels = discrete.levels[[strata.var]]
        )
        subset.target.code.list[[length(subset.target.code.list) + 1L]] <- discrete.codes[[var]]
        subset.strata.code.list[[length(subset.strata.code.list) + 1L]] <- discrete.codes[[strata.var]]
        subset.target.matrix.list[[length(subset.target.matrix.list) + 1L]] <- subset.target.matrices[[var]][[strata.var]]
      }
    }
  }

  discrete.code.list <- if(length(discrete.names) > 0L) unname(discrete.codes[discrete.names]) else list()
  discrete.target.list <- if(length(discrete.names) > 0L) unname(discrete.targets[discrete.names]) else list()

  continuous.supplement <- list()
  for(var in continuous.names) {
    continuous.supplement[[var]] <- createContinuousSupplement(
      sample = sample,
      var = var,
      con.target = continuous.targets[[var]]
    )
  }

  weights <- (weights * nrow(sample)) / sum(weights)

  list(
    sample = sample,
    weights_start = weights,
    selection.base.weights = weights,
    n.original = n.original,
    valid.idx = valid.idx,
    continuous.targets = continuous.targets,
    discrete.targets = discrete.targets,
    discrete.target.subset = discrete.target.subset,
    mean.targets = mean.targets,
    continuous.names = continuous.names,
    discrete.names = discrete.names,
    mean.names = mean.names,
    discrete.codes = discrete.codes,
    discrete.code.list = discrete.code.list,
    discrete.target.list = discrete.target.list,
    subset.target.code.list = subset.target.code.list,
    subset.strata.code.list = subset.strata.code.list,
    subset.target.matrix.list = subset.target.matrix.list,
    continuous.supplement = continuous.supplement,
    tot.obs = nrow(sample)
  )
}

getPreparedDrakeInputs <- function(cache.key = NULL,
                                   sample,
                                   continuous.targets,
                                   discrete.targets,
                                   discrete.target.subset,
                                   mean.targets,
                                   initial.weights,
                                   subset) {
  if(is.null(cache.key) || identical(cache.key, "")) {
    return(prepareDrakeInputs(
      sample = sample,
      continuous.targets = continuous.targets,
      discrete.targets = discrete.targets,
      discrete.target.subset = discrete.target.subset,
      mean.targets = mean.targets,
      initial.weights = initial.weights,
      subset = subset
    ))
  }

  key <- as.character(cache.key)[1L]
  if(exists(key, envir = .drakePreparedCache, inherits = FALSE)) {
    return(get(key, envir = .drakePreparedCache, inherits = FALSE))
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
  assign(key, prepared, envir = .drakePreparedCache)
  prepared
}

unirootSlim <- function (f, interval, lower = min(interval), upper = max(interval)) {
  f.lower = f(lower)
  f.upper = f(upper)
  extendInt = c("no", "yes", "downX", "upX")
  check.conv = FALSE
  tol = .Machine$double.eps^0.25
  maxiter = 1000
  trace = 0
  Sig <- 0
  
  truncate <- function(x) pmax.int(pmin(x, .Machine$double.xmax), 
                                   -.Machine$double.xmax)
  
  doX <- FALSE
  
  val <- .External2(stats:::C_zeroin2, function(arg) f(arg), 
                    lower, upper, f.lower, f.upper, tol, as.integer(maxiter))
  iter <- as.integer(val[2L])
  if (iter < 0) {
    (if (check.conv) 
      stop
     else warning)(sprintf(ngettext(maxiter, "_NOT_ converged in %d iteration", 
                                    "_NOT_ converged in %d iterations"), maxiter), 
                   domain = NA)
    iter <- maxiter
  }
  
  it <- NA_integer_
  return(val[1L])
}
weightByMeanLinear <- function(weight, var, mean.target) {
  vw <- var * weight
  vw.sum <- sum(vw)
  wt.sum <- sum(weight)
  current.mean <- vw.sum /   wt.sum
  diffs <- abs(mean.target - var)
  
  # diffs <- log(diffs+1)
  hilo <- var<mean.target
  if(current.mean<mean.target) {
    fun <- function(k){
      vw.a.sum <- sum(vw[hilo] / (k ^ (diffs[hilo] + 1)))
      vw.b.sum <- sum(vw[!hilo] * (k ^ (diffs[!hilo] + 1)))
      
      n_a <- sum(weight[hilo] / (k ^ (diffs[hilo] + 1)))
      n_b <- sum(weight[!hilo] * (k ^ (diffs[!hilo] + 1)))
      
      out <- (vw.a.sum + vw.b.sum) / (n_a + n_b) - mean.target
      return(out)
    }
    k <- unirootSlim(f = fun, lower = 1, upper = 20)
    weight[hilo] <- weight[hilo] / (k ^ (diffs[hilo] + 1))
    weight[!hilo] <- weight[!hilo] * (k ^ (diffs[!hilo] + 1))
  } else {
    fun <- function(k){
      vw.a.sum <- sum(vw[hilo] * (k ^ (diffs[hilo] + 1)))
      vw.b.sum <- sum(vw[!hilo] / (k ^ (diffs[!hilo] + 1)))
      
      n_a <- sum(weight[hilo] * (k ^ (diffs[hilo] + 1)))
      n_b <- sum(weight[!hilo] / (k ^ (diffs[!hilo] + 1)))
      
      out <- (vw.a.sum + vw.b.sum) / (n_a + n_b) - mean.target
      return(out)
    }
    k <- unirootSlim(f = fun, lower = 1, upper = 20)
    # print(k)
    weight[hilo] <- weight[hilo] * (k ^ (diffs[hilo] + 1))
    weight[!hilo] <- weight[!hilo] / (k ^ (diffs[!hilo] + 1))
  }
  return(weight)
}

weightByMean <- function(weight, var, mean.target) {
  n <- sum(!is.na(weight ))
  weight <- (weight / sum(weight, na.rm = TRUE)) * n
  vw <- var * weight
  current.prob <- sum(vw, na.rm = TRUE) / n
  hilo <- var>mean.target
  
  if(current.prob<mean.target) {
    n_b <- sum(weight[hilo], na.rm = TRUE)
    b <- sum(vw[hilo], na.rm = TRUE) / n_b
    
    n_a <- sum(weight[!hilo], na.rm = TRUE)
    a <- sum(vw[!hilo], na.rm = TRUE) / n_a
    
  } else {
    n_a <- sum(weight[hilo], na.rm = TRUE)
    a <- sum(vw[hilo], na.rm = TRUE) / n_a
    
    n_b <- sum(weight[!hilo], na.rm = TRUE)
    b <- sum(vw[!hilo], na.rm = TRUE) / n_b
  }
  
  fun <- function(k) {
    (((a * n_a) / k ) + (b * n_b * k)) / 
      ((n_a/k)  + (k * n_b)) - 
      mean.target
  }
  
  k <- unirootSlim(f = fun, lower = 1, upper = 10)
  
  weight[hilo] <- weight[hilo] * k
  weight[!hilo] <- weight[!hilo] / k
  # questionr::wtd.mean(var, weight)
  return(weight)
}
densitySlim <- function (x, bw = 1, adjust = 1, kernel = "gaussian", weights = NULL, window = "gaussian", 
                         width, n = 512, from, to, cut = 3, na.rm = FALSE) {
  
  name <- ""
  x <- as.vector(x)
  x.na <- is.na(x)
  if (any(x.na)) {
    if (na.rm) 
      x <- x[!x.na]
    else stop("'x' contains missing values")
  }
  
  N <- nx <- as.integer(length(x))
  n.user <- n
  n <- max(n, 512)
  if (n > 512) 
    n <- 2^ceiling(log2(n))
  lo <- from - 4 * bw
  up <- to + 4 * bw
  y <- .Call(stats:::C_BinDist, x, weights, lo, up, n) 
  kords <- seq.int(0, 2 * (up - lo), length.out = 2L * n)
  kords[(n + 2):(2 * n)] <- -kords[n:2]
  kords <- dnorm(kords, sd = bw)
  
  kords <- fft(fft(y) * Conj(fft(kords)), inverse = TRUE)
  kords <- pmax.int(0, Re(kords)[1L:n]/length(y))
  
  xords <- seq.int(lo, up, length.out = n)
  
  xout <- seq.int(from, to, length.out = n.user)
  y <- approxSlim(x = xords, y = kords, xout = xout)
  # y <- approx(xords, kords, x)$y
  structure(list(x = xout, y = y, bw = bw, 
                 n = N, data.name = name), 
            class = "density")
}

densityPreparedY <- function(x, weights, density.prep) {
  y <- .Call(stats:::C_BinDist, as.vector(x), weights,
             density.prep$lo, density.prep$up, density.prep$n)
  smooth <- fft(fft(y) * density.prep$kernel_fft, inverse = TRUE)
  smooth <- pmax.int(0, Re(smooth)[1L:density.prep$n] / length(y))
  left <- density.prep$interp.left
  right <- density.prep$interp.right
  frac <- density.prep$interp.frac
  smooth[left] + ((smooth[right] - smooth[left]) * frac)
}

regularizeValuesSlim <- function (x, y, ties) {
  x <- xy.coords(x, y, setLab = FALSE)
  y <- x$y
  x <- x$x
  
  nx <- length(x)
  if (!identical(mean, "ordered")) {
    o <- .Internal(radixsort(TRUE, FALSE, FALSE, TRUE, x))
    x <- x[o]
    y <- y[o]
  }
  list(x = x, y = y)
}

approxSlim <- function (x, y = NULL, xout, n = 50, na.rm = FALSE) {
  method <- "linear"
  method <- pmatch(method, c("linear", "constant"))
  
  rule <- 1
  f <- 0
  lenR <- 1
  
  x <- regularizeValuesSlim(x, y, mean)
  y <- x$y
  x <- x$x
  yleft <- NA
  yright <- y[length(y)]
  
  x <- as.double(x)
  y <- as.double(y)
  yout <- .Call(stats:::C_Approx, x, y, xout, method, yleft, yright, f, na.rm)
  return(yout)
}

weightContinuousOnceValues <- function(x, weights, con.target, dens.matches) {
  supp <- resolveContinuousSupplement(dens.matches, con.target)
  if(!is.null(supp$x.values)) {
    x <- supp$x.values
  }

  sample.y <- densityPreparedY(x = x, weights = weights, density.prep = supp$density.prep)
  sample.y <- sample.y / sum(sample.y)
  ratios <- supp$target.y / sample.y
  newwt <- ratios[supp$match.index] * weights

  if(anyNA(newwt)) {
    stop("NAs on weights after raking on continuous target")
  }
  return(newwt)
}

weightContinuousOnce <- function(data, var, con.target, dens.matches) {
  weightContinuousOnceValues(data[, var], data[, "weights"], con.target, dens.matches)
}

weightByContinuous <- function(weights = NULL, sample, var, con.target, 
                               max.weights = max.weights, min.weights = min.weights,
                               cap.every.var, con.supp) {
  wt.init <- if(is.null(weights)) sample[, "weights"] else weights
  if(inherits(con.target, "density")) {
    supp <- con.supp[[var]]
    wt.out <- weightContinuousOnceValues(supp$x.values, wt.init, con.target, dens.matches = supp)
  } else {
    wt.out <- wt.init
    stratify.var <- names(con.target)
    for(strat in stratify.var) {
      stratify.values <- names(con.target[[strat]])
      stratify.values <- stratify.values[!is.na(stratify.values)]
      for(kk in stratify.values) {
        supp <- con.supp[[strat]][[kk]]
        row.idx <- supp$rows
        tmp.wts <- wt.init[row.idx]
        tot.weight <- sum(tmp.wts)

        tmp.wts <- weightContinuousOnceValues(x = supp$x.values,
                                              weights = tmp.wts,
                                              con.target = con.target[[strat]][[kk]],
                                              dens.matches = supp)
        tmp.wts <- (tmp.wts / sum(tmp.wts)) * tot.weight
        wt.out[row.idx] <- tmp.wts
      }
    }
  }
  
  if(cap.every.var) {
    wt.out <- clampWeights(wt.out, max.weights = max.weights, min.weights = min.weights)
  }
  
  return(wt.out)
}


createContinuousSupplement <- function(sample, var, con.target) {
  x.values <- sample[[var]]

  if(inherits(con.target, "density")) {
    out <- list(list(
      x.values = x.values,
      match.index = nearestDensityGridIndex(x.values, con.target$x),
      target.y = normalizeDensityTargetY(con.target),
      density.prep = prepareDensityKernel(con.target)
    ))
    names(out) <- var
  } else {
    out <- list()
    stratify.var <- names(con.target)
    for(strat in stratify.var) {
      out[[strat]] <- list()
    }
    
    strat.vals <- list()
    for(strat in stratify.var) {
      strat.vals[[strat]] <- names(con.target[[strat]])
      strat.column <- sample[[strat]]
      if(!all(strat.column %in% strat.vals[[strat]])) {
        warning(paste0("For stratified draking, values in ", stratify.var, "not in targets: ",
                       unique(strat.column[!strat.column %in% strat.vals[[strat]]])))
      }
      if(!all(strat.vals[[strat]][!is.na(strat.vals[[strat]])] %in% strat.column)) {
        stop(paste0("For stratified draking, values in ", strat, "not in sample: ",
                    unique(strat.vals[[strat]][!is.na(strat.vals[[strat]]) &
                                                !strat.vals[[strat]] %in% strat.column])))
      }
      strat.vals[[strat]] <- strat.vals[[strat]][!is.na(strat.vals[[strat]])]
    }
    
    # stratify.values <- names(con.target[[stratify.var]])
    # stratify.values <- stratify.values[!is.na(stratify.values)]
    
    for(strat in stratify.var) {
      strat.column <- sample[[strat]]
      
      for(kk in strat.vals[[strat]] ) {
        row.idx <- which(strat.column == kk)
        out[[strat]][[kk]] <- list(
          rows = row.idx,
          x.values = x.values[row.idx],
          match.index = nearestDensityGridIndex(x.values[row.idx], con.target[[strat]][[kk]]$x),
          target.y = normalizeDensityTargetY(con.target[[strat]][[kk]]),
          density.prep = prepareDensityKernel(con.target[[strat]][[kk]])
        )
      }
    }
  }
  return(out)
}
fixDiscreteOrder <- function(sample, var, discrete.targets) {
  sample.vars <- names(table(sample[, var]))
  target.vars <- names(discrete.targets[[var]])
  
  if(all(sample.vars %in% target.vars) & all(target.vars %in% sample.vars) &
     length(sample.vars)==length(target.vars)) {
    discrete.targets[[var]] <- discrete.targets[[var]][match(sample.vars, target.vars)]
  }
  if(any(!sample.vars %in% target.vars) | any(!target.vars %in% sample.vars)) {
    stop("Sample values not in targets: ", paste(sample.vars[!sample.vars %in% target.vars], collapse = ", "), 
         ". Target values not in sample: ", paste(target.vars[!target.vars %in% sample.vars], collapse = ", "))
  }
  return(discrete.targets)
}

validateNormalizeDiscreteTargetVector <- function(target, label, tol = 1e-4) {
  target.sum <- sum(target)

  if(target.sum > (1 + tol)) {
    stop("Following targets sum to more than 1: ", label)
  }

  if(target.sum < (1 - tol)) {
    if(target.sum <= 0) {
      stop("Cannot normalize target with non-positive sum: ", label)
    }
    return(list(target = target / target.sum,
                target.sum = target.sum,
                normalized = TRUE))
  }

  return(list(target = target,
              target.sum = target.sum,
              normalized = FALSE))
}

normalizeDiscreteTargets <- function(discrete.targets, tol = 1e-4) {
  if(is.null(discrete.targets) || length(discrete.targets) == 0) {
    return(discrete.targets)
  }

  out <- discrete.targets
  normalized <- character(0)

  for(var in names(out)) {
    result <- validateNormalizeDiscreteTargetVector(out[[var]], var, tol = tol)
    out[[var]] <- result$target

    if(result$normalized) {
      normalized <- c(normalized, paste0(var, " (sum=",
                                         formatC(result$target.sum, format = "f", digits = 6),
                                         ")"))
    }
  }

  if(length(normalized) > 0) {
    warning("Normalized discrete.targets that summed to less than 1: ",
            paste(normalized, collapse = ", "))
  }

  return(out)
}

normalizeDiscreteTargetSubset <- function(discrete.target.subset, tol = 1e-4) {
  if(is.null(discrete.target.subset) || length(discrete.target.subset) == 0) {
    return(discrete.target.subset)
  }

  out <- discrete.target.subset
  normalized <- character(0)

  for(target.var in names(out)) {
    strata.vars <- names(out[[target.var]])
    for(strata.var in strata.vars) {
      strata.levels <- names(out[[target.var]][[strata.var]])
      for(strata.level in strata.levels) {
        label <- paste0(target.var, " [", strata.var, "=", strata.level, "]")
        result <- validateNormalizeDiscreteTargetVector(
          out[[target.var]][[strata.var]][[strata.level]],
          label,
          tol = tol
        )
        out[[target.var]][[strata.var]][[strata.level]] <- result$target

        if(result$normalized) {
          normalized <- c(normalized, paste0(label, " (sum=",
                                             formatC(result$target.sum, format = "f", digits = 6),
                                             ")"))
        }
      }
    }
  }

  if(length(normalized) > 0) {
    warning("Normalized discrete.target.subset vectors that summed to less than 1: ",
            paste(normalized, collapse = ", "))
  }

  return(out)
}

weightByDiscrete <- function(sample, var, init.weight, discrete.targets, 
                             max.weights, min.weights, cap.every.var,current.levels)
{
  # init.weight <- init.weight / sum(init.weight, na.rm = T)
  wt.table <- wttabSlim(x = sample[, var], weights = init.weight, current.levels = current.levels)
  wt.table <- prop.table(wt.table)
  ratios <- discrete.targets[[var]] / wt.table
  
  init.weight <- ratios[sample[, var]] * init.weight
  if(anyNA(sample[, "weights"])) {
    
    stop("NAs on weights after raking on ", var)
  }
  if(cap.every.var) {
    sample[, "weights"][sample[, "weights"]>max.weights] <- max.weights
    sample[, "weights"][sample[, "weights"]<min.weights] <- min.weights
  }
  return(init.weight)
}


checkOneContinuous <- function(data, var, con.target, weights, con.supp = NULL) {
  weight.vec <- if(is.character(weights) && length(weights) == 1L) {
    data[[weights]]
  } else {
    weights
  }
  supp <- if(is.null(con.supp)) {
    list(
      x.values = data[[var]],
      match.index = nearestDensityGridIndex(data[, var], con.target$x),
      target.y = normalizeDensityTargetY(con.target),
      density.prep = prepareDensityKernel(con.target)
    )
  } else {
    resolveContinuousSupplement(con.supp, con.target)
  }
  if(is.null(supp$x.values)) {
    supp$x.values <- data[[var]]
  }

  checkOneContinuousValues(
    x = supp$x.values,
    weights = weight.vec,
    con.target = con.target,
    con.supp = supp
  )
}
checkContinuous <- function(sample, var, con.target, weights, debug = FALSE, con.supp = NULL) {
  if(debug) {
    browser()
  }
  if(!is.null(con.supp) && !(is.character(weights) && length(weights) == 1L)) {
    return(checkContinuousPrepared(weights = weights, con.target = con.target, con.supp = con.supp))
  }

  sample <- sample[!is.na(sample[, var]) & !is.na(sample[, weights]), ]
  if(length(var)==0) {
    return(NULL)
  }
  if(inherits(con.target, "density")) {
    total.diff <- checkOneContinuous(
      sample,
      var,
      con.target,
      weights,
      con.supp = if(is.null(con.supp)) NULL else con.supp[[var]]
    )
  } else {
    stratify.var <- names(con.target)
    total.diff <- 0
    for(strat in stratify.var) {
      stratify.values <- names(con.target[[strat]])
      diffs <- rep(NA, length(stratify.values))
      names(diffs) <- stratify.values
      for(kk in stratify.values) {
        diffs[kk] <- checkOneContinuous(data = sample[which(sample[, strat]==kk), ], 
                                        var = var,
                                        con.target = con.target[[strat]][[kk]], 
                                        weights = weights,
                                        con.supp = if(is.null(con.supp)) NULL else con.supp[[strat]][[kk]])
      }
      if(max(diffs, na.rm = TRUE) > total.diff) {
        total.diff <- max(diffs)  
      }
    }
  }
  return(total.diff)
}

checkDiscrete <- function(discrete.targets, sample, weights) {
  by.weight <- as.list(rep(NA, length(weights)))
  names(by.weight) <- weights
  for(ii in weights) {
    by.weight[[ii]] <- lapply(names(discrete.targets), 
                              function(x)
                                wttabSlim(sample[, x], weights = sample[, ii], 
                                          current.levels = discrete.levels[[ii]]))    
    names(by.weight[[ii]]) <- names(discrete.targets)
  }
  vals <- as.list(rep(NA, length(discrete.targets)))
  names(vals) <- names(discrete.targets)
  
  for(ii in names(discrete.targets)) {
    vals[[ii]] <- round(mellonMisc::dtf(Target = discrete.targets[[ii]],
                                        sapply(by.weight, function(x) x[[ii]])) * 100, 3)
  }
  
  return(vals)
}
weightEfficiency <- function(final.weights, initial.weights) {
  initial.weights[is.na(final.weights)] <- NA
  efficiency <- ((sum(initial.weights * final.weights, na.rm = TRUE)^2) * 100) /
    (sum(initial.weights, na.rm = TRUE) * sum(initial.weights * final.weights^2, na.rm = TRUE))
  return(efficiency)
}


weightByDiscreteSubset <- function(sample, var, discrete.sub, 
                                   max.weights = max.weights, min.weights = min.weights,
                                   cap.every.var, current.levels) {
  wt.out <- rep(NA, nrow(sample))
  stratify.var <- names(discrete.sub)
  stratify.values <- names(discrete.sub[[stratify.var]])
  stratify.values <- stratify.values[!is.na(stratify.values)]
  # if(!all(sample[, stratify.var] %in% stratify.values)) {
  #   stop(paste0("While in ", var, " for stratified draking, values in ", stratify.var, "not in targets:",
  #               unique(sample[, stratify.var][!sample[, stratify.var] %in% stratify.values])))
  # }
  for(kk in stratify.values) {
    sample.temp <- sample[sample[, stratify.var]==kk, ]
    tmp.wts <- sample.temp[, "weights"]
    tot.weight <- sum(tmp.wts)
    temp.target <- list(discrete.sub[[stratify.var]][[kk]])
    names(temp.target) <- var
    tmp.wts <- weightByDiscrete(sample = sample.temp, var = var, 
                                init.weight =  tmp.wts,
                                discrete.targets = temp.target,
                                max.weights = max.weights, 
                                min.weights = min.weights, 
                                current.levels = current.levels, 
                                cap.every.var = FALSE)
    tmp.wts <- (tmp.wts / sum(tmp.wts)) * tot.weight
    
    weight.replace <- tmp.wts[match(sample[, "unique.id"], sample.temp[, "unique.id"])]
    wt.out[!is.na(weight.replace)] <- weight.replace[!is.na(weight.replace)]
  }
  
  wt.out[is.na(wt.out)] <- sample[is.na(wt.out), "weights"]

  if(cap.every.var) {
    wt.out[wt.out>max.weights] <- max.weights
    wt.out[wt.out<min.weights] <- min.weights  
  }
  return(wt.out)
}



calc_efficiency <- function(x, base = 1) {
  # taken from cchoe/numerator  package
  # x = weights
  # base = 1
  Pj <- rep(base, length(x))
  Rj <- x
  PjRj <- Pj*Rj
  PjRj.sq <- PjRj^2
  
  Pj.Sigm <- sum(Pj)
  Rj.Sigm <- sum(Rj)
  PjRj.Sigm <- sum(PjRj)
  PjRj.sq.Sigm <- sum(PjRj.sq)
  
  weight.index <- (PjRj.Sigm * Pj.Sigm) / Rj.Sigm
  weight.index.sq <- PjRj.sq.Sigm * ((Pj.Sigm / Rj.Sigm)^2)
  
  out <- 100 * (weight.index^2) / (weight.index.sq * Pj.Sigm)
  
  return(out)
}



ageDensityUNWPP <- function(year, country, min.age,
                            max.age, data.age.vec, pops,
                            bw.mult = 2) {
  pop.temp <- pops[pops$country==country & pops$Time==year, ]
  pop.temp   <- pop.temp[pop.temp$AgeGrp>=min.age, ]
  pop.temp$AgeGrp <- pop.temp$AgeGrp 
  data.age.vec <- data.age.vec 
  data.age.vec[which(data.age.vec >= max.age)] <- max.age
  
  source.dens <- density(na.omit(data.age.vec))
  source.bw <- source.dens$bw
  bw.to.use <- bw.mult * source.bw
  pop.temp$PopTotal <- pop.temp$PopTotal * 1000
  
  
  ages.pop <- inverse.rle(list(lengths = round(pop.temp$PopTotal), values = pop.temp$AgeGrp))
  ages.pop[ages.pop>=max.age] <- max.age
  ages.pop <- ages.pop + runif(length(ages.pop))
  target.density <- density(ages.pop, bw = bw.to.use)
  return(target.density)
}

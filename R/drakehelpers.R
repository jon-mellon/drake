wttabSlim <-function (x, weights = NULL,current.levels) 
{
  result <- .Internal(rowsum_matrix(weights, as.character(x), current.levels, F, current.levels))[,1]
  result[is.na(result)] <- 0
  # result <- as.table.default(result)
  return(result)
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
  weight <- (weight / sum(weight, na.rm = T)) * n
  vw <- var * weight
  current.prob <- sum(vw, na.rm = T) / n
  hilo <- var>mean.target
  
  if(current.prob<mean.target) {
    n_b <- sum(weight[hilo], na.rm = T)
    b <- sum(vw[hilo], na.rm = T) / n_b
    
    n_a <- sum(weight[!hilo], na.rm = T)
    a <- sum(vw[!hilo], na.rm = T) / n_a
    
  } else {
    n_a <- sum(weight[hilo], na.rm = T)
    a <- sum(vw[hilo], na.rm = T) / n_a
    
    n_b <- sum(weight[!hilo], na.rm = T)
    b <- sum(vw[!hilo], na.rm = T) / n_b
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

approxSlim <- function (x, y = NULL, xout, n = 50) {
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
  yout <- .Call(stats:::C_Approx, x, y, xout, method, yleft, yright, f,na.rm)
  return(yout)
}

weightContinuousOnce <- function(data, var, con.target, dens.matches) {
  start.weights <- sum(data[, "weights"])
  data[, "weights"] <- data[, "weights"] / start.weights
  
  sample.density <- densitySlim(x = data[, var], n = length(con.target$x), 
                                from = min(con.target$x), 
                                to = max(con.target$x), 
                                weights = data[, "weights"], bw = con.target$bw)
  sample.density$y <- sample.density$y / sum(sample.density$y)
  con.target$y <- con.target$y / sum(con.target$y)
  ratios <- con.target$y / sample.density$y
  
  unique.vals <- unique.default(data[, var])
  
  # dens.matches <- vapply(unique.vals, function(x)
  #   which.min(abs(sample.density$x - x)), 1)
  
  ratio.match <- ratios[dens.matches]
  
  names(ratio.match) <-  as.character(unique.vals)
  char.var <- as.character(data[, var])
  # newwt <- ratio.match[char.var]
  newwt <- ratio.match[attributes(dens.matches)$charmatches]
  # print(all(newwt==newwttest))
  
  newwt <- newwt * data[, "weights"]
  
  if(anyNA(newwt)) {
    
    stop("NAs on weights after raking on ", var)
  }
  return(newwt)
}
weightByContinuous <- function(sample, var, con.target, 
                               max.weights = max.weights, min.weights = min.weights,
                               cap.every.var, con.supp) {
  if(class(con.target)=="density") {
    wt.out <- weightContinuousOnce(data = sample, var, con.target, dens.matches = con.supp[[var]])
  } else {
    wt.out <- rep(NA, nrow(sample))
    stratify.var <- names(con.target)
    stratify.values <- names(con.target[[stratify.var]])
    stratify.values <- stratify.values[!is.na(stratify.values)]
    if(!all(sample[, stratify.var] %in% stratify.values)) {
      stop(paste0("For stratified draking, values in ", stratify.var, "not in targets:",
                  unique(sample[, stratify.var][!sample[, stratify.var] %in% stratify.values])))
    }
    for(kk in stratify.values) {
      sample.temp <- sample[sample[, stratify.var]==kk, ]
      tmp.wts <- sample.temp[, "weights"]
      tot.weight <- sum(tmp.wts)
      
      tmp.wts <- weightContinuousOnce(data = sample.temp, var = var, 
                                      con.target = con.target[[stratify.var]][[kk]],
                                      dens.matches = con.supp[[stratify.var]][[kk]])
      tmp.wts <- (tmp.wts / sum(tmp.wts)) * tot.weight
      
      weight.replace <- tmp.wts[match(sample[, "unique.id"], sample.temp[, "unique.id"])]
      wt.out[!is.na(weight.replace)] <- weight.replace[!is.na(weight.replace)]
    }
  }
  if(cap.every.var) {
    wt.out[wt.out>max.weights] <- max.weights
    wt.out[wt.out<min.weights] <- min.weights  
  }
  return(wt.out)
}


createContinuousSupplement <- function(sample, var, con.target) {
  if(class(con.target)=="density") {
    testdensity <- densitySlim(x = sample[, var], n = length(con.target$x), 
                               from = min(con.target$x), 
                               to = max(con.target$x), 
                               weights = sample[, "weights"], bw = con.target$bw)
    unique.vals <- unique.default(sample[, var])
    
    dens.matches <- vapply(unique.vals, function(x)
      which.min(abs(testdensity$x - x)), 1)
    
    attributes(dens.matches)$charmatches <- match(as.character(sample[, var]), unique.vals)
    
    out <- list(dens.matches)
    names(out) <- var
  } else {
    out <- list(list())
    
    wt.out <- rep(NA, nrow(sample))
    stratify.var <- names(con.target)
    names(out) <- stratify.var
    
    stratify.values <- names(con.target[[stratify.var]])
    stratify.values <- stratify.values[!is.na(stratify.values)]
    if(!all(sample[, stratify.var] %in% stratify.values)) {
      stop(paste0("For stratified draking, values in ", stratify.var, "not in targets:",
                  unique(sample[, stratify.var][!sample[, stratify.var] %in% stratify.values])))
    }
    for(kk in stratify.values) {
      sample.temp <- sample[sample[, stratify.var]==kk, ]
      tmp.wts <- sample.temp[, "weights"]
      tot.weight <- sum(tmp.wts)
      
      testdensity <- densitySlim(x = sample.temp[, var], n = length(con.target[[stratify.var]][[kk]]$x), 
                                 from = min(con.target[[stratify.var]][[kk]]$x), 
                                 to = max(con.target[[stratify.var]][[kk]]$x), 
                                 weights = sample[, "weights"], bw = con.target[[stratify.var]][[kk]]$bw)
      unique.vals <- unique.default(sample.temp[, var])
      
      dens.matches <- vapply(unique.vals, function(x)
        which.min(abs(testdensity$x - x)), 1)
      attributes(dens.matches)$charmatches <- match(as.character(sample.temp[, var]), unique.vals)
      
      out[[stratify.var]][[kk]] <- dens.matches
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


checkOneContinuous <- function(data, var, con.target, weights) {
  data[, weights] <- data[, weights] / sum(data[, weights])
  
  sample.density <- densitySlim(data[, var], n = length(con.target$x), 
                                from = min(con.target$x), 
                                to = max(con.target$x), 
                                weights = data[, weights], bw = con.target$bw)
  
  sample.density$y <- sample.density$y / sum(sample.density$y)
  con.target$y <- con.target$y / sum(con.target$y)
  total.diff <- sum(abs(con.target$y - sample.density$y))
  
  return(total.diff)
}
checkContinuous <- function(sample, var, con.target, weights, debug = F) {
  if(debug) {
    browser()
  }
  sample <- sample[!is.na(sample[, var]) & !is.na(sample[, weights]), ]
  if(length(var)==0) {
    return(NULL)
  }
  if(class(con.target)=="density") {
    total.diff <- checkOneContinuous(sample, var, con.target, weights)
  } else {
    stratify.var <- names(con.target)
    stratify.values <- names(con.target[[stratify.var]])
    diffs <- rep(NA, length(stratify.values))
    names(diffs) <- stratify.values
    for(kk in stratify.values) {
      diffs[kk] <- checkOneContinuous(data = sample[sample[, stratify.var]==kk, ], 
                                      var = var,
                                      con.target = con.target[[stratify.var]][[kk]], 
                                      weights = weights)
    }
    total.diff <- max(diffs)
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
    vals[[ii]] <- round(dtf(Target = discrete.targets[[ii]], sapply(by.weight, function(x) x[[ii]]))  * 100, 3)
  }
  
  return(vals)
}
weightEfficiency <- function(final.weights, initial.weights) {
  initial.weights[is.na(final.weights)] <- NA
  efficiency <- ((sum(initial.weights * final.weights, na.rm = T)^2) * 100) / 
    (sum(initial.weights, na.rm = T) * sum(initial.weights* final.weights^2, na.rm = T))  
  return(efficiency)
}


weightByDiscreteSubset <- function(sample, var, discrete.sub, 
                                   max.weights = max.weights, min.weights = min.weights,
                                   cap.every.var, current.levels) {
  wt.out <- rep(NA, nrow(sample))
  stratify.var <- names(discrete.sub)
  stratify.values <- names(discrete.sub[[stratify.var]])
  stratify.values <- stratify.values[!is.na(stratify.values)]
  if(!all(sample[, stratify.var] %in% stratify.values)) {
    stop(paste0("For stratified draking, values in ", stratify.var, "not in targets:",
                unique(sample[, stratify.var][!sample[, stratify.var] %in% stratify.values])))
  }
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
  if(cap.every.var) {
    wt.out[wt.out>max.weights] <- max.weights
    wt.out[wt.out<min.weights] <- min.weights  
  }
  return(wt.out)
}

#' Drake main function
drake <- function(sample, continuous.targets = NULL, discrete.targets,
                  mean.targets = NULL,
                  max.weights = 25, min.weights = 1/max.weights,
                  maxit = 1000, initial.weights = rep(1, nrow(sample)), 
                  max.discrete.diff = 0.0005, max.mean.diff = 0.001,
                  max.con.diff = 0.01,
                  subset = rep(T, nrow(sample)), 
                  debug = F, cap.every.var = F, check.convergence.every = 100) {
  if(debug) {
    browser()
  }
  tot.weights <- sapply(discrete.targets, sum)
  if(any(tot.weights>1.0001)) {
    stop("Following targets sum to more than 1: ", paste(names(which(tot.weights>1.00001)), collapse = ", "))
  }
  
  require(mellonMisc)
  sample <- dtf(sample)
  sample[, "unique.id"] <- 1:nrow(sample)
  sample.bk <- sample
  
  var.names.cont <- names(continuous.targets)
  var.names.discrete <- names(discrete.targets)
  var.names.cont2 <-  unlist(lapply(continuous.targets, function(x) names(x)) ) 
  var.names.mean <- names(mean.targets)
  if(any(var.names.cont2 %in% c("data.name", "bw"))) {
    var.names.cont2 <- NULL
  }
  var.names.comb <- c(var.names.cont, var.names.discrete, var.names.cont2,var.names.mean,  "unique.id")
  
  initial.weights[initial.weights==0] <- NA
  
  sample <- sample[, var.names.comb, drop = F]
  sample[, "weights"] <- initial.weights
  
  valid.cases <- complete.cases(sample[, var.names.comb, drop = F]) & subset & !is.na(sample[, "weights"])
  
  sample <- sample[valid.cases, ]
  initial.weights <- as.vector(initial.weights[valid.cases])
  
  
  require(questionr)
  for(var in names(discrete.targets)) {
    discrete.targets <- fixDiscreteOrder(sample, var, discrete.targets)  
  }
  
  weight.change <- 1
  ii <- 1
  sample[, "weights"] <- sample[, "weights"] / sum(sample[, "weights"], na.rm = T)
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
  for(var in names(discrete.targets)) {
    if(is.numeric(sample[, var])) {
      sample[, var] <- factor(as.character(sample[, var]), 
                              levels = unique(sample[, var])[order(unique(sample[, var]))])
    }
    if(is.character(sample[, var])) {
      sample[, var] <- factor(sample[, var], levels = names(discrete.targets[[var]]))
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
  
  while(ii < maxit & ((current.discrete.diff>max.discrete.diff) |
                      (current.con.diff>max.con.diff) | 
                      current.mean.diff > max.mean.diff )  ) {
    
    if((ii / 10)==round(ii/10)) {
      # print(ii)
    }
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
    sample[, "weights"] <- prop.table(sample[, "weights"]) 
    
    sample[, "weights"][sample[, "weights"] > (max.wt)] <- max.wt
    sample[, "weights"][sample[, "weights"] < (min.wt)] <- min.wt
    
    
    
    
    if(length(continuous.targets)!=0 & ((ii / check.convergence.every)==round(ii/check.convergence.every)))  {
      current.con.diff <- rep(NA, length(names(continuous.targets)))
      names(current.con.diff) <- names(continuous.targets)
      for(con.t in names(continuous.targets)) {
        current.con.diff[con.t] <- checkContinuous(sample = sample, var = con.t, 
                                                   con.target = continuous.targets[[con.t]], 
                                                   weights = "weights", debug = F)
      }
      current.con.diff <- max(current.con.diff)
    } else {
      current.con.diff <- 0
    }
    
    if(length(discrete.targets)!=0 & ((ii / check.convergence.every)==round(ii/check.convergence.every)))  {
      current.values <- lapply(names(discrete.targets),
                               function(x) wttabSlim(sample[, x], weights = sample[, "weights"],
                                                     current.levels = discrete.levels[[x]]))
      
      if(!is.null(discrete.targets)) {
        current.discrete.diff <- max(mapply(function(x,y) max(abs(x-y)), 
                                            discrete.targets, current.values))  
      } else {
        current.discrete.diff <- 0
      }
    }
    
  }
  if(maxit==ii) {
    warning("Maximum iterations reached without convergence.")
  }
  
  final.weights <- sample[, "weights"]
  
  # efficiency calculation
  
  final.weights <- final.weights/ mean(final.weights)
  
  final.weights <- as.vector(final.weights)
  
  output.weights <- final.weights[match(sample.bk[, "unique.id"], sample[, "unique.id"])]
  return(output.weights)
}



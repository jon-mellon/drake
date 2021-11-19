getAgeDensities <- function(data, weight.var, var.suffix, target.suffix, 
                            continuous.var = "age",
                            bw.multiple = 2, min.bw = 0,
                            stratify.var = "turnout",
                            stratify.vals = c("Voted", "DNV")) {
  data <- mellonMisc::dtf(data)
  data <- data[!(is.na(data[, paste0(continuous.var, var.suffix)]) | is.na(data[, weight.var])), ]
  
  density.list <- list()
  for(strat in stratify.vals) {
    density.list[[strat]] <- suppressWarnings(density(data[, paste0(continuous.var, var.suffix)][which(data[, paste0(stratify.var, var.suffix)]==strat)], 
                                                      weights = data[which(data[, paste0(stratify.var, var.suffix)]==strat), weight.var]))
  }
  
  bw <- max(sapply(density.list, function(x) x$bw))
  bw <- bw * bw.multiple
  bw <- max(c(bw, min.bw))
  data[, weight.var] <- data[, weight.var] / sum(data[, weight.var])
  
  
  density.list <- list()
  for(strat in stratify.vals) {
    density.list[[strat]] <- suppressWarnings(density(data[, paste0(continuous.var, var.suffix)][which(data[, paste0(stratify.var, var.suffix)]==strat)], 
                                                      weights = data[which(data[, paste0(stratify.var, var.suffix)]==strat), weight.var],
                                                      bw = bw))
  }
  
  continuous.targets <- list()
  continuous.targets[[paste0(continuous.var, target.suffix)]] <- list()
  continuous.targets[[paste0(continuous.var, target.suffix)]][[paste0(stratify.var, target.suffix)]] <- 
    density.list
  
  return(continuous.targets)
}

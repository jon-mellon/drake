createContinuousTargets <- function(continuous.vars, max, min, bw = 5, sample) {
  
  getTargetDensity <- function(x) {
    pop.x.density <- density(x, from = min, to = max,
                             n = length(min:max), bw)
    return(pop.x.density)
  }
  
  continuous.targets <- lapply(continuous.vars, getTargetDensity)  
  return(continuous.targets)
}
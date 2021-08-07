drakeClose <- function(sample, continuous.targets, discrete.targets,
                       max.weights = 25, min.weights = 1/max.weights,
                       maxit = 1000, initial.weights = 1, 
                       max.discrete.diff = 0.005, 
                       max.con.diff = 0.2, step.size = 5, min.step = 0.1) {
  
  current.max <- max.weights
  current.weights <- initial.weights
  
  while(step.size>min.step) {
    print(current.max)
    outcome <- try(drake(sample, continuous.targets, discrete.targets,
                         max.weights = current.max, 
                         min.weights = 1/current.max,
                         maxit = maxit, initial.weights = current.weights, 
                         max.discrete.diff = max.discrete.diff, 
                         max.con.diff = max.con.diff))
    
    if(class(outcome)=="try-error") {
      step.size <- step.size / 2
      current.max <- current.max + step.size
    } else {
      step.size <- step.size * (2/3)
      current.weights <- outcome
      current.max <- (current.max - step.size)
    }
  }
  return(current.weights)
}
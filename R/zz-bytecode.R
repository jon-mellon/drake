.drakeByteCompile <- function(env = environment()) {
  if(!requireNamespace("compiler", quietly = TRUE)) {
    return(invisible(NULL))
  }

  targets <- c(
    "drake",
    "drakeClose",
    "createContinuousSupplement",
    "weightByContinuous",
    "weightContinuousOnceValues",
    "checkContinuousPrepared",
    "checkContinuous",
    "checkOneContinuousValues",
    "densityPreparedY",
    "prepareDensityKernel",
    "maxMeanTargetDiff",
    "normalizeDiscreteTargets",
    "normalizeDiscreteTargetSubset",
    "collectDrakeBenchmarkWarnings",
    "runDrakeBenchmarkInvocation",
    "runDrakeBenchmarkCase"
  )

  for(name in targets) {
    if(exists(name, envir = env, inherits = FALSE)) {
      fn <- get(name, envir = env, inherits = FALSE)
      if(is.function(fn) && !isTRUE(attr(fn, ".drake_bytecompiled"))) {
        compiled <- compiler::cmpfun(fn)
        attr(compiled, ".drake_bytecompiled") <- TRUE
        assign(name, compiled, envir = env)
      }
    }
  }

  invisible(NULL)
}

.drakeByteCompile(environment())

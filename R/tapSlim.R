
tapSlim <- function (X, INDEX, FUN = NULL) {
  # INDEX <- as.factor(INDEX)
  level <- levels(INDEX)
  namelist <- list(level)
  INDEX <- list(INDEX)
  nI <- 1
  cumextent <-extent <- length(level)
  storage.mode(cumextent) <- "integer"
  ngroup <- cumextent
  group <- as.integer(INDEX[[1L]])
  levels(group) <- as.character(seq_len(ngroup))
  class(group) <- "factor"
  ans <- split.default(X, group)
  names(ans) <- NULL
  index <- as.logical(lengths(ans))
  ans <- sapSlim(X = ans[index], FUN = FUN)
  names(ans) <- namelist[[1]]
  return(ans)
}
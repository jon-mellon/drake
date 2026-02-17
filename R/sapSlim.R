sapSlim <- function (X, FUN) {
  # Sapply slim version
  answer <- lapply(X = X, FUN = FUN)
  answer <- s2arraySlim(answer, higher = TRUE)
  return(answer)
}

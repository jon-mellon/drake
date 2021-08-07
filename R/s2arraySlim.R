s2arraySlim <- function (x, higher = TRUE) {
  # simplify2array slim version
  if (length(common.len <- unique.default(lengths(x))) > 1L) 
    return(x)
  if (common.len == 1L) 
    unlist(x, recursive = FALSE)
  else if (common.len > 1L) {
    n <- length(x)
    r <- unlist(x, recursive = FALSE, use.names = FALSE)
    if (higher && length(c.dim <- unique.default(lapply(x, dim))) == 
        1 && is.numeric(c.dim <- c.dim[[1L]]) && prod(d <- c(c.dim, n)) == length(r)) {
      iN1 <- is.null(n1 <- dimnames(x[[1L]]))
      n2 <- names(x)
      dnam <- if (!(iN1 && is.null(n2))) 
        c(if (iN1) rep.int(list(n1), length(c.dim)) else n1, 
          list(n2))
      array(r, dim = d, dimnames = dnam)
    }
    else if (prod(d <- c(common.len, n)) == length(r)) 
      array(r, dim = d, dimnames = if (!(is.null(n1 <- names(x[[1L]])) & 
                                         is.null(n2 <- names(x)))) 
        list(n1, n2))
    else x
  }
  else x
}

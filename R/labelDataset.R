labelDataset <- function(data) {
  correctLabel <- function(x) {
    if(!is.null(attributes(x)$labels)) {
      class(attributes(x)$labels) <- typeof(x)
    }
    return(x)
  }
  for(i in colnames(data)) {
    data[, i] <- correctLabel(data[, i])
  }
  return(data)
}
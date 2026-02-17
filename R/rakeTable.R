
rakeTable <- function(tab, cs, rs, iterations = 100) {
  for(k in 1:iterations) {
    currentcolsums <- colSums(tab, na.rm = TRUE)
    cs.ratios <- cs / currentcolsums 
    cs.ratios[cs.ratios==Inf] <- 1
    
    for(ii in 1:length(cs.ratios)) {
      tab[, ii] <- tab[, ii] * cs.ratios[ii]
    }
    
    currentrowsums <- rowSums(tab, na.rm = TRUE)
    rs.ratios <- rs / currentrowsums
    
    rs.ratios[rs.ratios==Inf] <- 1
    for(ii in 1:length(rs.ratios)) {
      tab[ii, ] <- tab[ii, ] * rs.ratios[ii]
    }
    diff <- sum(abs(cs - colSums(tab))) +   sum(abs(rs - rowSums(tab)))  
  }
  tab[is.na(tab)] <- 0
  return(tab)
}

calcPropNewEligUN2 <- function(country, prev.date, current.date, 
                               min.age.current, min.age.prev,
                               pops) {
  
  elec.gap <- as.numeric((current.date - prev.date) / 365)
  
  if(country=="NIR") {
    country <- "GB"
  }
  yearcurrent <- lubridate::year(current.date)
  
  age.current <- pops[which(pops$country==country & pops$Time==yearcurrent), ]
  age.current <- age.current[which(age.current$AgeGrp>=min.age.current), ]
  
  age.current$AgeGrp <- age.current$AgeGrp + 0.999
  age.current <- mellonMisc::slct(age.current, AgeGrp, PopTotal)
  
  if(any(duplicated(age.current))) {
    stop("Weird duplicate issue")
  }
  
  age.current <- rbind(mellonMisc::dtf(AgeGrp = min.age.current, PopTotal = 0), age.current)
  age.current$cumelig <- cumsum(age.current$PopTotal)
  age.current$years.elig <- age.current$AgeGrp - min.age.current
  elig.time <- elec.gap + (min.age.prev - min.age.current) 
  
  pred.new.elig <- approxfun(x = c(age.current$years.elig), y = c(age.current$cumelig))(elig.time)
  prop.new.elig <- pred.new.elig / sum(age.current$PopTotal)
  
  return(prop.new.elig)
}

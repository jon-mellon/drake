calcAgeDensity2 <- function(country, 
                            date,
                            bw.mult, 
                            min.age = 18,
                            max.age = 80, 
                            pops = NULL,
                            survey.n = 3000) {
  year <- lubridate::year(date)
  if(country=="NIR") {
    country <- "GB"
  }
  if(is.null(pops)) {
    pops <- popBySingleYear(country = country)
  }
  age.current <- pops[which(pops$country==country & pops$Time==year), ]
  age.current <- age.current[which(age.current$AgeGrp>=min.age), ]
  
  over.max.age <- age.current[age.current$AgeGrp>=max.age, ]
  age.current <- age.current[age.current$AgeGrp<max.age, ]
  
  over.max.age <- c(AgeGrp = max.age, colSums(over.max.age[, c("PopMale", "PopFemale", "PopTotal")]))
  over.max.age <- over.max.age[colnames(age.current)]
  names(over.max.age) <- colnames(age.current)
  age.current <- rbind(age.current, over.max.age)
  
  age.current$PopTotal <- round((age.current$PopTotal / sum(age.current$PopTotal)) * survey.n)
  data.temp <- inverse.rle(list(values = age.current$AgeGrp, lengths = age.current$PopTotal))
  
  age.density <- density(data.temp)
  bw <- age.density$bw
  
  age.density <- density(data.temp, bw = bw * bw.mult)
  return(age.density)
}

getExpectedMortality <- function(country, date1, date2, min.age, pops = popBySingleYear(),
                                 hmd.user, hmd.pw, 
                                 noisy = F) {
  pop.temp <- pops[which(pops$Time==lubridate::year(date1) & 
                           pops$country==country & 
                           pops$AgeGrp>=min.age), ]
  pop.temp$age <- pop.temp$AgeGrp
  pop.temp$gender <- "Male"
  gap <- as.numeric((date2 - date1) / 365)
  
  pop.temp$malemortality <- calculateMortality2(data = pop.temp, 
                                                country = country, 
                                                elec.gap = gap, 
                                                elec1.year = lubridate::year(date1), 
                                                suffix = "", 
                                                hmd.user = hmd.user, hmd.pw = hmd.pw, 
                                                mort.countries = getMortCountries(),  noisy= noisy)
  pop.temp$gender <- "Female"
  pop.temp$femalemortality <- calculateMortality2(data = pop.temp, 
                                                  country = country, 
                                                  elec.gap = gap, 
                                                  elec1.year = lubridate::year(date1), 
                                                  suffix = "", 
                                                  hmd.user = hmd.user, hmd.pw = hmd.pw, 
                                                  mort.countries = getMortCountries(),  
                                                  noisy= noisy)
  
  mortality <- (sum((pop.temp$PopFemale * pop.temp$femalemortality) * 1000) + 
                  sum((pop.temp$PopMale * pop.temp$malemortality) * 1000)) / 
    (sum(pop.temp$PopFemale + pop.temp$PopMale) * 1000)
  return(mortality)
}
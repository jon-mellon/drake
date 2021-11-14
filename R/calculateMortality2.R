
calculateMortality2 <- function(data, country, elec.gap, 
                                suffix, elec1.year, 
                                debug = F, hmd.user, hmd.pw, mort.countries, noisy) {
  if(debug) {
    browser()
  }
  data <- dtf(data)
  data$index <- 1:nrow(data)
  library(readr)
  library(lubridate)
  data$yob <-  elec1.year - data[, paste0("age", suffix)]
  
  country.mort <- mort.countries[country]
  
  if(!is.na(country.mort)) {
    mortality.table <- readHMDweb(CNTRY  = country.mort, 
                                  username = hmd.user, password = hmd.pw,
                                  item = "Mx_1x1")
    1
    current.mort.index <- suppressWarnings(indexMortality(mortality.table = mortality.table, 
                                                          index.year = elec1.year, 
                                                          elec.gap = elec.gap, 
                                                          valid.years = min(floor(data$yob), na.rm = T):max(ceiling(data$yob), na.rm = T),
                                                          noisy = noisy
    ))    
    current.mort.index <- current.mort.index[current.mort.index$gender!="Total", ]
    data$yob <- round(data$yob)
    data$gender <- data[, paste0("gender", suffix)]
    data$index <- 1:nrow(data)
    data <- safemerge(data, current.mort.index, 
                      by = c("yob", "gender"),
                      type = "m:1", all.x = T)
    
    data <- data[order(data$index), ]
  } else {
    warning(paste0("No mortality data for ", country))
    data$mortality <- NA
  }
  
  return(data$mortality)
}

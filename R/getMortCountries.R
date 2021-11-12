getMortCountries <- function() {
  mort.countries <- c(AT = "AUT", AU = "AUS", BE = "BEL", BR = NA, 
                      CA = "CAN", CH = "CHE", DE = "DEUTNP", 
                      DK = "DNK", ES = "ESP", FR = "FRATNP",
                      GB = "GBR_NP", IE =  "IRL", IL = "ISL",
                      IT = "ITA", JP = "JPN", NIR = "GBR_NIR",
                      NL = "NLD", NO = "NOR", NZ = "NZL_NP", 
                      PL = "POL", PT = "PRT", RO = "BGR", 
                      RU = "RUS", SE = "SWE", TW = "TWN", 
                      US = "USA")
  return(mort.countries)
}
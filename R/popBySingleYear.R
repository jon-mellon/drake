popBySingleYear <- function(source = c("wpp", "hmd"),
                            country,
                            mort.countries = getMortCountries(),
                            hmd.user = NULL, hmd.pw = NULL) {
  if(source[1]=="wpp") {
    pops <- tidywpp::get_wpp(indicator = c("PopTotal", "PopMale", "PopFemale"),
                             indicator_file_group = "PopulationBySingleAgeSex",
                             wpp_version = max(wpp_indicators$wpp))  
    countries <- unique(pops$Location)
    country.iso2c <- countrycode::countrycode(countries, origin = "country.name", destination = "iso2c")
    
    valid <- !is.na(country.iso2c) & !duplicated(country.iso2c)
    
    countries <- countries[valid]
    country.iso2c <- country.iso2c[valid]
    
    pops$country <- country.iso2c[match(pops$Location, countries)]
    pops <- pops[!is.na(pops$country), ]  
    pops$VarID <- NULL
    pops$Variant <- NULL
    pops$AgeGrpStart <- NULL
    pops$MidPeriod <- NULL
    pops$LocID <- NULL
    pops$Location <- NULL
    pops <- pops[pops$country==country, ]
    return(pops)
  }
  
  if(source[1]=="hmd") {
    country.mort <- mort.countries[country]
    
    itemlookup <- getHMDitemavail("USA", username = hmd.user, 
                                  password = hmd.pw)
    
    pop.table <- readHMDweb(CNTRY  = country.mort, 
                                  username = hmd.user, password = hmd.pw,
                                  item = "Population")
    
    pop.table$AgeGrpSpan <- ifelse(pop.table$OpenInterval, -1, 1)
    
    pop.table <- pop.table %>% mellonMisc::slct(Time = Year,
                                   AgeGrp = Age,
                                   PopFemale = Female1, 
                                   PopMale = Male1, 
                                   PopTotal = Total1) %>%
      mutate(PopFemale = PopFemale / 1000,
             PopMale = PopMale / 1000,
             PopTotal = PopTotal / 1000)
    pop.table$country <- country
    return(pop.table)
  }
  
  
}

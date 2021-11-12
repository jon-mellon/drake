popBySingleYear <- function() {
  pops <- tidywpp::get_wpp(indicator = c("PopTotal", "PopMale", "PopFemale"),
                           indicator_file_group = "PopulationBySingleAgeSex",
                           wpp_version = max(wpp_indicators$wpp))  
  countries <- unique(pops$Location)
  country.iso2c <- countrycode::countrycode(countries, origin = "country.name", destination = "iso2c")
  pops$country <- country.iso2c[match(pops$Location, countries)]
  return(pops)
}
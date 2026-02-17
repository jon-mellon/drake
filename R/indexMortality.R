#' @export indexMortality
indexMortality <- function(mortality.table, index.year = 1967, valid.years, elec.gap, noisy = TRUE) {
	
	mortality.table$yob <- mortality.table$Year - mortality.table$Age
	if(max(mortality.table$Year)<(floor(index.year) + elec.gap)) {
		years.to.add <- ceiling((floor(index.year) + elec.gap) - max(mortality.table$Year)) + 1
		if(noisy) {
			message(paste("Sample included years not present in mortality data, shifting years by ", years.to.add))	
		}
		mortality.table$Year <- mortality.table$Year + years.to.add
		mortality.table$yob <- mortality.table$yob + years.to.add
	}
	
	mortality.table <- mortality.table[order(mortality.table$yob, mortality.table$Year), ]
	
	mortality.table$Female[which(mortality.table$Female > 0.99)] <- 0.98
	mortality.table$Male[which(mortality.table$Male > 0.99)] <- 0.98
	mortality.table$Total[which(mortality.table$Total > 0.99)] <- 0.98
	
	mortality.table <- mortality.table[mortality.table$Year >= floor(index.year), ]
	miss.open <- names(which(table(mortality.table$Year, mortality.table$OpenInterval)[, "TRUE"] == 0))
	
	min.yob <- tapply(mortality.table$yob, mortality.table$Year, min)
  
  for(ii in miss.open) {
  	mortality.table[mortality.table$Year==ii & mortality.table$yob==min.yob[ii], "OpenInterval"] <- TRUE
  }
	mortality.table$Male[mortality.table$OpenInterval & mortality.table$Male == 0] <- max(mortality.table$Male, na.rm = TRUE)
	mortality.table$Female[mortality.table$OpenInterval & mortality.table$Female == 0] <- max(mortality.table$Female, na.rm = TRUE)
	mortality.table$Total[mortality.table$OpenInterval & mortality.table$Total == 0] <- max(mortality.table$Total, na.rm = TRUE)
	
	mortality.table$Female[is.na(mortality.table$Female) & mortality.table$Age > 80] <- max(mortality.table$Female, na.rm = TRUE)
	mortality.table$Male[is.na(mortality.table$Male) & mortality.table$Age > 80] <- max(mortality.table$Male, na.rm = TRUE)
	mortality.table$Total[is.na(mortality.table$Total) & mortality.table$Age > 80] <- max(mortality.table$Total, na.rm = TRUE)
	
  for(year in unique(mortality.table$Year)) {
  	miss.years <- valid.years[!valid.years %in% mortality.table$yob[mortality.table$Year == year]]
  	extra.mort.row <- mortality.table[mortality.table$OpenInterval & mortality.table$Year == year, ]
  	
  	extra.mort.row <- extra.mort.row[rep(1, length(miss.years)), ]
  	extra.mort.row$yob <- miss.years
  	extra.mort.row$Age <- extra.mort.row$Year - extra.mort.row$yob
  	mortality.table <- rbind(mortality.table, extra.mort.row)
  }
  
	mortality.table <- mortality.table[mortality.table$yob %in% valid.years, ]
	
	mortality.table <- mortality.table[order(mortality.table$yob, mortality.table$Year), ]
  
  mortality.table$maleliferate <- 1 - mortality.table$Male
  mortality.table$femaleliferate <- 1 - mortality.table$Female
  mortality.table$totalliferate <- 1 - mortality.table$Total
  
  mortality.table$cumdeathfemale <- 1- unlist(tapply(mortality.table$femaleliferate, mortality.table$yob, cumprod))
  mortality.table$cumdeathmale <- 1- unlist(tapply(mortality.table$maleliferate, mortality.table$yob, cumprod))
  mortality.table$cumdeathtotal <- 1- unlist(tapply(mortality.table$totalliferate, mortality.table$yob, cumprod))
  
  mortality.table <- mortality.table[!is.na(mortality.table$cumdeathmale), ]
  mortality.table <- mortality.table[!is.na(mortality.table$cumdeathfemale), ]
  mortality.table <- mortality.table[!is.na(mortality.table$cumdeathtotal), ]
  
  pickCumMort <- function(cum, yearsgoneby) {
  	
  	cumd <- mellonMisc::dtf(cum = cum, yearsout = 1:length(cum))
  	cumd <- rbind(mellonMisc::dtf(cum = 0, yearsout = 0), cumd)
  	
  	if(any(cumd$yearsout==yearsgoneby)) {
  		return(cumd$cum[cumd$yearsout==yearsgoneby])
  	}
  	
  	year.diffs <- (yearsgoneby - cumd$yearsout)
  	
  	minmort <- cumd$cum[max(which(year.diffs>0))]
  	
  	if(min(which(year.diffs<0))!=Inf) {
  		maxmort <- cumd$cum[min(which(year.diffs<0))]	
  	} else {
  		maxmort <- 1
  	}
  	
  	# mod<- scam(data = cumd, formula = cum ~ s(yearsout, bs = "mpi"), 
  	# 		 family = "quasibinomial", knots =2)
  	mod<- loess(data = cumd, formula = cum ~ yearsout)
 	 	pred.dead <- predict(newdata = mellonMisc::dtf(yearsout = yearsgoneby),
  											 mod, type = "response")
  	
  	if(pred.dead>maxmort) {
  		return(maxmort)
  	}
  	if(pred.dead<minmort) {
  		return(minmort)
  	}
  	return(pred.dead)
  }
  
  
  # cum <- mortality.table[mortality.table$yob==1902, "cumdeathmale"]
  #  View(mortality.table[mortality.table$yob==1902, ])
  panel.mortality <- rbind(mellonMisc::dtf(mortality = tapply(mortality.table$cumdeathmale,
  																								mortality.table$yob, pickCumMort, yearsgoneby = elec.gap), 
  														 gender = "Male", yob = unique(mortality.table$yob)),
  												 mellonMisc::dtf(mortality = tapply(mortality.table$cumdeathfemale,
  												 											 mortality.table$yob, pickCumMort, yearsgoneby = elec.gap), 
  												 		gender = "Female", yob = unique(mortality.table$yob)),
  												 mellonMisc::dtf(mortality = tapply(mortality.table$cumdeathtotal,
  												 											 mortality.table$yob, pickCumMort, yearsgoneby = elec.gap), 
  												 		gender = "Total", yob = unique(mortality.table$yob)))
 
  return(panel.mortality)
}

pop.n = 10000
poor.rr = 0.3
rich.rr = 1
sample.n = 1000
big.hh.rich = F

hhs <- sample(1:pop.n, replace = T)
hhs <- as.numeric(factor(hhs))
hh.sizes <- table(hhs)[as.character(hhs)]

if(big.hh.rich) {
  wealth <- ifelse(rbinom(size = 1 , prob = (1 - (1 / hh.sizes)), n=pop.n), "Rich", "Poor")
} else {
  wealth <- ifelse(rbinom(size = 1 , prob = ((1 / hh.sizes)), n=pop.n), "Rich", "Poor")
  wealth <- ifelse(rbinom(size = 1 , prob = 0.5, n=pop.n), "Rich", "Poor")
}

 
vaccinated <- rbinom(prob = ifelse(wealth=="Rich", 0.8, 0.3), n = pop.n, size = 1)
vaccinated <- ifelse(vaccinated==1, "Vaccinated", "Unvaccinated")
hates.surveys <- ifelse(rbinom(prob = 0.6, n = pop.n, size = 1)==1, "Hates surveys", "Loves surveys")
rr <- rep(0.7, length(wealth))
rr[vaccinated=="Vaccinated" & wealth=="Rich"] <- 1
rr[vaccinated=="Unvaccinated" & wealth=="Poor"] <- 0.21
rr[vaccinated=="Unvaccinated" & wealth=="Rich"] <- 0.45
rr[hates.surveys=="Hates surveys"] <- 0.1

sampled.hh <- sample(unique(hhs), sample.n, replace = F)
sampled.people <- c()
for(ii in sampled.hh){
  sampled.people <- c(sampled.people, sample(rep(which(hhs==ii), 2), 1))
}

wealth.sampled <- wealth[sampled.people]
respondents <- sampled.people[
  rbinom(n= sample.n, prob = rr[sampled.people], size = 1)==1]
actual.rr <- length(respondents) / sample.n

data <- data.frame(wealth = wealth[sampled.people], 
           vaccinated = vaccinated[sampled.people], 
           hhsize = as.numeric(hh.sizes[sampled.people]), 
           hates.surveys = hates.surveys[sampled.people])


discrete.targets <- list(vaccinated = prop.table(table(vaccinated)), 
     wealth = prop.table(table(wealth)), 
     hates.surveys = prop.table(table(hates.surveys)))

library(drake)

data$selection.weights <- data$hhsize / mean(data$hhsize)
data$weight <- drake(sample = data,   initial.weights = data$selection.weights, 
                     RR = actual.rr, 
                     selection.weights = FALSE, 
                     discrete.targets  =discrete.targets)

data$newweight <- drake(sample = data,   initial.weights = data$selection.weights, 
      RR = actual.rr, 
      selection.weights = TRUE, 
      discrete.targets  =discrete.targets)

library(questionr)
discrete.targets
prop.table(wtd.table(data$wealth, weights= data$weight))
prop.table(wtd.table(data$vaccinated, weights= data$weight))

sd(data$weight)
sd(data$newweight)

min(data$newweight/data$selection.weights)



min(data$weight/data$selection.weights)

library(haven)
anes.methods <- read_stata("C:/Dropbox/drake/data/anes_timeseries_2016_methodology_dta.dta")
anes <- read_stata("C:/Dropbox/drake/data/anes_timeseries_2016.dta")
anes.methods$weight_ftfbwt0[anes.methods$weight_ftfbwt0==0] <- NA
anes$baseweight <- anes.methods$weight_ftfbwt0[match(anes$V160001_orig, anes.methods$V160001_orig)]

table(is.na(anes.methods$weight_ftfbwt0))
anes
table(is.na(anes$baseweight))
# table(anes$V160001_orig %in% anes.methods$V160001_orig)

(1:5)[match(10:1, 1:5)]

table(is.na(anes$baseweight))

anes$selwt <- anes$baseweight / mean(anes$baseweight, na.rm = T)

table(is.na(anes$V160101))
anes$V160101

anes$ratio <- anes$V160101 / anes$selwt
anes$ratio_adj <- anes$ratio / mean(anes$ratio, na.rm = T)
min(anes$ratio_adj, na.rm = T)

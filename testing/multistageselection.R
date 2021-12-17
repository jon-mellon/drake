pop.n = 10000
poor.rr = 0.3
rich.rr = 1
sample.n = 1000
big.hh.rich = T

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
hates.surveys <- ifelse(rbinom(prob = 0.3, n = pop.n, size = 1)==1, 
                        "Hates surveys", "Loves surveys")
hates.phones <- ifelse(rbinom(prob = 0.3, n = pop.n, size = 1)==1, 
                       "Hates phones", "Loves phones")
rr <- rep(0.2, length(wealth))
rr[vaccinated=="Unvaccinated" & wealth=="Poor"] <- 0.1
rr[vaccinated=="Unvaccinated" & wealth=="Rich"] <- 0.25
rr[hates.surveys=="Loves surveys"] <- 0.95
rr[hates.phones=="Loves phones"] <- 0.95
rr[vaccinated=="Vaccinated" & wealth=="Rich" ] <- 1

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
                   hates.surveys = hates.surveys[sampled.people], 
                   hates.phones = hates.phones[sampled.people])


discrete.targets <- list(vaccinated = prop.table(table(vaccinated)), 
                         wealth = prop.table(table(wealth)), 
                         hates.surveys = prop.table(table(hates.surveys)),
                         hates.phones = prop.table(table(hates.phones)))

library(drake)

data$selection.weights <- data$hhsize / mean(data$hhsize)
data$weight <- drake(sample = data,   initial.weights = data$selection.weights, 
                     RR = actual.rr, 
                     selection.weights = FALSE, 
                     discrete.targets  =discrete.targets)

data$newweight <- drake(sample = data,
                        initial.weights = data$selection.weights, 
                        RR = actual.rr, 
                        selection.weights = TRUE, 
                        discrete.targets  =discrete.targets)

ratiosnew <- data$newweight/data$selection.weights
ratiosnew <- ratiosnew / mean(ratiosnew)

ratiosold <- data$weight / data$selection.weights
ratiosold <- ratiosold / mean(ratiosold)

actual.rr
min(ratiosnew)
min(ratiosold)


sd(data$weight)
sd(data$newweight)

library(questionr)
discrete.targets
prop.table(wtd.table(data$wealth, weights= data$weight))
prop.table(wtd.table(data$vaccinated, weights= data$weight))
prop.table(wtd.table(data$hates.surveys, weights= data$weight))

min(data$newweight)

data$newweight[data$hates.phones =="Hates phones" & 
                 data$hates.surveys=="Hates surveys"]


rm(list = ls())
library(haven)
library(drake)

anes.methods <- read_stata("C:/Dropbox/drake/data/anes_timeseries_2016_methodology_dta.dta")
anes <- read_stata("C:/Dropbox/drake/data/anes_timeseries_2016.dta")

anes.methods$weight_aprenrwgt0[anes.methods$weight_prenrfct==0] <- NA

anes$baseweight <- anes.methods$weight_aprenrwgt0[match(anes$V160001_orig, anes.methods$V160001_orig)]

anes$age_sex <- as_factor(anes.methods$weight_age_sex[match(anes$V160001_orig, anes.methods$V160001_orig)])
anes$age_sex <- factor(anes$age_sex)
anes$racethn_educ <- as_factor(anes.methods$weight_racethn_educ[match(anes$V160001_orig, anes.methods$V160001_orig)])
anes$racethn_educ <- factor(anes$racethn_educ)

anes$maritl_sex <- as_factor(anes.methods$weight_maritl_sex[match(anes$V160001_orig, anes.methods$V160001_orig)])
anes$maritl_sex <- factor(anes$maritl_sex)

anes$racethn_region <- as_factor(anes.methods$weight_racethn_region[match(anes$V160001_orig, anes.methods$V160001_orig)])
anes$racethn_region <- factor(anes$racethn_region)

anes$nation <- as_factor(anes.methods$weight_nation[match(anes$V160001_orig, anes.methods$V160001_orig)])
anes$nation <- factor(anes$nation)

anes$tenure_urban <- as_factor(anes.methods$weight_tenure_urban[match(anes$V160001_orig, anes.methods$V160001_orig)])
anes$tenure_urban <- factor(anes$tenure_urban)
levels(anes$maritl_sex)
levels(anes$nation)
levels(anes$racethn_region)
anes <- anes[!is.na(anes$baseweight), ]

discrete.targets <- list(age_sex = c(`1. male 18-39` = 40810968.39, 
                 `2. female 18-39` = 41912891.16, 
                 `3. male 40-59` = 36749843.34,
                 `4. female 40-59` = 38905301.04, 
                 `5. male 60+` = 29993020.05, 
                 `6. female 60+` = 35686981.10), 
                 racethn_educ = c(`1. Hisp less than HS` = 5023482.73, 
                      `2. Hisp HS` = 8350891.83, 
                      `3. Hisp HS+` = 13287919.38,
                      `4. Black less than HS` = 3394149.78, 
                      `5. Black HS` = 9300184.94, 
                      `6. Black HS+` = 14831522.87, 
                      `7. other less than HS` = 11940364.38,
                      "8. other HS"   = 47866980.93, "9. other HS+"  = 110063508.24),
                 maritl_sex =  c("1. Married male" = 59110107.66, 
                   "2. Married female" = 58101736.80, 
                   "3. others male" = 15342663.08, 
                   "4. others female" = 28314696.45, 
                   "5. single male" = 33101061.04, 
                   "6. single female" = 30088740.05),
                 racethn_region = c("1. Hisp NorthEast" = 3896543.75, 
                   "2. Hisp MidWest" = 2355788.18, 
                   "3. Hisp South" = 9738856.33, 
                   "4. Hisp West" = 10671105.68, 
                   "5. Black NE"=  4201578.97,
                   "6. Black MW"= 4784757.17, 
                   "7. Black S" = 16068129.85, 
                   "8. Black W" = 2471391.61, 
                   "9. other NE" = 31961295.67, 
                   "10. other MW" = 42115734.59, 
                   "11. other S" = 58350452.31, 
                   "12. other W" = 37443370.97), 
                 nation = c("1. US born"  = 204212112.89, 
                            "2. Foreign born" =  19846892.19), 
                 tenure_urban = c("1. Not rented, urban"= 133347764.47, 
                                  "2. Not rented, non-urban" = 25499294.22, 
                                  "3. Rented, urban" = 57629146.74, 
                                  "4. Rented, non-urban" = 7582799.65)
     )


# 0.554

discrete.targets <- lapply(discrete.targets, prop.table)
anes$selwt <- anes$baseweight / mean(anes$baseweight, na.rm = T)
library(questionr)
library(anesrake)

anes <- anes[!is.na(anes$selwt), ]
anes <- anes[complete.cases(anes[, names(discrete.targets)]), ]

# weights
anes <- data.frame(anes)
weight.out <- anesrake(inputter = discrete.targets, 
         dataframe = anes, weightvec = anes$selwt, 
         caseid = anes$V160001_orig, cap = Inf)
anes$anesrakeweight <- weight.out$weightvec

anes$V160101f[anes$V160101f==0] <- NA
anes$ftfweight <- anes$V160101f / mean(anes$V160101f, na.rm = T)
anes$testweight <- drake(sample = anes, discrete.targets = discrete.targets, 
                         initial.weights = anes$selwt)
plot(anes$anesrakeweight, anes$ftfweight)
plot(anes$ftfweight, anes$testweight)
plot(anes$testweight, anes$anesrakeweight)

cor(anes$ftfweight, anes$testweight, use = "pairwise.complete.obs")

anes$fixedweight <- drake(sample = anes, discrete.targets = discrete.targets, 
                         initial.weights = anes$selwt, 
                         selection.weights = TRUE, RR = 0.554)

anes$ratiotest <- anes$testweight / anes$selwt
anes$ratiostartest <- anes$ratiotest / mean(anes$ratiotest, na.rm = T)
min(anes$ratiostartest, na.rm = T)

hist(anes$testweight)
hist(anes$fixedweight)
hist(anes$ftfweight)

calc_efficiency(na.omit(anes$fixedweight))
calc_efficiency(na.omit(anes$ftfweight))
calc_efficiency(na.omit(anes$testweight))
anes$fixedratio <- anes$fixedweight / anes$selwt
anes$fixedratiostar <- anes$fixedratio / mean(anes$fixedratio, na.rm = T)

anes$ftfweightratio <- anes$ftfweight / anes$selwt
anes$ftfweightratiostar <- anes$ftfweightratio / mean(anes$ftfweightratio, na.rm = T)

min(anes$fixedratiostar)
min(anes$ftfweightratiostar)
# table(anes$ftfweightratiostar<0.554)

ess <- function(x) {
  x <- na.omit(x)
  sum(x)^2 / sum(x^2)
}

max(anes$fixedweight, na.rm = T)
max(anes$testweight, na.rm = T)
max(anes$ftfweight, na.rm = T)

ess(anes$fixedweight)
ess(anes$testweight)
ess(anes$ftfweight)

min(anes$fixedweight, na.rm = T)
min(anes$testweight, na.rm = T)

mean(anes$testweight, na.rm = T)
mean(anes$ftfweight, na.rm = T)
mean(anes$ftfweight, na.rm = T)

table(is.na(anes$testweight))
table(is.na(anes$ftfweight))

min(anes$ratiostartest, na.rm = T)

discrete.targets$racethn_educ
prop.table(wtd.table(anes$racethn_educ, weights =  anes$anesrakeweight))
prop.table(wtd.table(anes$racethn_educ, weights =  anes$fixedweight))


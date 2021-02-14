rm(list=ls())
#-----------------IMPORTING LIBRARIES-----------------------------------

library(readxl)
library(plyr)
library(tidyr)
library(lme4) 
library(dplyr)

#-----------------IMPORTING THE DATASETS------------------------------------
#-----------------IMPORTING HVP PERFORMACE SCORES---------------------------

perf.score.dataset = read_excel("performance_scores.xlsx")
colnames(perf.score.dataset) = tolower(make.names(colnames(perf.score.dataset)))
attach(perf.score.dataset)
summary(perf.score.dataset)
nrow(perf.score.dataset)
ncol(perf.score.dataset)

#-----------------IPPS FY 2017 DATA---------------------------

ipps.payment.dataset = read_excel("IPPS FY 2017 Data.xlsx")
colnames(ipps.payment.dataset) = tolower(make.names(colnames(ipps.payment.dataset)))
attach(ipps.payment.dataset)
summary(ipps.payment.dataset)
nrow(ipps.payment.dataset)
ncol(ipps.payment.dataset)

#-----------------Statewise Population---------------------------

statewise_population = read_excel("State County Level Population 2017.xlsx")
colnames(statewise_population) = tolower(make.names(colnames(statewise_population)))
attach(statewise_population)
summary(statewise_population)
nrow(statewise_population)
ncol(statewise_population)

#----------------- Statewise Average Income ---------------------------

income.dataset = read_excel("house_income.xlsx")
colnames(income.dataset) = tolower(make.names(colnames(income.dataset)))
attach(income.dataset)
summary(income.dataset)
nrow(income.dataset)
ncol(income.dataset)

#-----------------RENAMING VARIABLE/FEATURES TO MAKE THEM CONSISTENT----------------

names(ipps.payment.dataset)
names(perf.score.dataset)

names(perf.score.dataset)[names(perf.score.dataset) == "ï..facility.id"] <- "facility.id"
names(ipps.payment.dataset)[names(ipps.payment.dataset) == "provider.id"] <- "facility.id"

income.dataset$state = state.abb[match(income.dataset$state,state.name)]
statewise_population$state = state.abb[match(statewise_population$state,state.name)]

#-----------------PERFORMING INNER JOIN TO COMBINE BOTH DATASETS BASED ON 'FACILITY_ID'-----------

master.dataset = join(ipps.payment.dataset, perf.score.dataset,
                      type = "inner")
state_merge_demo = join(statewise_population, income.dataset,
                      type = "inner")
master.dataset = join(master.dataset, state_merge_demo,
                      type = "inner")

#--------------------DATA PREPROCESSING-----------------------------------------------------

names(master.dataset)
master.dataset = separate(data = master.dataset, col = drg.definition, into = c("drg.code", "drg.description"), sep = "\\-")
master.dataset = separate(data = master.dataset, col = hospital.referral.region..hrr..description, into = c("hospital.state", "hospital.region"), sep = "\\-")

#---------------IMPORTING FINAL PREPROCESSED DATA INTO A NEW DATAFRAME-----------------------------------------

clean.dataset = subset(master.dataset, select = -c(facility.name, address, city, state, zip.code, location, hospital.state, hospital.region))

clean.dataset <- clean.dataset[!(clean.dataset$unweighted.normalized.clinical.outcomes.domain.score == "Not Available"
                                  | clean.dataset$weighted.normalized.clinical.outcomes.domain.score == "Not Available"
                                  | clean.dataset$unweighted.person.and.community.engagement.domain.score == "Not Available"
                                  | clean.dataset$weighted.person.and.community.engagement.domain.score == "Not Available"
                                  | clean.dataset$unweighted.normalized.safety.domain.score == "Not Available"
                                  | clean.dataset$weighted.safety.domain.score == "Not Available"
                                  | clean.dataset$unweighted.normalized.efficiency.and.cost.reduction.domain.score == "Not Available"
                                  | clean.dataset$weighted.efficiency.and.cost.reduction.domain.score == "Not Available"
                                 ),]

#---------------------- Generating Features ------------------------------

clean.dataset$average.outofpocket.payments = (clean.dataset$average.total.payments - clean.dataset$average.medicare.payments)
clean.dataset$average.perc.outofpocket.payments = ((clean.dataset$average.total.payments - clean.dataset$average.medicare.payments) / clean.dataset$average.total.payments)*100
clean.dataset$average.extra.payments = clean.dataset$average.covered.charges - clean.dataset$average.total.payments

#----------------------converting scores to numeric and removing outlier values--------------

clean.dataset$unweighted.normalized.clinical.outcomes.domain.score = as.numeric(clean.dataset$unweighted.normalized.clinical.outcomes.domain.score)
clean.dataset$weighted.normalized.clinical.outcomes.domain.score = as.numeric(clean.dataset$weighted.normalized.clinical.outcomes.domain.score)
clean.dataset$unweighted.person.and.community.engagement.domain.score = as.numeric(clean.dataset$unweighted.person.and.community.engagement.domain.score)
clean.dataset$weighted.person.and.community.engagement.domain.score = as.numeric(clean.dataset$weighted.person.and.community.engagement.domain.score)
clean.dataset$unweighted.normalized.safety.domain.score = as.numeric(clean.dataset$unweighted.normalized.safety.domain.score)
clean.dataset$unweighted.normalized.efficiency.and.cost.reduction.domain.score = as.numeric(clean.dataset$unweighted.normalized.efficiency.and.cost.reduction.domain.score)
clean.dataset$weighted.safety.domain.score = as.numeric(clean.dataset$weighted.safety.domain.score)
is.num <- sapply(clean.dataset, is.numeric)
is.num
clean.dataset[is.num] <- lapply(clean.dataset[is.num], round, 4)

#--------------------------------------Scaling variables-------------------------------------------
clean.dataset$total.discharges = scale(clean.dataset$total.discharges)
clean.dataset$weighted.normalized.clinical.outcomes.domain.score = scale(clean.dataset$weighted.normalized.clinical.outcomes.domain.score)
clean.dataset$weighted.efficiency.and.cost.reduction.domain.score = scale(clean.dataset$weighted.efficiency.and.cost.reduction.domain.score)
clean.dataset$weighted.person.and.community.engagement.domain.score = scale(clean.dataset$weighted.person.and.community.engagement.domain.score)
clean.dataset$weighted.safety.domain.score = scale(clean.dataset$weighted.safety.domain.score)

clean.dataset$scale.average.covered.charges = scale(clean.dataset$average.covered.charges)
clean.dataset$scale.average.medicare.payments = scale(clean.dataset$average.medicare.payments)
clean.dataset$scale.average.total.payments = scale(clean.dataset$average.total.payments)
clean.dataset$scale.average.extra.payments = scale(clean.dataset$average.extra.payments)
clean.dataset$scale.average.outofpocket.payments = scale(clean.dataset$average.outofpocket.payments)

clean.dataset$log.average.medicare.payments = log(clean.dataset$average.medicare.payments)
clean.dataset$log.average.total.payments = log(clean.dataset$average.total.payments)
clean.dataset$log.average.covered.charges = log(clean.dataset$average.covered.charges)
clean.dataset$log.average.outofpocket.payments = log(clean.dataset$average.extra.payments)
clean.dataset$log.average.extra.payments = log(clean.dataset$average.extra.payments)


attach(clean.dataset)
colSums(is.na(clean.dataset))
clean.dataset = clean.dataset[!(is.na(clean.dataset$log.average.outofpocket.payments) | clean.dataset$log.average.outofpocket.payments==""), ]
clean.dataset = clean.dataset[!(is.na(clean.dataset$log.average.extra.payments) | clean.dataset$log.average.extra.payments==""), ]
clean.dataset = clean.dataset[!(is.na(clean.dataset$log.average.medicare.payments) | clean.dataset$log.average.medicare.payments==""), ]
clean.dataset = clean.dataset[!(is.na(clean.dataset$log.average.total.payments) | clean.dataset$log.average.total.payments==""), ]
clean.dataset = clean.dataset[!(is.na(clean.dataset$log.average.covered.charges) | clean.dataset$log.average.covered.charges==""), ]
colSums(is.na(clean.dataset))


#-------------------storing updated dataframe into the file for reference-------------
#write.csv(clean.dataset, "project_dataset.csv")
#dataset = read.csv("project_combined_dataset_final.csv")

#-------------------DATA EXPLORATION-----------------------------------------------

#-------------------EXLORING THE DISTRIBUTION OF THE PREDICTORS------------------------------------------

hist(total.discharges, col="skyblue") #Rightskewed
hist(log(total.discharges), col="green") #Looks better hence could be used as the predictor. 

hist(average.covered.charges , col="green") 
hist(log.average.covered.charges, col="violet") 

hist(average.medicare.payments, col="yellow") 
hist(log.average.medicare.payments, col="red") 

hist(average.outofpocket.payments, col="pink") 
hist(log.average.outofpocket.payments, col="violet") 

hist(average.extra.payments, col="gray") 
hist(log.average.extra.payments, col="brown") 

hist(average.total.payments, col="gray") 
hist(log.average.total.payments, col="brown") 


#-------------------CONVERTING CATEGORICAL VARIABLES INTO FACTORS----------------------------------------
clean.dataset$drg.code <- factor(clean.dataset$drg.code)
levels(clean.dataset$drg.code)

clean.dataset$provider.city <- factor(clean.dataset$provider.city)
levels(clean.dataset$provider.city)

clean.dataset$provider.state <- factor(clean.dataset$provider.state)
levels(clean.dataset$provider.state)

clean.dataset$county.name <- factor(clean.dataset$county.name)
levels(clean.dataset$county.name)

clean.dataset$facility.id <- factor(clean.dataset$facility.id)
levels(clean.dataset$facility.id)

#---------------- Random Effect Model Average Payment varying across DRG codes in Different states
   
#1) How much Average Medicare Payment  Vary by state as per DRG code?

avg.med.care.state <- lmer(log.average.medicare.payments ~ log(total.discharges)
                           + scale.average.covered.charges + scale.average.extra.payments
                           + scale.average.outofpocket.payments + scale.average.total.payments
                           + weighted.efficiency.and.cost.reduction.domain.score + weighted.normalized.clinical.outcomes.domain.score
                           + weighted.person.and.community.engagement.domain.score +weighted.safety.domain.score
                           + (1 | provider.state/drg.code), data=clean.dataset, REML=FALSE)
summary(avg.med.care.state)  

confint(avg.med.care.state)
AIC(avg.med.care.state)
fixef(avg.med.care.state)                                       # Magnitude of fixed effect
ranef(avg.med.care.state)                                       # Magnitude of random effect
coef(avg.med.care.state) 

#2) Which facility ID and DRG code within Florida charge maximum extra payments to the patients?

avg.fl.ext.charges <- lmer(average.extra.payments ~ log(total.discharges) 
                           + scale.average.covered.charges + scale.average.total.payments
                           + scale.average.outofpocket.payments + scale.average.medicare.payments
                           + weighted.efficiency.and.cost.reduction.domain.score + weighted.normalized.clinical.outcomes.domain.score
                           + weighted.person.and.community.engagement.domain.score +weighted.safety.domain.score
                           + (1 | facility.id) + (1 | drg.code), data=subset(clean.dataset, provider.state == "FL"), REML=FALSE)
summary(avg.fl.ext.charges) 

avg.fl.ext.charges <- lmer(average.extra.payments ~ log(total.discharges) 
                           + scale.average.covered.charges + scale.average.total.payments
                           + scale.average.outofpocket.payments + scale.average.medicare.payments
                           + weighted.efficiency.and.cost.reduction.domain.score + weighted.normalized.clinical.outcomes.domain.score
                           + weighted.person.and.community.engagement.domain.score +weighted.safety.domain.score
                           + (1 | facility.id) + (1 | drg.code), data=subset(clean.dataset, provider.state == "FL"), REML=FALSE)
summary(avg.fl.ext.charges)  

confint(avg.fl.ext.charges)
AIC(avg.fl.ext.charges)
fixef(avg.fl.ext.charges)                                       # Magnitude of fixed effect
ranef(avg.fl.ext.charges)                                       # Magnitude of random effect
coef(avg.fl.ext.charges) 

#3) How much out of pocket expense vary according to the drg code by state?

avg.out.pock.exp.state <- lmer(log.average.outofpocket.payments ~ log(total.discharges) 
                           + scale.average.covered.charges + scale.average.extra.payments
                           + scale.average.medicare.payments + scale.average.total.payments
                           + weighted.efficiency.and.cost.reduction.domain.score + weighted.normalized.clinical.outcomes.domain.score
                           + weighted.person.and.community.engagement.domain.score + weighted.safety.domain.score
                           + (1 | provider.state/drg.code), data=clean.dataset, REML=FALSE)
summary(avg.out.pock.exp.state)  

confint(avg.out.pock.exp.state)
AIC(avg.out.pock.exp.state)
fixef(avg.out.pock.exp.state)                                       # Magnitude of fixed effect
ranef(avg.out.pock.exp.state)                                       # Magnitude of random effect
coef(avg.out.pock.exp.state) 


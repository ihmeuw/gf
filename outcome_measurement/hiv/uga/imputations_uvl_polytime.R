# ----------------------------------------------
# Caitlin O'Brien-Carelli

# 5/8/2018
# Multiple imputation for the Uganda Viral Load Dashboard using polytime
# Created for use on the cluster
# ----------------------------------------------
# Set up R

rm(list=ls())
library(data.table)
library(rgeos)
library(raster)
library(ggplot2)
library(rgdal)
library(tibble)
library(dplyr)
library(RColorBrewer)
library(Amelia)
library(MASS)
library(gtools)

# set input/output directory
# ----------------------------------------------
dir <- '/home/j/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/imputed'

# upload the data with month, year, sex
uvl1 <- readRDS(paste0(dir, "/sex_data.rds"))
uganda_vl <- uvl1

# start from scale up and continue through febrary 2018
uganda_vl <- uganda_vl[!(month==3 & year==2018)]
uganda_vl <- uganda_vl[year==2016 | year==2017 | year==2018]

uganda_vl[sex=='Female', sex1:=1]
uganda_vl[sex=='Male', sex1:=2]
uganda_vl[sex=='Uknown', sex1:=3]
uganda_vl[ ,combine:= paste0(facility_id, '_', date, '_', sex1)]
uganda_vl[order(combine)]
uganda_vl[duplicated(combine)]
uganda_vl[,combine:=NULL]
uganda_vl[,sex1:=NULL]

# ----------------------------------------------
# prep the data for imputation 

# --------------
# add an expanded data set that has missing rows for missing values

# store list of unique faciltiies, sexes and dates
f_ids <- unique(uganda_vl$facility_id)
length(f_ids)
sexes <- unique(uganda_vl$sex)
dates <- seq(from=min(uganda_vl$date), to=max(uganda_vl$date), by='month')

# make a "fully rectangularized" dataset with all months for each facility-sex
expanded_data <- data.table(expand.grid(f_ids, dates, sexes))
setnames(expanded_data, c('facility_id', 'date', 'sex'))
# --------------------

# merge in the blank rows for facility_id (by date, sex)
uganda_vl <- merge(uganda_vl, expanded_data, by=c('facility_id', 'date', 'sex'), all=TRUE)

#--------------------
# check for duplicats
uganda_vl[sex=='Female', sex1:=1]
uganda_vl[sex=='Male', sex1:=2]
uganda_vl[sex=='Uknown', sex1:=3]
uganda_vl[ ,combine:= paste0(facility_id, '_', date, '_', sex1)]
uganda_vl[duplicated(combine)]

#-------------------
# collapse the duplicate entries to only single values
# this only matters if there are duplicate entries - should be none (perform as a check)
sumVars <- c("patients_received", "samples_received", "rejected_samples", "plasma_samples",
             "dbs_samples", "samples_tested", "valid_results", "suppressed")

x <- uganda_vl[, lapply(.SD, sum, na.rm=TRUE), by=c('facility_id', 'facility_name', 'dhis2name', 
                                                    'sex', 'date', 'district_id', 'dist_name', 'level'),
               .SDcols=sumVars]

# delete the variables used to check for duplicate entries
uganda_vl[, combine:=NULL]
uganda_vl[ , sex1:=NULL]

#-------------------

#-------------------
# run a for loop that fills in missing identifier values

ids <- uganda_vl[!is.na(facility_name), .(facility_id=unique(facility_id)),  
                 by=.(facility_name, level, dhis2name, district_id, dist_name)]

for (f in ids$facility_id) {
  uganda_vl <- uganda_vl[is.na(facility_name) & facility_id==f, level:=ids[facility_id==f]$level]  
  uganda_vl <- uganda_vl[is.na(facility_name) & facility_id==f, dhis2name:=ids[facility_id==f]$dhis2name]   
  uganda_vl <- uganda_vl[is.na(facility_name) & facility_id==f, district_id:=ids[facility_id==f]$district_id]   
  uganda_vl <- uganda_vl[is.na(facility_name) & facility_id==f, dist_name:=ids[facility_id==f]$dist_name] 
  uganda_vl <- uganda_vl[is.na(facility_name) & facility_id==f, facility_name:=ids[facility_id==f]$facility_name]
  
  print(f)
}

#-------------------
# add zeroes to the data where one sex is present but not the other
# male data but missing females

# add zeroes for places where one sex is present but the other missing
uganda_vl[ ,combine:= paste0(facility_id, '_', date)]

# male data but missing females
missing_females <- uganda_vl[sex=='Female' & is.na(patients_received), .(combine=combine)]

males <- uganda_vl[sex=='Male']
males <- males[!is.na(patients_received), .(combine)]
males <- merge(males, missing_females, by='combine')

males[ , missing_females:=1]
uganda_vl <- merge(uganda_vl, males, by='combine', all.x=TRUE)
uganda_vl[is.na(missing_females), missing_females:=0]

# 
# male_data <- uganda_vl[missing_females==1 & sex=='Male', .(total_pts = sum(patients_received)), by=.(facility_id, facility_name)]
# male_data <- male_data[order(total_pts, decreasing=T)]
# male_data
# 
# write.csv(male_data, file=paste0(dir,"/males_only.csv"))

# # add zeroes for all the missing female entries where these is male data
# uganda_vl[missing_females==1, patients_received:=0] 
# uganda_vl[missing_females==1, samples_received:=0] 
# uganda_vl[missing_females==1, rejected_samples:=0] 
# uganda_vl[missing_females==1, plasma_samples:=0] 
# uganda_vl[missing_females==1, dbs_samples:=0] 
# uganda_vl[missing_females==1, samples_tested:=0] 
# uganda_vl[missing_females==1, suppressed:=0] 
# uganda_vl[missing_females==1, valid_results:=0] 

# female data but missing males
missing_males <- uganda_vl[sex=='Male' & is.na(patients_received), .(combine=combine)]

females <- uganda_vl[sex=='Female']
females <- females[!is.na(patients_received), .(combine)]
females <- merge(females, missing_males, by='combine')

females[ , missing_males:=1]
uganda_vl <- merge(uganda_vl, females, by='combine', all.x=TRUE)
uganda_vl[is.na(missing_males), missing_males:=0]
# 
# female_data <- uganda_vl[missing_males==1 & sex=='Female', .(total_pts = sum(patients_received)), by=.(facility_id, facility_name)]
# female_data <- female_data[order(total_pts, decreasing=T)]
# female_data
# 
# write.csv(female_data, file=paste0(dir,"/females_only.csv"))
# 

# add zeroes for all the missing female entries where these is male data
uganda_vl[missing_males==1, patients_received:=0] 
uganda_vl[missing_males==1, samples_received:=0] 
uganda_vl[missing_males==1, rejected_samples:=0] 
uganda_vl[missing_males==1, plasma_samples:=0] 
uganda_vl[missing_males==1, dbs_samples:=0] 
uganda_vl[missing_males==1, samples_tested:=0] 
uganda_vl[missing_males==1, suppressed:=0] 
uganda_vl[missing_males==1, valid_results:=0] 

#---------------------
# check for duplicates
# create a unique identifier (char) of facilityid_date_sex
uganda_vl[sex=="Female", sex1:=1 ]
uganda_vl[sex=="Male", sex1:=2]
uganda_vl[sex=="Unknown", sex1:=3]

uganda_vl[ ,combine1:= paste0(facility_id, '_', date, '_', sex1)]
uganda_vl[,length(unique(combine1))] 

uganda_vl[duplicated(combine1)] # no duplicates
uganda_vl[,combine1:=NULL] # then delete the identifier
uganda_vl[,combine:=NULL]
uganda_vl[,sex1:=NULL]


#---------------------
# descriptives
uganda_vl[missing_females==1, .(sex, sum(patients_received)), by=facility_id]


# ----------------------------------------------
# prep the data for the imputation

#------------------------
# change integers to doubles
# drop out month, year and leave date 

Vars <- c("patients_received", "samples_received", "rejected_samples","dbs_samples", "plasma_samples",
          "samples_tested", "suppressed", "valid_results")


uganda_vl <- uganda_vl[,lapply(.SD, as.double), by=c('facility_id', 'facility_name', 'dhis2name', 
                                                     'district_id', 'dist_name', 'sex', 'date', 'missing_males', 'missing_females', 'level'), 
                       .SDcols=Vars]


#------------------------
# calculate a suppression ratio
uganda_vl[suppressed > valid_results] # check equality constraint
uganda_vl[, sup_ratio:=(suppressed/valid_results)]

# alter valid results in the denominator to be the district suppression ratio
uganda_vl[valid_results==0, valid_results0:=T]
uganda_vl[!is.na(valid_results) & valid_results>0, valid_results0:=F]

# #compute district average ratio among nonzeroes
uganda_vl[valid_results0==FALSE, district_ratio:=mean(sup_ratio, na.rm=TRUE), by='district_id']

uganda_vl[, district_ratio:=mean(sup_ratio, na.rm=TRUE), by='district_id']

# replace ratio to district average when the number of samples was zero
uganda_vl[valid_results0==TRUE, sup_ratio:=district_ratio]

#---------------------------
# create a variable marking which rows to impute
uganda_vl[is.na(patients_received), imputed:=1]
uganda_vl[!is.na(patients_received), imputed:=0]

#---------------------------

#---------------------------
# transform the suppression ratio

# keep track of 0s and 1s
hist(uganda_vl$sup_ratio)

uganda_vl[sup_ratio==0, ratio0:=T] 
uganda_vl[sup_ratio>0, ratio0:=F] 

uganda_vl[sup_ratio==1, ratio1:=T] 
uganda_vl[sup_ratio<1, ratio1:=F] 

#---------------
# transform 0s and 1s

# replace suppression ratio = 1 with the maximum possible value < 1
uganda_vl[, max_ratio:= uganda_vl[sup_ratio<1, max(sup_ratio, na.rm=T)]]
uganda_vl[ratio1==T, sup_ratio:=max_ratio]

# replace suppression ratio = 0 with the minimum possible value < 1
uganda_vl[ ,min_ratio:= uganda_vl[sup_ratio>0, min(sup_ratio, na.rm=T)]]
uganda_vl[ratio0==T, sup_ratio:=min_ratio]           

# check that no 0s or 1s remain
uganda_vl[, min(sup_ratio, na.rm=T)]
uganda_vl[, max(sup_ratio, na.rm=T)]
summary(uganda_vl$sup_ratio)
hist(uganda_vl$sup_ratio)

#---------------
# logit transform the ratio
uganda_vl[ , sup_ratio:=logit(sup_ratio)]
hist(uganda_vl$sup_ratio)

#---------------------------
# log the counts

# remove the numerator from the data set
uganda_vl[ , suppressed:=NULL]
uganda_vl[ , max_ratio:=NULL]
uganda_vl[ , min_ratio:=NULL]
uganda_vl[ , district_ratio:=NULL]

#---------------
# mark the 0s
uganda_vl[patients_received==0, patients_received0:=T] # this will only be true when one sex is present and not the other
uganda_vl[samples_received==0, samples_received0:=T] # this will only be true when one sex is present and not the other
uganda_vl[rejected_samples==0, rejected_samples0:=T]
uganda_vl[dbs_samples==0, dbs_samples0:=T]
uganda_vl[samples_tested==0, samples_tested0:=T]
uganda_vl[valid_results==0, valid_results0:=T]

uganda_vl[patients_received!=0, patients_received0:=F] 
uganda_vl[samples_received!=0, samples_received0:=F] 
uganda_vl[rejected_samples!=0, rejected_samples0:=F]
uganda_vl[dbs_samples!=0, dbs_samples0:=F]
uganda_vl[samples_tested!=0, samples_tested0:=F]
uganda_vl[valid_results!=0, valid_results0:=F]

#---------------
# reset the 0s in the counts to reflect the minimum value, 1
uganda_vl[patients_received>0, min(patients_received)]

uganda_vl[patients_received==0, patients_received:=1] # this is only true if data for the other sex are missing
uganda_vl[samples_received==0, samples_received:=1] # this is only true if data for the other sex are missing
uganda_vl[rejected_samples==0, rejected_samples:=1]
uganda_vl[dbs_samples==0, dbs_samples:=1] 
uganda_vl[samples_tested==0, samples_tested:=1]
uganda_vl[valid_results==0, valid_results:=1]


#------------------------------------
# log all the variables to impute

uganda_vl[ , patients_received:=log(patients_received)]
uganda_vl[ , samples_received:=log(samples_received)]
uganda_vl[ , rejected_samples:=log(rejected_samples)]
uganda_vl[ , dbs_samples:=log(dbs_samples)]
uganda_vl[ , samples_tested:=log(samples_tested)]
uganda_vl[ , valid_results:=log(valid_results)] 

# remove plasma samples to calculate later on
uganda_vl[ , plasma_samples:=NULL]

#------------------------------------

# ---------------
# drop out demarcation variables (use a merge after imputation to identify 0s and 1s)

uvl <- uganda_vl[ , .(facility_id, facility_name, dhis2name, district_id, dist_name, sex, date,
                      patients_received, samples_received, rejected_samples, dbs_samples, samples_tested, valid_results, 
                      ratio=sup_ratio, level, 
                      imputed, missing_males, missing_females, ratio0, ratio1,
                      patients_received0, samples_received0, rejected_samples0, dbs_samples0, samples_tested0, valid_results0)]


# data prep for imputation complete!!! :)
#-----------------------------------


#------------------------------------
# run imputation using amelia

#---------------
# make cs variable
uvl[, cs_variable:=paste0(facility_id, sex)]

# set idvars
idVars <- c("facility_id", "sex", "facility_name", "dhis2name", "dist_name", "imputed", 
            'missing_males', 'missing_females', 'ratio0', 'ratio1',
            'patients_received0', 'samples_received0', 'rejected_samples0', 'dbs_samples0', 'samples_tested0', 'valid_results0')

# list the variables to lag
lagVars <- c( "patients_received", "samples_received", "rejected_samples", "dbs_samples", "samples_tested", "valid_results", "ratio")

# original  imputation
# imputed_data <- amelia(uvl, m=50, cs='cs_variable', ts='date', lags=lagVars, idvars=idVars, noms='district_id', ords='level')


#------------------
# polytime versions 
imputed_time1 <- amelia(uvl, m=2, cs='cs_variable', ts='date', lags=lagVars, idvars=idVars, noms='district_id', ords='level', polytimne==1)
imputed_time2 <- amelia(uvl, m=2, cs='cs_variable', ts='date', lags=lagVars, idvars=idVars, noms='district_id', ords='level', polytime==2)
imputed_time3 <- amelia(uvl, m=2, cs='cs_variable', ts='date', lags=lagVars, idvars=idVars, noms='district_id', ords='level', polytime==3)


#------------------ 
# bind the imputations together to create a single data set called imputed_time#

# time 1 - linear time effects
for( i in 1:50 ) {
  imputed_time1$imputations[[i]]$imputation_number <- i
  if (i==1)  amelia_time1 <- data.table(imputed_time1$imputations[[i]])
  if (i>1) amelia_time1 <- rbind(amelia_time1, imputed_time1$imputations[[i]])
}

# save the imputed data as an RDS
saveRDS(amelia_time1, file= paste0(dir, "/imputed_time1.rds"))  


#------------
# time 2 - squared time effects

for( i in 1:50 ) {
  imputed_time2$imputations[[i]]$imputation_number <- i
  if (i==1)  amelia_time2 <- data.table(imputed_time2$imputations[[i]])
  if (i>1) amelia_time2 <- rbind(amelia_time2, imputed_time2$imputations[[i]])
}

# save the imputed data as an RDS
saveRDS(amelia_time1, file= paste0(dir, "/imputed_time2.rds"))  

#------------
# time 3 - cubed time effects

for( i in 1:50 ) {
  imputed_time3$imputations[[i]]$imputation_number <- i
  if (i==1)  amelia_time3 <- data.table(imputed_time3$imputations[[i]])
  if (i>1) amelia_time3 <- rbind(amelia_time3, imputed_time3$imputations[[i]])
}

# save the imputed data as an RDS
saveRDS(amelia_time3, file= paste0(dir, "/imputed_time3.rds"))  



#---------------------------------




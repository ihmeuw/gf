# ----------------------------------------------
# Caitlin O'Brien-Carelli

# 5/15/2018
# Multiple imputation for the Uganda Viral Load Dashboard
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
dir <- 'J:/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard'

# upload the data with month, year, sex
uvl1 <- readRDS(paste0(dir, "/sex_data.rds"))
uganda_vl <- uvl1

# start from scale up and continue through april 2018
uganda_vl <- uganda_vl[!(month==3 & year==2018)] # fix me
uganda_vl <- uganda_vl[year==2016 | year==2017 | year==2018]
uganda_vl <- uganda_vl[sex!='Unknown'] # there should not be unknowns in the data set; confirm

# check for duplicate entries 
uganda_vl[sex=='Female', sex1:=1]
uganda_vl[sex=='Male', sex1:=2]
uganda_vl[ ,combine:= paste0(facility_id, '_', date, '_', sex1)]
uganda_vl <- uganda_vl[order(combine)]
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
uganda_vl[ ,combine:= paste0(facility_id, '_', date, '_', sex1)]
uganda_vl[duplicated(combine)]

#-------------------
# collapse the duplicate entries to only single values
# this only matters if there are duplicate entries - should be none (perform as a check)
sumVars <- c("patients_received", "samples_received", "rejected_samples", "plasma_samples",
             "dbs_samples", "samples_tested", "valid_results", "suppressed")

uganda_vl <- uganda_vl[, lapply(.SD, sum, na.rm=FALSE), by=c('facility_id', 'facility_name', 'dhis2name', 
                                                    'sex', 'date', 'district_id', 'dist_name', 'level'),
                                                    .SDcols=sumVars]
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
# add zeroes to the data where one sex is present but the other is missing in a specific month
# for example, in january 2016 in facility 2 if males are present but females are NA

#-------------------
# male data but missing females
uganda_vl[ ,combine:= paste0(facility_id, '_', date)]

# male data but missing females
missing_females <- uganda_vl[sex=='Female' & is.na(patients_received), .(combine=combine)]

males <- uganda_vl[sex=='Male']
males <- males[!is.na(patients_received), .(combine)]
males <- merge(males, missing_females, by='combine')

males[ , missing_females:=1]
uganda_vl <- merge(uganda_vl, males, by='combine', all.x=TRUE)
uganda_vl[is.na(missing_females), missing_females:=0]

#add zeroes for all female rows that in dates and facilities that have male data but no female data is reported
uganda_vl[missing_females==1 & sex=='Female', patients_received:=0]
uganda_vl[missing_females==1 & sex=='Female', samples_received:=0]
uganda_vl[missing_females==1 & sex=='Female', rejected_samples:=0]
uganda_vl[missing_females==1 & sex=='Female', plasma_samples:=0]
uganda_vl[missing_females==1 & sex=='Female', dbs_samples:=0]
uganda_vl[missing_females==1 & sex=='Female', samples_tested:=0]
uganda_vl[missing_females==1 & sex=='Female', suppressed:=0]
uganda_vl[missing_females==1 & sex=='Female', valid_results:=0]

#-------------------
# female data but missing males
missing_males <- uganda_vl[sex=='Male' & is.na(patients_received), .(combine=combine)]

females <- uganda_vl[sex=='Female']
females <- females[!is.na(patients_received), .(combine)]
females <- merge(females, missing_males, by='combine')

females[ , missing_males:=1]
uganda_vl <- merge(uganda_vl, females, by='combine', all.x=TRUE)
uganda_vl[is.na(missing_males), missing_males:=0]

# add zeroes for all the missing female entries where these is male data
uganda_vl[missing_males==1 & sex=='Male', patients_received:=0] 
uganda_vl[missing_males==1 & sex=='Male', samples_received:=0] 
uganda_vl[missing_males==1 & sex=='Male', rejected_samples:=0] 
uganda_vl[missing_males==1 & sex=='Male', plasma_samples:=0] 
uganda_vl[missing_males==1 & sex=='Male', dbs_samples:=0] 
uganda_vl[missing_males==1 & sex=='Male', samples_tested:=0] 
uganda_vl[missing_males==1 & sex=='Male', suppressed:=0] 
uganda_vl[missing_males==1 & sex=='Male', valid_results:=0] 

#-----------------------

#---------------------
# check for duplicates
# create a unique identifier (char) of facilityid_date_sex
uganda_vl[sex=="Female", sex1:=1 ]
uganda_vl[sex=="Male", sex1:=2]

uganda_vl[ ,combine1:= paste0(facility_id, '_', date, '_', sex1)]
uganda_vl[,length(unique(combine1))] 

uganda_vl[duplicated(combine1)] # no duplicates
uganda_vl[,combine1:=NULL] # then delete the identifier
uganda_vl[,combine:=NULL]
uganda_vl[,sex1:=NULL]

#---------------------
# basic descriptives for missing males and females

# the number of males in facilities and months where there is no female data
uganda_vl[missing_females==1 & sex=='Male', .(patients_received, sum(patients_received)), by=.(sex, facility_id, date)]


# ----------------------------------------------
# prep the data for the imputation

#------------------------
# change integers to numerics to accomodate decimal values
# drop out month, year and leave date 

Vars <- c("patients_received", "samples_received", "rejected_samples","dbs_samples", "plasma_samples",
          "samples_tested", "suppressed", "valid_results")

uganda_vl <- uganda_vl[,lapply(.SD, as.numeric), by=c('facility_id', 'facility_name', 'dhis2name', 
                                               'district_id', 'dist_name', 'sex', 'date', 'missing_males', 'missing_females', 'level'), 
                                              .SDcols=Vars]

#-------------------------------------------------------------------------



#---------------------------
# identify the original values for imputation 

# create a variable marking which rows to impute
uganda_vl[is.na(patients_received), imputed:=1]
uganda_vl[!is.na(patients_received), imputed:=0]

#---------------------------

#---------------
# mark the 0s
uganda_vl[patients_received==0 & imputed==0, patients_received0:=T] # this will only be true when one sex is present and not the other
uganda_vl[samples_received==0 & imputed==0, samples_received0:=T] # this will only be true when one sex is present and not the other
uganda_vl[rejected_samples==0 & imputed==0, rejected_samples0:=T]
uganda_vl[dbs_samples==0 & imputed==0, dbs_samples0:=T]
uganda_vl[plasma_samples==0 & imputed==0, plasma_samples0:=T]
uganda_vl[samples_tested==0 & imputed==0, samples_tested0:=T]
uganda_vl[valid_results==0 & imputed==0, valid_results0:=T]
uganda_vl[suppressed==0 & imputed==0, suppressed0:=T]

uganda_vl[patients_received!=0 & imputed==0, patients_received0:=F] 
uganda_vl[samples_received!=0 & imputed==0, samples_received0:=F] 
uganda_vl[rejected_samples!=0 & imputed==0, rejected_samples0:=F]
uganda_vl[dbs_samples!=0 & imputed==0, dbs_samples0:=F]
uganda_vl[plasma_samples!=0 & imputed==0, plasma_samples0:=F]
uganda_vl[samples_tested!=0 & imputed==0, samples_tested0:=F]
uganda_vl[valid_results!=0 & imputed==0, valid_results0:=F]
uganda_vl[suppressed!=0 & imputed==0, suppressed0:=F]

#---------------------------------
# export the data set to use in distinct methods of transformation for imputation
dir <- 'J:/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/imputed'
saveRDS(uganda_vl, file= paste0(dir, "/impute_ready.rds"))

#--------------------------
# OFFSET TRANSFORM

#---------------
# add 1 to every value in the data set that is not being imputed
# drop out plasma samples to avoid violating equality constraints

uganda_vl[!is.na(patients_received), patients_received:=(patients_received+1)] 
uganda_vl[!is.na(samples_received), samples_received:=(samples_received+1)] 
uganda_vl[!is.na(rejected_samples), rejected_samples:=(rejected_samples+1)]
uganda_vl[!is.na(dbs_samples), dbs_samples:=(dbs_samples+1)] 
uganda_vl[!is.na(samples_tested), samples_tested:=(samples_tested+1)]
uganda_vl[!is.na(valid_results), valid_results:=(valid_results+1)]
uganda_vl[!is.na(suppressed), suppressed:=(suppressed+1)]

#---------------------------
# check equality constraints
uganda_vl[samples_received < patients_received]
uganda_vl[samples_received < dbs_samples]
uganda_vl[samples_received < rejected_samples]
uganda_vl[samples_received < samples_tested]
uganda_vl[samples_received < valid_results]
uganda_vl[samples_tested < valid_results]
uganda_vl[valid_results < suppressed]

#------------------------
# calculate the suppression ratio
uganda_vl[, ratio:=(suppressed/valid_results)]

#------------------------

# if valid results is 0, alter the ratio to be the mean district/level suppression ratio
# compute district average ratio among nonzeroes
uganda_vl[valid_results0==FALSE, district_ratio:=mean(ratio, na.rm=TRUE), by=.(district_id, level)]
uganda_vl[, district_ratio:=mean(district_ratio, na.rm=TRUE), by=.(district_id, level)]

# replace ratio to district average when the number of samples was zero
uganda_vl[valid_results0==TRUE, ratio:=district_ratio]
uganda_vl[ , district_ratio:=NULL]
 
#------------------------
# counts are transformed; transform the suppression ratio
hist(uganda_vl$ratio)

# keep track of 1s - the suppression ratio can never be 0 bc the numerator is never 0
uganda_vl[ratio==1, ratio1:=T] 
uganda_vl[ratio<1 & !is.na(ratio), ratio1:=F] 

#---------------
# transform the ratio using a lemon squeeze
n <- uganda_vl[ !is.na(patients_received),.N]
uganda_vl[ , ratio:=logit(((ratio*(n - 1))+0.5)/n)]

# code for using min./max. values instead of the lemon squeeze formula
# # replace suppression ratio = 1 with the maximum possible value < 1
# uganda_vl[, max_ratio:= uganda_vl[ratio<1, max(ratio, na.rm=T)]] #max. ratio 0s 0.9991682
# uganda_vl[ratio1==T, ratio:=max_ratio]
# check all 0s and 1s have been removed
# uganda_vl[ ,.(max(ratio, na.rm=T))]
# uganda_vl[ ,.(min(ratio, na.rm=T))]
# 
# #---------------
# # logit transform the ratio
# uganda_vl[ , ratio:=logit(ratio)]
# hist(uganda_vl$ratio)

#---------------------------
# log the counts

# remove the numerator from the data set
uganda_vl[ , suppressed:=NULL]


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
# clean up the data set and place columns in an intuitive order

uvl <- uganda_vl[ , .(facility_id, facility_name, dhis2name, level, district_id, dist_name, sex, date,
                             patients_received, samples_received, rejected_samples, dbs_samples, samples_tested, valid_results, ratio, 
                              imputed, missing_males, missing_females, ratio1,
                               patients_received0, samples_received0, rejected_samples0, 
                              dbs_samples0, samples_tested0, valid_results0, suppressed0)]


saveRDS(uvl, file= paste0(dir, "/impute_ready_offset.rds"))  


# data prep for imputation complete!!! :)
#-----------------------------------


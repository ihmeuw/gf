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
library(dplyr)
library(RColorBrewer)
library(Amelia)
library(MASS)
library(gtools)

# set input/output directory
# ----------------------------------------------

dir <- '/home/j/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard'

# upload the data with month, year, sex
uvl1 <- readRDS(uganda_vl, file= paste0(dir, "/impute_ready.rds"))
uganda_vl <- uvl1



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
# transform the 1s

# replace suppression ratio = 1 with the maximum possible value < 1
uganda_vl[, max_ratio:= uganda_vl[ratio<1, max(ratio, na.rm=T)]] #max. ratio 0s 0.9991682
uganda_vl[ratio1==T, ratio:=max_ratio]

# check all 0s and 1s have been removed
uganda_vl[ ,.(max(ratio, na.rm=T))]
uganda_vl[ ,.(min(ratio, na.rm=T))]

#---------------
# logit transform the ratio
uganda_vl[ , ratio:=logit(ratio)]
hist(uganda_vl$ratio)

#---------------------------
# log the counts

# remove the numerator from the data set
uganda_vl[ , suppressed:=NULL]
uganda_vl[ , max_ratio:=NULL]


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


# data prep for imputation complete!!! :)
#-----------------------------------


#------------------------------------
# run imputation using amelia
# offset model

#---------------
# make cs variable
uvl[, cs_variable:=paste0(facility_id, sex)]

# set idvars
idVars <- c("facility_id", "sex", "facility_name", "dhis2name", "dist_name", "imputed", 
            'missing_males', 'missing_females', 'ratio1',
            'patients_received0', 'samples_received0', 'rejected_samples0', 'dbs_samples0', 'samples_tested0', 'valid_results0', 'suppressed0')

# list the variables to lag
lagVars <- c( "patients_received", "samples_received", "rejected_samples", "dbs_samples", "samples_tested", "valid_results", "ratio")

# run imputation
imputed_data <- amelia(uvl, m=50, cs='cs_variable', ts='date', lags=lagVars, idvars=idVars, noms='district_id', ords='level')

#---------------------------
# graph one test case to see how it looks
cstmp = sample(unique(uvl$cs_variable),1)
merged = merge(uvl, imputed_data[[1]]$imp1, by=c('cs_variable','date'), all=T)
ggplot(merged[cs_variable==cstmp], aes(y=ratio.y, x=date)) +
  geom_point(color='red') +
  geom_point(aes(y=ratio.x), color='black') +
  labs(title=paste('Facility-sex:', cstmp))

#--------------------------------------------------------------
# bind the imputations together to create a single data set called amelia_data

for( i in 1:50 ) {
  imputed_data$imputations[[i]]$imputation_number <- i
  if (i==1)  amelia_data <- data.table(imputed_data$imputations[[i]])
  if (i>1) amelia_data <- rbind(amelia_data, imputed_data$imputations[[i]])
}

# save the imputed data as an RDS
saveRDS(amelia_data, file= paste0(dir, "/imputed_offset.rds"))  
#-------------------------------

#------------------
# polytime versions 
imputed_time2 <- amelia(uvl, m=50, cs='cs_variable', ts='date', lags=lagVars, idvars=idVars, noms='district_id', ords='level', polytime==2, parallel='snow')
saveRDS(imputed_time2, file= paste0(dir, "/imputed_offset_time2.rds"))  

imputed_time3 <- amelia(uvl, m=50, cs='cs_variable', ts='date', lags=lagVars, idvars=idVars, noms='district_id', ords='level', polytime==3, parallel='snow')
saveRDS(imputed_time2, file= paste0(dir, "/imputed_offset_time2.rds"))  

# ------------------


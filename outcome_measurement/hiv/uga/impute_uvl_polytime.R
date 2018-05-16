# ----------------------------------------------
# Caitlin O'Brien-Carelli

# 5/16/2018
# Multiple imputation for the Uganda Viral Load Dashboard
# Run MI (linear and polytime models) on the cluster
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
dir <- '/home/j/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/imputed'

# upload the data with month, year, sex
uvl <- readRDS(paste0(dir, "/impute_ready_offset.rds"))

#------------------------------------

#------------------------------------
# run imputation using amelia
# offset model - the counts are offset by +1 and the ratios are lemon squeeze

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
imputed_data <- amelia(uvl, m=50, cs='cs_variable', ts='date', lags=lagVars, leads=lagVars, idvars=idVars, noms='district_id', ords='level')

# #---------------------------
# # graph one test case to see how it looks
# cstmp = sample(unique(uvl$cs_variable),1)
# merged = merge(uvl, imputed_data[[1]]$imp1, by=c('cs_variable','date'), all=T)
# ggplot(merged[cs_variable==cstmp], aes(y=ratio.y, x=date)) +
#   geom_point(color='red') +
#   geom_point(aes(y=ratio.x), color='black') +
#   labs(title=paste('Facility-sex:', cstmp))

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


#--------------------------------------------------------------
# Models with polytime
# run imputation using amelia

#---------------
# make cs variable
uvl[, cs_variable:=paste0(facility_id, sex)]

# set idvars
idVars <- c("facility_id", "sex", "facility_name", "dhis2name", "dist_name", "imputed", 
            'missing_males', 'missing_females', 'ratio1',
            'patients_received0', 'samples_received0', 'rejected_samples0', 'dbs_samples0', 'samples_tested0', 'valid_results0', 'suppressed0')

# list the variables to lag
lagVars <- c( "patients_received", "samples_received", "rejected_samples", "dbs_samples", "samples_tested", "valid_results", "ratio")

#------------------
# run the imputations with polytime 
imputed_time2 <- amelia(uvl, m=2, cs='cs_variable', ts='date', lags=lagVars, leads=lagVars, idvars=idVars, noms='district_id', ords='level', polytime=2, parallel='snow')

imputed_time3 <- amelia(uvl, m=2, cs='cs_variable', ts='date', lags=lagVars, leads=lagVars, idvars=idVars, noms='district_id', ords='level', polytime=3, parallel='snow')

# ------------------

#--------------------------------------------------------------
# bind the imputations together to create a single data set called amelia_data_time2
# squared time

for( i in 1:50 ) {
  imputed_time2$imputations[[i]]$imputation_number <- i
  if (i==1)  amelia_time2 <- data.table(imputed_time2$imputations[[i]])
  if (i>1) amelia_time2 <- rbind(amelia_time2, imputed_time2$imputations[[i]])
}

# save the imputed data as an RDS
saveRDS(amelia_time2, file= paste0(dir, "/imputed_offset_time2.rds"))  
#--------------------------------------------------------------
# bind the imputations together to create a single data set called amelia_data_time2
# cubed time

for( i in 1:50 ) {
  imputed_time3$imputations[[i]]$imputation_number <- i
  if (i==1)  amelia_time3 <- data.table(imputed_time3$imputations[[i]])
  if (i>1) amelia_time3 <- rbind(amelia_time3, imputed_time3$imputations[[i]])
}

# save the imputed data as an RDS
saveRDS(amelia_time3, file= paste0(dir, "/imputed_offset_time3.rds"))  

#--------------------------------------------------------------



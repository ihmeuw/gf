# ----------------------------------------------
# Caitlin O'Brien-Carelli

# 5/15/2018
# Lemon squeeze
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
library(parallel)

# set input/output directory
# ----------------------------------------------
#dir <- 'J:/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/imputed'

j <- ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
dir <- paste0(j, '/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/imputed')

# upload the data with month, year, sex
uvl1 <- readRDS(paste0(dir, "/impute_ready.rds"))
uganda_vl <- uvl1

#-----------------------------------------------

# create a data set that includes only the relevant values
uvl <- uganda_vl[ , .(facility_id, facility_name, dhis2name, level, district_id, dist_name, sex, date,
                      samples_received, dbs_samples, samples_tested, valid_results, suppressed,
                      imputed, missing_males, missing_females,
                      samples_received0, dbs_samples0, samples_tested0, valid_results0, suppressed0)]


# transform the values into ratios
uvl[ ,dbs:=(dbs_samples/samples_received)]
uvl[ ,tested:=(samples_tested/samples_received)]
uvl[ ,valid:=(valid_results/samples_tested)]
uvl[ ,sup:=(suppressed/valid_results)]

# when the ratio is 0/0, use the mean district-platform ratio for that variable
uvl[samples_received0==FALSE, district_dbs:=mean(dbs, na.rm=TRUE), by=.(district_id, level)]
uvl[samples_received0==FALSE, district_tested:=mean(tested, na.rm=TRUE), by=.(district_id, level)]
uvl[samples_tested0==FALSE, district_valid:=mean(valid, na.rm=TRUE), by=.(district_id, level)]
uvl[valid_results0==FALSE, district_sup:=mean(sup, na.rm=TRUE), by=.(district_id, level)]

uvl[, district_dbs:=mean(district_dbs, na.rm=TRUE), by=.(district_id, level)]
uvl[, district_tested:=mean(district_tested, na.rm=TRUE), by=.(district_id, level)]
uvl[, district_valid:=mean(district_valid, na.rm=TRUE), by=.(district_id, level)]
uvl[, district_sup:=mean(district_sup, na.rm=TRUE), by=.(district_id, level)]

uvl[samples_received0==T, dbs:=district_dbs]
uvl[samples_received0==T, tested:=district_tested]
uvl[samples_tested0==T, valid:=district_valid]
uvl[valid_results0==T, sup:=district_sup]

#-----------------------------------------------------
# offset samples received and log it

uvl[ ,samples_received:=(samples_received+1)]
uvl[ ,samples_received:=(log(samples_received))]

#----------------------------
# keep only the relevant variables for imputation

uvl <- uvl[ , .(samples_received, dbs, tested, valid, sup),
            by=.(facility_id, facility_name, dhis2name, level, district_id, dist_name, sex, date,
                 imputed, missing_males, missing_females,
                 samples_received0, dbs_samples0, samples_tested0, valid_results0, suppressed0)]

#----------------------------
# lemon squeeze the ratios
# transform the ratio using a lemon squeeze
n <- uvl[ !is.na(samples_received),.N]
uvl[ , dbs:=logit(((dbs*(n - 1))+0.5)/n)]
uvl[ , tested:=logit(((tested*(n - 1))+0.5)/n)]
uvl[ , valid:=logit(((valid*(n - 1))+0.5)/n)]
uvl[ , sup:=logit(((sup*(n - 1))+0.5)/n)]

#------------------------

# data is now prepped for Amelia MI

#----------------

#---------------
# make cs variable
uvl[, cs_variable:=paste0(facility_id, sex)]

# set idvars
idVars <- c("facility_id", "sex", "facility_name", "dhis2name", "dist_name", "imputed", 
            'missing_males', 'missing_females', 
             'samples_received0', 'dbs_samples0', 'samples_tested0', 'valid_results0', 'suppressed0')

# list the variables to lag
lagVars <- c("samples_received", "dbs", "tested", "valid", "sup")

# set up for parallel
ncores = as.numeric(detectCores())

# run the imputation - no polytime
imputed_data <- amelia(uvl, m=50, cs='cs_variable', ts='date', lags=lagVars, leads=lagVars, idvars=idVars, noms='district_id', ords='level', parallel='multicore', ncpus=ncores)


#--------------------------------------------------------------
# bind the imputations together to create a single data set called amelia_data

for( i in 1:50 ) {
  imputed_data$imputations[[i]]$imputation_number <- i
  if (i==1)  amelia_data <- data.table(imputed_data$imputations[[i]])
  if (i>1) amelia_data <- rbind(amelia_data, imputed_data$imputations[[i]])
}

# save the imputed data as an RDS
saveRDS(amelia_data, file= paste0(dir, "/imputed_ls.rds"))  
#-------------------------------


#POLYTIME
#--------------------------------------------------------------
# Models with polytime
# run imputation using amelia

#------------------
# run the imputations with polytime - time squared
imputed_time2 <- amelia(uvl, m=50, cs='cs_variable', ts='date', lags=lagVars, leads=lagVars, idvars=idVars, noms='district_id',
                        ords='level', polytime=2, parallel='multicore', ncpus=ncores)

#--------------------------------------------------------------
# bind the imputations together to create a single data set called amelia_data_time2
# squared time

for( i in 1:50 ) {
  imputed_time2$imputations[[i]]$imputation_number <- i
  if (i==1)  amelia_time2 <- data.table(imputed_time2$imputations[[i]])
  if (i>1) amelia_time2 <- rbind(amelia_time2, imputed_time2$imputations[[i]])
}

# save the imputed data as an RDS
saveRDS(amelia_time2, file= paste0(dir, "/imputed_ls_time2.rds"))  
#--------------------------------------------------------------
# bind the imputations together to create a single data set called amelia_data_time2
# cubed time

imputed_time3 <- amelia(uvl, m=50, cs='cs_variable', ts='date', lags=lagVars, leads=lagVars, idvars=idVars, noms='district_id', ords='level', 
                        polytime=3, parallel='multicore', ncpus=ncores)


for( i in 1:50 ) {
  imputed_time3$imputations[[i]]$imputation_number <- i
  if (i==1)  amelia_time3 <- data.table(imputed_time3$imputations[[i]])
  if (i>1) amelia_time3 <- rbind(amelia_time3, imputed_time3$imputations[[i]])
}

# save the imputed data as an RDS
saveRDS(amelia_time3, file= paste0(dir, "/imputed_ls_time3.rds"))  

#--------------------------------------------------------------










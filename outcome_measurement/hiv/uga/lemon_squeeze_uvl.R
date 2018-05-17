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

# set input/output directory
# ----------------------------------------------
dir <- 'J:/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/imputed'

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
uganda_vl[ dbs:=(dbs_samples/samples_received)]
uganda_vl[ tested:=(samples_tested/samples_received)]
uganda_vl[ valid:=(valid_results/samples_tested)]
uganda_vl[ sup:=(suppressed/valid_results)]

# when the ratio is 0/0, use the mean district-platform ratio for that variable
uganda_vl[samples_received0==FALSE, district_dbs:=mean(dbs, na.rm=TRUE), by=.(district_id, level)]
uganda_vl[samples_received0==FALSE, district_tested:=mean(tested, na.rm=TRUE), by=.(district_id, level)]
uganda_vl[samples_tested0==FALSE, district_valid:=mean(valid, na.rm=TRUE), by=.(district_id, level)]
uganda_vl[valid_results0==FALSE, district_sup:=mean(sup, na.rm=TRUE), by=.(district_id, level)]

#uganda_vl[, district_ratio:=mean(district_ratio, na.rm=TRUE), by=.(district_id, level)]

uganda_vl[, district_dbs:=mean(district_dbs, na.rm=TRUE), by=.(district_id, level)]
uganda_vl[, district_tested:=mean(district_tested, na.rm=TRUE), by=.(district_id, level)]
uganda_vl[, district_valid:=mean(district_valid, na.rm=TRUE), by=.(district_id, level)]
uganda_vl[, district_sup:=mean(district_sup, na.rm=TRUE), by=.(district_id, level)]


#-----------------------------------------------------
# offset log samples received

uganda_vl[ ,samples_received:=(samples_received+1)]
uganda_vl[ ,samples_received:=(log(samples_received))]

#----------------------------
# lemon squeeze the ratios


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


#---------------







# ----------------------------------------------
# Audrey Batzel
#
# 5/16/19 
# final prep for MI (used to be in prep_for_MI.R)
setwd('C:/local/gf/')
# ----------------------------------------------

# --------------------
# Set up R / install packages
rm(list=ls())
library(data.table)
library(reshape2)
library(stringr)
library(RColorBrewer)
library(ggplot2)
library(lubridate)
library(readxl)
library(stats)
library(rlang)
library(zoo)
library(tidyr)
library(dplyr)
library(parallel)
# --------------------

# ----------------------------------------------
# Overview - Files and Directories
# ---------------------------------------------- 
root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# data directory
dir_prepped = paste0(root, "/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLP/archive/")
dir = paste0(root, "/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLP/")

# input files
dups = "pnlp_matrix_of_duplicate_rows.rds"
dt_outliers_labelled = "pnlp_outliers_labeled.rds"

# output files
output_dt = "PNLP_dt_forMI_updated.rds"
# ----------------------------------------------   

# ----------------------------------------------
# Read in the data
# ---------------------------------------------- 
dt = readRDS(paste0(dir, dt_outliers_labelled))
dup_matrix = readRDS(paste0(dir, dups))
# ----------------------------------------------   

# ----------------------------------------------
# Remove duplicates using the dup_matrix generated from screen_duplciates.R
# ---------------------------------------------- 
dt2 = dup_matrix[num_identical >= 20]
remove_ids = unique(c(dt2$i, dt2$j))
# ---------------------------------------------- 

# ---------------------------------------------- 
# Deterministically impute total health facilities and convert the number reporting to a proportion over the total, then drop the original variable for number reporting and impute
# the proportion so that we can back calculate the number reporting and it will always be less than the total number of facilities.
# ----------------------------------------------   
ameliaDT[, year:= year(date)]

ameliaDT$healthFacilities_total <- as.numeric(ameliaDT$healthFacilities_total)
ameliaDT$healthFacilities_numReported <- as.numeric(ameliaDT$healthFacilities_numReported)
# keep track of what the original values are before we change them:
ameliaDT[, healthFacilities_total_orig := healthFacilities_total]
ameliaDT[, healthFacilities_numReported_orig := healthFacilities_numReported]

# Number of health facilities reporting should not be greater than the total number of health facilities but there are 165 instances of this.
# ---------------- 
# NOTE: commented out 01/19 because I think we would want to handle this problem AFTER taking max of total fac and set the num reported rather than the total
# # when health facilities reporting is greater than total health facilities, change health facilities total to = health facilities reporting
# ameliaDT[healthFacilities_numReported > healthFacilities_total, healthFacilities_total:=healthFacilities_numReported]
# ----------------
# NOTE: commented out 3/11 because I don't think this is the right approach either.. might be best to set both to NA and then just deterministically impute health facilities
# total the same way
# # when the number of health facilities reporting is greater than total health facilities:
#   # if the number of health facilities reporting is equal to the max of health facilities total by health zone/year, then set
#     # health facilities total = health facilities max
#   ameliaDT <- ameliaDT[, healthFacilities_max := max(healthFacilities_total, na.rm=TRUE), by=c("dps", "health_zone", "year")]
#   ameliaDT <- ameliaDT[healthFacilities_max == "-Inf", healthFacilities_max:=NA]  # result was -Inf where all values were missing by unique group
#   ameliaDT[healthFacilities_numReported == healthFacilities_max, healthFacilities_total:= healthFacilities_max]
#   # if health facilities total 
# ----------------  
# set both health facilities reporting and health facilities total to missing when reporting > total
ameliaDT[healthFacilities_numReported > healthFacilities_total, c('healthFacilities_total', 'healthFacilities_numReported') := NA]

# when healthFacilities_total variable is missing for a given year set it to be the same as the following year (since it is mostly earlier years missing)
test <- ameliaDT[, .(healthFacilities_max = max(healthFacilities_total, na.rm=TRUE)), by=c("dps", "health_zone", "year")]
test <- test[healthFacilities_max == "-Inf", healthFacilities_max:=NA]  # result was -Inf where all values were missing by group
# order by year descending for use of na.locf later 
test <- test[order(dps, health_zone, -year),]

# warning - na.locf won't work if all health facilities total data is missing in most recent year
# manually set mabalako 2017 so that it works
test[health_zone=="mabalako" & year==2017, healthFacilities_max:=28]
#na.locf replaces an NA with the most recent non NA prior to it
test[, healthFacilities_max:= na.locf(healthFacilities_max), by=health_zone]

# TO DO - go back and add this in?? Ask David (1/22/19)         
# # in some cases the mode facilities occurs 7 of 8 times and there's one random outlier value.  We want to change those
# test[ , healthFacilities_mode := getmode(healthFacilities_max), by= c("dps", "health_zone")]
# test2 <- test[healthFacilities_max == healthFacilities_mode, .(num_yrs_equal_to_mode = .N), by= c("dps", "health_zone")]
# test <- merge(test, test2, by=c("dps", "health_zone"), all=TRUE)

# merge test back to ameliaDT
ameliaDT <- merge(ameliaDT, test, all.x=TRUE, by=c("dps", "health_zone", "year"))

# TO DO - clarify this is what we wanted with David - also what would the direction of bias be here?    

# if health facilities total is missing, set it to be health facilities max (the max # of facilites in that year?)
ameliaDT[is.na(healthFacilities_total), healthFacilities_total := healthFacilities_max] 
# This created 66 new cases where num reporting > total facilities - in all of these cases, the original total facilities was NA
# set these to be NA too:
ameliaDT[ healthFacilities_numReported > healthFacilities_total, healthFacilities_numReported := NA]
# 1/22/19 changed this to divide reporting facilities by total facilities... make sure this is what we should do. 
ameliaDT[, healthFacilitiesProportion := ( healthFacilities_numReported / healthFacilities_total )]
if ( ameliaDT[healthFacilitiesProportion > 1, .N] > 0 ) stop("Proportion of health facilities reporting must be 1.0 or less")
# ----------------------------------------------   

# ---------------------------------------------- 
# Standardize Test Results
# combine age groups for tests to account for where these are separated out in different years of data- check with David, is this okay? best way to do this?
ameliaDT[, RDT_completed := ifelse( year <= 2014, RDT_completed, (RDT_completedUnder5 + RDT_completed5andOlder))]
ameliaDT[, RDT_positive := ifelse( year <= 2014, RDT_positive, (RDT_positiveUnder5 + RDT_positive5andOlder))]
ameliaDT[, smearTest_completed := ifelse( year <= 2014, smearTest_completed,(smearTest_completedUnder5 + smearTest_completed5andOlder))]
ameliaDT[, smearTest_positive := ifelse( year <= 2014, smearTest_positive, (smearTest_positiveUnder5 + smearTest_positive5andOlder))]

ameliaDT <- ameliaDT[, -c("year", "smearTest_completedUnder5", "smearTest_completed5andOlder", "smearTest_positiveUnder5", "smearTest_positive5andOlder",
                          "RDT_positive5andOlder", "RDT_positiveUnder5", "RDT_completedUnder5", "RDT_completed5andOlder")]
# ----------------------------------------------     

# ----------------------------------------------
# 3/11/19 Commenting this out because we don't want to back cast years where the health zone might not have existed. 
# I double checked this against the drc geographies map that I made last year, and there is the correct number of rows (49,320) for
# health zones 

# # rectangularize the data so there is an observation for every health zone and date
#   hzDPS <- unique(ameliaDT[, .(dps, health_zone)])
#   dates <- unique(ameliaDT$date)
#   
#   rect <- hzDPS[rep(1:nrow(hzDPS), length(unique(dates)))]
#   rect[, date:=rep(dates, each=nrow(hzDPS))]
#   
#   dt <- merge(ameliaDT, rect, by=c("date", "province11_name", "health_zone", "dps"), all=TRUE)

# add an id column
ameliaDT[, id:= .I]
# ----------------------------------------------

# ----------------------------------------------  
# remove vars not needed for imputation - we will impute the proportion of health facilities reporting, and then use that to back-calculate the number reporting
ameliaDT[, c("healthFacilities_total_orig", "healthFacilities_numReported_orig", "healthFacilities_numReported", "healthFacilities_max", "healthFacilities_numReportedWithinDeadline"):=NULL]
# ----------------------------------------------     

# ----------------------------------------------  
# export data  
saveRDS(ameliaDT, paste0(dir_prepped, output_dt))
# ---------------------------------------------- 

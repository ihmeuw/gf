# ----------------------------------------------
#  Audrey Batzel
# 
# 4/25/19 separated out from visualize_qr_outliers.R,
# so that this additional prep for SIGL is it's own file
# ----------------------------------------------

# --------------------
# Set up R
setwd('C:/local/gf')
rm(list=ls())
library(data.table)
library(quantreg)
library(ggplot2)
library(RColorBrewer)
library(stringr)
# --------------------

#------------------------------------
# set directories

# detect if operating on windows or on the cluster 
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set the directory for input and output
dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/')

# input files
inFile = paste0(dir, 'prepped/sigl/raw_sigl_quantreg_results.rds')

# output files
outData = 'prepped/sigl/prepped_sigl_quantreg_imputation_results.rds' 

# functions
source('./core/standardizeHZNames.R')
#------------------------------------

#------------------------------------
# read in the data
dt = readRDS(inFile)
#------------------------------------

#------------------------------------
# Additional SIGL prep:
#------------------------------------
# Note; commenting this out because when I reran QR on SIGL I did this in the prep code beforehand:
# # fix problem in sigl where when health zone is the org unit type, the health_zone variable was missing and 
# # got set incorrectly to bena-tshadi.
# dt[org_unit_type == "health_zone", health_zone := NA]
# dt[is.na(health_zone) & org_unit_type == "health_zone", health_zone1 := unlist(lapply(strsplit(org_unit, " "), "[", 2))]
# dt[is.na(health_zone) & org_unit_type == "health_zone", health_zone2 := unlist(lapply(strsplit(org_unit, " "), "[", 3))]
# dt[is.na(health_zone) & org_unit_type == "health_zone", health_zone3 := unlist(lapply(strsplit(org_unit, " "), "[", 4))]
# dt[ health_zone3 != 'Zone' & health_zone2 != 'Zone', health_zone := paste(health_zone1, health_zone2, health_zone3) ]
# dt[ health_zone3=='Zone', health_zone := paste(health_zone1, health_zone2)]
# dt[ health_zone2=='Zone', health_zone := health_zone1]
# dt[, c('health_zone1', 'health_zone2', 'health_zone3'):=NULL]
# dt$health_zone_std <- standardizeHZNames(dt$health_zone)

# fix where level is NA because we are going to do outlier detection by level
dt[is.na(level) & grepl(org_unit, pattern = "zone", ignore.case= TRUE), level := "health_zone"]
dt[is.na(level) & grepl(org_unit, pattern = "aire", ignore.case= TRUE), level := "health_area"]
dt[is.na(level) & grepl(org_unit, pattern = "polyc", ignore.case= TRUE), level := "polyclinic"]
dt[is.na(level) & grepl(org_unit, pattern = "hos.*cent|cent.*hos|CH", ignore.case= TRUE), level:="hospital_center"]
dt[is.na(level) & grepl(org_unit, pattern = "m*dical", ignore.case= TRUE), level:="medical_center"]
dt[is.na(level) & grepl(org_unit, pattern = "r*rence", ignore.case= TRUE), level:="reference_health_center"]
dt[is.na(level) & grepl(org_unit, pattern = "cent|CS", ignore.case= TRUE) & org_unit != "kn Centre Pediatrique de Matonge", level:="health_center"]
dt[is.na(level) & grepl(org_unit, pattern = "clin", ignore.case= TRUE), level:="clinic"]

# copy orig values before we do anything that changes or imputes values
dt[ , orig_value := value]

# MOVED TO EARLIER PREP STAGE:
# # remove rectangularization where a drug is completely missing in an org unit across all variables
# dt[, completely_missing:=all(is.na(value)), by=c('org_unit_id','drug')]
# dt = dt[completely_missing==FALSE]

# Change minimum number of values to imputed to 5: (will use simple median imputation to impute the ones that have less values)
# get the number of values that were present before QR imputation
check = dt[!is.na(value), .(number_of_values = .N), by = .(org_unit_id, drug, variable) ]
dt = merge(dt, check, all = TRUE, by = c('org_unit_id', 'drug', 'variable'))
dt[, variable_id := NULL]
dt[, drug_id := NULL]

# set fitted values and resid to NA where N < 5, but save orig fitted values as a new var
dt[, orig_fitted_value := fitted_value]
dt[ number_of_values < 5, fitted_value := NA]
dt[ number_of_values < 5, resid := NA]
dt[ number_of_values < 5, skipped_qr := "yes"]
#------------------------------------

#------------------------------------
# Save data
#------------------------------------
saveRDS(dt, paste0(dir, outData))
#------------------------------------



# ----------------------------------------------
# Audrey Batzel
# updated 2/21/20

# final prep/cleaning for DHIS2 sets
# ----------------------------------------------

# --------------------
# Set up R / install packages
# --------------------
library(data.table)
library(reshape2)
library(stringr)
library(ggplot2)
library(lubridate)
library(tidyr)
# --------------------

# ----------------------------------------------
# Overview - Files and Directories
# ----------------------------------------------
# data directory
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/')
out_dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/6_final_prepped/')

# input file
if(set == 'sigl') {
  inFile = paste0(dir, '3_prepped/', set, '/', set, '_prepped.rds')
} else {
  inFile = paste0(dir, '5_outliers_replaced/', set, '/', set, '_prepped_outliers_replaced.rds')
}

# output file
outFile = paste0(out_dir, set, '/', set, 'File.rds')

# functions
setwd('C:/local/gf/')
source('./core/standardizeHZNames.R')
source('./core/standardizeDPSNames.R')
# ----------------------------------------------

# ----------------------------------------------
# Read in data
# ----------------------------------------------
dt = readRDS(inFile)
# org_units = readRDS(paste0(dir, '0_meta_data/org_units.rds'))
# master_fac = readRDS(paste0(dir, '0_meta_data/master_facilities.rds'))
# ----------------------------------------------

# ----------------------------------------------
# Standardize health zone and dps names
# ----------------------------------------------
dt[, health_zone := standardizeHZNames(health_zone)]
dt[, dps := standardizeDPSNames(dps)]

# missing_org_units = dt[is.na(health_zone), unique(org_unit_id)]
# missing_org_units[missing_org_units %in% org_units$org_unit_ID]
# org_units[org_unit_ID %in% missing_org_units, org_unit_name]

#check unique ids:
if( nrow(unique(dt[, .(date, org_unit_id, element, category)])) != nrow(dt) ) stop("need to check unique id vars - they do not uniquely id rows")
# ----------------------------------------------

# ----------------------------------------------
# save prepped data
# ----------------------------------------------
saveRDS(dt, outFile)
# ----------------------------------------------



# ----------------------------------------------
# fix sigl health zones 
# health_zone is na where org_unit_type is health zone- we want these to be corrected so that every 
# row has a health_zone! (this isn't a problem in the other DHIS2 sets)
if(set == 'sigl') {
  sigl[is.na(health_zone) & org_unit_type == "health_zone", health_zone1 := unlist(lapply(strsplit(org_unit, " "), "[", 2))]
  sigl[is.na(health_zone) & org_unit_type == "health_zone", health_zone2 := unlist(lapply(strsplit(org_unit, " "), "[", 3))]
  sigl[is.na(health_zone) & org_unit_type == "health_zone", health_zone3 := unlist(lapply(strsplit(org_unit, " "), "[", 4))]
  sigl[ health_zone3 != 'Zone' & health_zone2 != 'Zone', health_zone := paste(health_zone1, health_zone2, health_zone3) ]
  sigl[ health_zone3=='Zone', health_zone := paste(health_zone1, health_zone2)]
  sigl[ health_zone2=='Zone', health_zone := health_zone1]
  sigl[, c('health_zone1', 'health_zone2', 'health_zone3'):=NULL]

meta_data = readRDS(paste0(dir, 'meta_data/data_elements.rds'))
dt = merge(sigl, meta_data, all.x = TRUE, by = "element_id")
dt[, element.x := as.character(element.x)]
dt[, element.y := as.character(element.y)]
dt[element.x!=element.y,] # check that they are the same
dt[, element.y := NULL]
setnames(dt, 'element.x', 'element')

# remove data that is incomplete due to reporting lag because this will affect QR
reporting = dt[, unique(org_unit_id), by = 'date']
reporting = reporting[, .N, by = 'date']
plot(reporting, N ~ date)
dt[ , year := year(date)]
dt = dt[ year >= 2017, ] # remove before 2017
dt = dt[ date != "2019-04-01", ] # remove last month of the download (april 2019)

saveRDS(dt, paste0(out_dir, sigl_out))
}

# ----------------------------------------------

# # ----------------------------------------------
# # PATI data sets:
# # separate new-TB cases and relapses by age group and sex variable from the rest of the data since this is the only one broken down by age and sex
# # ----------------------------------------------
# cases_by_age_sex <- dt1[category != "default", ]
# reg <- dt1[category == "default", ]
# 
# cases_by_age_sex$category <- as.character(cases_by_age_sex$category)
# cases_by_age_sex[, c("age", "sex") := tstrsplit(category, ",", fixed=TRUE)]
# cases_by_age_sex$sex <- trimws(cases_by_age_sex$sex)
# cases_by_age_sex$age <- trimws(cases_by_age_sex$age)
# 
# cases_by_age_sex[, category := NULL]
# # vars <- colnames(cases_by_age_sex)
# # vars <- vars[!vars %in% c("sex", "value")]
# # formula_for_cast <- as.formula(paste(paste(vars, collapse=" + "), "sex", sep= " ~ "))
# # cases_by_age_sex <- dcast.data.table(cases_by_age_sex, formula_for_cast, value.var = "value")
# 
# # drop unneeded vars
# cases_subset <- cases_by_age_sex[, .(date, quarter, dps, health_zone, health_area, org_unit, org_unit_id, org_unit_type, level, element, element_eng, age, sex, value)]
# # ----------------------------------------------
# # cast wide the results data
# # ----------------------------------------------
# vars <- colnames(dt2)
# vars <- vars[!vars %in% c("category", "value")]
# formula_for_cast <- as.formula(paste(paste(vars, collapse=" + "), "category", sep= " ~ "))
# dt2 <- dcast.data.table(dt2, formula_for_cast, value.var = "value")
# dt2 <- dt2[is.na(default), ]
# # ----------------------------------------------












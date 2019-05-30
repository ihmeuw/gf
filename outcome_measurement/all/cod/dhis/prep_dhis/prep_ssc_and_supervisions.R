setwd("C:/Users/elineb/Documents/gf/") 
# working directory should be the root of the repository

#-------------------------------------
# AUTHOR: Emily Linebarger / Audrey Batzel (modified 5/7/19 to combine, simplify, and fix duplicates)
# PURPOSE: Prep SSC and supervisions data from PNLS
# DATE: April 2019
rm(list=ls())
#-------------------------------------

#--------------------------------------------------
# Set up R, and read in data 
#--------------------------------------------------
library(data.table)

# directories:
j = ifelse(Sys.info()[1]=='Windows','J:','/home/j')
dir = paste0(j, "Project/Evaluation/GF/outcome_measurement/cod/dhis_data/pre_prep/")
prepped_dir = paste0(j, "Project/Evaluation/GF/outcome_measurement/cod/dhis_data/prepped/")
catalog = fread(paste0(j, "Project/Evaluation/GF/outcome_measurement/cod/dhis_data/catalogues/data_elements_cod.csv"))

# input files
sscFile = "ssc/SSC_data_2017_2018.rds"
supervisionsFile = "supervisions/Supervisions_2017_2018.rds"

# output files
prepped_ssc = 'ssc_prepped.rds'
prepped_supervisions = 'supervisions_prepped.rds'

# functions
source('./core/standardizeDPSNames.R')
source('./core/standardizeHZNames.R')

# load data
ssc = as.data.table(readRDS(paste0(dir, sscFile)))
sv = as.data.table(readRDS(paste0(dir, supervisionsFile)))
#--------------------------------------------------

#--------------------------------------------------
# Standardize variable names  
#--------------------------------------------------
names(ssc) = tolower(names(ssc))
names(sv) = tolower(names(sv))

# standardize health zone and dps names (simplified this)
ssc[, health_zone := standardizeHZNames(hz)]
ssc[, dps := standardizeDPSNames(dps)]

sv[, health_zone := standardizeHZNames(hz)]
sv[, dps := standardizeDPSNames(dps)]
#--------------------------------------------------

#--------------------------------------------------
# Merge with element catalog 
#--------------------------------------------------
ssc = merge(ssc, catalog[, .(element, element_eng, element_id)], by='element', all.x=TRUE) 
sv = merge(sv, catalog[, .(element, element_eng, element_id)], by='element', all.x=TRUE) 

#Check the merge
if (nrow(ssc[is.na(element_eng)]) != 0) stop("Check the merge; some values for element_eng are NA and shouldn't be") #Empty data table, so all matched.
if (nrow(sv[is.na(element_eng)]) != 0) stop("Check the merge; some values for element_eng are NA and shouldn't be") #Empty data table, so all matched.
#--------------------------------------------------

#--------------------------------------------------
# Create other variable names  
#--------------------------------------------------
ssc[, data_set := 'ssc'] 
sv[, data_set := 'supervisions']

ssc[, level:="health_zone"]
sv[, level:="health_zone"]

ssc[, hz := NULL]
sv[, hz := NULL]
#--------------------------------------------------

#--------------------------------------------------
# need to sum over health zones because the renaming functions combines two sets of two different health zones 
# unique idenitifiers test will fail without this
#--------------------------------------------------
vars = names(ssc)[!names(ssc) %in% "value"]

ssc = ssc[, .(value = sum(value)), by = vars]

sv = sv[, .(value = sum(value)), by = vars]
#--------------------------------------------------

#--------------------------------------------------
# check unique ids
#--------------------------------------------------
# test unique identifiers:
if (nrow(ssc) != nrow( unique( ssc[, .(date, health_zone, dps, element)]))) {
  stop('Unique identifiers do not uniquely identify rows in ssc')
}

if (nrow(sv) != nrow( unique( sv[, .(date, health_zone, dps, element)]))) {
  stop('Unique identifiers do not uniquely identify rows in ssc')
}
#--------------------------------------------------

#--------------------------------------------------
# Save the prepped data sets
#--------------------------------------------------
saveRDS(ssc, paste0(prepped_dir, prepped_ssc))
saveRDS(sv, paste0(prepped_dir, prepped_supervisions))
#--------------------------------------------------

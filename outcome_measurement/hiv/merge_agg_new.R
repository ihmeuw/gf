# ----------------------------------------------
# David Phillips, Caitlin O'Brien-Carelli
#
# 12/21/2018
# To acombine the data sets from the Uganda Viral Load Dashboard into a single data set
# ----------------------------------------------

# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(jsonlite)
library(httr)
library(stringr) # to help extract meta data from file names

# ----------------------------------------------
# Files and directories

# set directory
dir = "J:/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard"

# ---------------------
# upload both the webscrape data and the facility names as data tables

# upload the facilities names
facilities <- readRDS(paste0(dir, "/facilities.rds"))

# upload the uganda_vl data with month, year, sex filters
uganda_vl <- readRDS(paste0(dir, "/webscrape_agg/sex_data.rds"))


# ----------------------------------------------

# create an id variable in uganda_vl to merge on using facility_id
names(uganda_vl)
names(facilities)

uganda_vl[ , id:=facility_id]


# list unique facility ids
uganda_vl [,id, by=id] # 2042 values
facilities[, id, by=id] # 2012 values

# identify mismatches
uganda_vl$id[!uganda_vl$id %in% facilities$id]
facilities$id[!facilities$id %in% uganda_vl$id]

# format id as numeric
facilities[, id:=as.numeric(id)]

# add table 1 to uganda_vl 
# these values will repeat to match the number of values in the data table
uganda_vl <- merge(uganda_vl, facilities[,c('id','name')], by='id', all.x=TRUE)

# handle missing names
uganda_vl[is.na(name), name:=paste0('Facility #',id)]


# upload the district names
districts <- readRDS(paste0(dir, "/districts.rds"))

uganda_vl <- merge(uganda_vl, districts[, 'district_name'], by='district_id', all.x=TRUE)

# ----------------------------------------------

saveRDS(uganda_vl, paste0(dir,"/main.rds"))


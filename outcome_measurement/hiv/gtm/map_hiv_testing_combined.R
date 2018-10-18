# ----------------------------------------------
# Naomi Provost
# October 12, 2018
# Master code file for GTM HIV data graphing
# ----------------------------------------------
###### Set up R / install packages  ###### 
# ----------------------------------------------
rm(list=ls())
library(lubridate)
library(data.table)
library(readxl)
library(stats)
library(stringr)
library(rlang)
library(zoo)
library(raster)
library(rgeos)
library(ggplot2)
library(maptools)
library(RColorBrewer)

# ----------------------------------------------
#----------------------------------
# Set the directory to download the data
# detect if operating on windows or on the cluster 
root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

#------------ Prep Data------------------------
#### Prep Data
# define main directory
dir <- paste0(root, '/Project/Evaluation/GF/outcome_measurement/gtm/HIV/SIGSA/')
prep_dir <- paste0(root, '/Project/Evaluation/GF/outcome_measurement/gtm/prepped_data/')
outFile = paste0(root, '/Project/Evaluation/GF/outcome_measurement/gtm/visualizations/SIGSA_hiv_test_2ndRound.pdf')
mapping_dir = paste0(root, '/Project/Evaluation/GF/mapping/gtm/')

# Read in file
dt = readRDS(paste0(prep_dir, "hiv_patientlvl_combined.rds"))

# remove repeat positive tests for patients, only keep the first one
dt_graphing = dt
dt_graphing_pos = dt[hiv_screening_result == "REACTIVO"]
dt_graphing_pos[,keep := min(visit_num), by = id]
keep_list = unique(dt_graphing_pos[,.(id, keep)])

hiv_pos_patients = dt_graphing_pos$id
dt_neg = dt[!id %in% hiv_pos_patients]

dt_pos = dt[id %in% hiv_pos_patients]
dt_pos = merge(dt_pos, keep_list)
dt_pos = dt_pos[visit_num <= keep]
dt_pos$keep =NULL

total_dt = rbind(dt_neg, dt_pos)

dt_map = total_dt[,.(date, completed_hiv_screening_test, hiv_screening_result, risk_condition_eng, isTrans, isMSM, sex)]


# melt to make long
mapping_long = melt(dt_map, id.vars = c('date', 'sex', 'risk_condition_eng', "isMSM", "isTrans"))




setwd("C:/Users/elineb/Documents/gf/") 
# working directory should be the root of the repository

#-------------------------------------
# AUTHOR: Emily Linebarger / Audrey Batzel (modified 5/7/19)
# PURPOSE: Prep SSC data from PNLS
# DATE: April 2019
rm(list=ls())
#-------------------------------------

#--------------------------------------------------
# Set up R, and read in data 
#--------------------------------------------------
library(data.table)

# directories:
j = ifelse(Sys.info()[1]=='Windows','J:','/home/j')
dir = paste0(j, "Project/Evaluation/GF/outcome_measurement/cod/dhis_data/pre_prep/ssc/")
prepped_dir = paste0(j, "Project/Evaluation/GF/outcome_measurement/cod/dhis_data/prepped/")

ssc = readRDS(paste0(dir, "SSC_data_2017_2018.rds"))

base_services = readRDS(paste0(prepped_dir, "base_services_prepped.rds"))
catalog = fread(paste0(j, "Project/Evaluation/GF/outcome_measurement/cod/dhis_data/catalogues/data_elements_cod.csv"))

# functions
source('./core/standardizeDPSNames.R')
source('./core/standardizeHZNames.R')

# load data
#--------------------------------------------------

#--------------------------------------------------
# Standardize variable names  
#--------------------------------------------------
names(ssc) = tolower(names(ssc))

# standardize health zone and dps names (simplified this)
ssc[,]
ssc[, dps:=standardizeDPSNames(dps)]

#--------------------------------------------------
# Merge with element catalog 
#--------------------------------------------------
ssc = merge(ssc, catalog[, .(element, element_eng, element_id)], by='element', all.x=T) #Don't pull over any of the data set variables from the catalog here. 
#Check the merge
ssc[is.na(element_eng)] #Empty data table, so all matched. 
#--------------------------------------------------
# Create other variable names  
#--------------------------------------------------
ssc[, data_set:='ssc'] #Audrey do I need to pick one of the ones from the catalog? 
ssc[, country:="République Démocratique du Congo"] #We probably don't need this but just to keep things matched. 
ssc[, level:="health_zone"]
#--------------------------------------------------
# What variables are you missing?  
#--------------------------------------------------
names(base_services)[!names(base_services)%in%names(ssc)]


#--------------------------------------------------
# Save the prepped dataset 
#--------------------------------------------------
saveRDS(ssc, paste0(prepped_dir, "ssc_prepped.rds"))

#-------------------------------------
# AUTHOR: Emily Linebarger
# PURPOSE: Prep supervisions data from PNLS
# DATE: April 2019
#-------------------------------------

#--------------------------------------------------
# Set up R, and read in data 
#--------------------------------------------------
rm(list=ls())
library(data.table)
setwd("C:/Users/elineb/Documents/gf/") #Set to your repo

j = ifelse(Sys.info()[1]=='Windows','J:','/home/j')
raw_dir = paste0(j, "Project/Evaluation/GF/outcome_measurement/cod/dhis_data/pre_prep/supervisions/")
prepped_dir = paste0(j, "Project/Evaluation/GF/outcome_measurement/cod/dhis_data/prepped/")

raw_supervisions = readRDS(paste0(raw_dir, "Supervisions_2017_2018.rds"))
setDT(raw_supervisions)
base_services = readRDS(paste0(prepped_dir, "base_services_prepped.rds"))
catalog = fread(paste0(j, "Project/Evaluation/GF/outcome_measurement/cod/dhis_data/catalogues/data_elements_cod.csv"))

source('./core/standardizeDPSNames.R')

supervisions = copy(raw_supervisions)
#--------------------------------------------------
# Standardize variable names  
#--------------------------------------------------
names(supervisions) = tolower(names(supervisions))

#It looks like "hz" is the same as "health_zone" in base services. Standardize this. 
setnames(supervisions, "hz", "health_zone")
supervisions[, health_zone:=tstrsplit(health_zone, " ", fixed=TRUE, keep=c(2))]
supervisions[, health_zone:=tolower(health_zone)]

#check it worked - verify this with Audrey. 
unique(supervisions[!health_zone%in%base_services$health_zone, .(health_zone)])

#Standardize variable names for DPS. 
supervisions[, dps:=substr(dps, 4, nchar(dps))]
supervisions[, dps:=standardizeDPSNames(dps)]

#--------------------------------------------------
# Merge with element catalog 
#--------------------------------------------------
supervisions = merge(supervisions, catalog[, .(element, element_eng, element_id)], by='element', all.x=T) #Don't pull over any of the data set variables from the catalog here. 
#Check the merge
supervisions[is.na(element_eng)] #Empty data table, so all matched. 
#--------------------------------------------------
# Create other variable names  
#--------------------------------------------------
supervisions[, data_set:='supervisions'] #Audrey do I need to pick one of the ones from the catalog? 
supervisions[, country:="République Démocratique du Congo"] #We probably don't need this but just to keep things matched. 
supervisions[, level:="health_zone"]
#--------------------------------------------------
# What variables are you missing?  
#--------------------------------------------------
names(base_services)[!names(base_services)%in%names(supervisions)]


#--------------------------------------------------
# Save the prepped dataset 
#--------------------------------------------------
saveRDS(supervisions, paste0(prepped_dir, "supervisions_prepped.rds"))

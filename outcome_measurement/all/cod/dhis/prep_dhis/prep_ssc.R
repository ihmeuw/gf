#-------------------------------------
# AUTHOR: Emily Linebarger
# PURPOSE: Prep SSC data from PNLS
# DATE: April 2019
#-------------------------------------

#--------------------------------------------------
# Set up R, and read in data 
#--------------------------------------------------
rm(list=ls())
library(data.table)
setwd("C:/Users/elineb/Documents/gf/") #Set to your repo

j = ifelse(Sys.info()[1]=='Windows','J:','/home/j')
raw_dir = paste0(j, "Project/Evaluation/GF/outcome_measurement/cod/dhis_data/pre_prep/ssc/")
prepped_dir = paste0(j, "Project/Evaluation/GF/outcome_measurement/cod/dhis_data/prepped/")

raw_ssc = readRDS(paste0(raw_dir, "SSC_data_2017_2018.rds"))
setDT(raw_ssc)
base_services = readRDS(paste0(prepped_dir, "base_services_prepped.rds"))
catalog = fread(paste0(j, "Project/Evaluation/GF/outcome_measurement/cod/dhis_data/catalogues/data_elements_cod.csv"))

source('./core/standardizeDPSNames.R')

ssc = copy(raw_ssc)
#--------------------------------------------------
# Standardize variable names  
#--------------------------------------------------
names(ssc) = tolower(names(ssc))

#It looks like "hz" is the same as "health_zone" in base services. Standardize this. 
setnames(ssc, "hz", "health_zone")
ssc[, health_zone:=gsub(" Zone de Santé", "", health_zone)]
ssc[, health_zone:=tstrsplit(health_zone, " ", fixed=TRUE, keep=c(2))]
ssc[, health_zone:=tolower(health_zone)]

#check it worked - verify this with Audrey. 
unique(ssc[!health_zone%in%base_services$health_zone, .(health_zone)])

#Standardize variable names for DPS. 
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

#-----------------------------------------------------
# Initial absorption finding statements
# For synthesis report  - September 2019 
#-----------------------------------------------------

rm(list=ls())
library(data.table) 

#Read in absorption data 
cod = readRDS("J:/Project/Evaluation/GF/resource_tracking/_gf_files_gos/cod/prepped_data/absorption_cod.rds")
gtm = readRDS("J:/Project/Evaluation/GF/resource_tracking/_gf_files_gos/gtm/prepped_data/absorption_gtm.rds")
sen = readRDS("J:/Project/Evaluation/GF/resource_tracking/_gf_files_gos/sen/prepped_data/absorption_sen.rds")
uga = readRDS("J:/Project/Evaluation/GF/resource_tracking/_gf_files_gos/uga/prepped_data/absorption_uga.rds")

#Current grant periods 
current_grant_periods = c('2018-2018', '2016-2019', '2018-2020')

#Malaria 
for (loc in c('cod', 'gtm', 'sen', 'uga')){
  assign(paste0(loc, "_malaria"), get(loc)[grant_disease=="malaria" & grant_period%in%current_grant_periods, .(budget=sum(budget, na.rm=T), expenditure=sum(expenditure, na.rm=T)), 
    by=c('grant', 'grant_period', 'semester', 'gf_module', 'gf_intervention')])
  get(paste0(loc, "_malaria"))[, absorption:=round((expenditure/budget)*100, 2)]
} 

#Analyses by country 
cod_malaria[order(-absorption), .(gf_intervention, absorption)]

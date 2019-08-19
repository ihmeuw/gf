# Final cleaning on PNLS
# Use this to manually aggregate the final variables
# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2)
library(dplyr)
library(stringr) 
library(openxlsx)
# --------------------

# shell script for working on the cluster
# sh /share/singularity-images/rstudio/shells/rstudio_qsub_script.sh -p 1247 -s 2 

# --------------------
# set working directories

# detect if operating on windows or on the cluster 
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set the directory for input and output
# dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/')

# local directory
dir = "C:/Users/ccarelli/Documents/pnls_data"

#---------------------------------------
# load the file that represents a subset (no sex, age, or support)
# dt = data.table(readRDS(paste0(dir, 'prepped/pnls_sets/pnls_vct_2017_01_01_2019_02_01.rds')))

# load local data table
dt = data.table(readRDS(paste0(dir, '/pnls_vct_2017_01_01_2019_02_01.rds')))

# set the name of the set to clean
set_name = dt[ ,tolower(unique(pnls_set))]
# #---------------------------------------
# 
# # import the translated elements
# new_elements = data.table(read.xlsx(paste0(dir, 'meta_data/translate/pnls_elements_translations_', set_name, '.xlsx' )))

# import the translated elements locally
new_elements = data.table(read.xlsx(paste0(dir, '/pnls_elements_translations_', set_name, '.xlsx' )))

# drop out french elements for the merge 
new_elements[ ,element:=NULL]

# check to make sure you didn't include additional spaces
new_elements[, element_eng:=trimws(element_eng)]
#---------------------------------------
#---------------------------------------------------------------------
# Merge the data and the new English elements together 

# drop the previous english elements out the data for the corrected elements
dt[ ,c("maternity", "case","stock_category", "tb"):=NULL] # all of these elements are missing

dt = merge(dt, new_elements, by='element_id', all.x = TRUE)
#---------------------------------------------------------------------
# check the final translations 

# export only the french elements and their translations
export_file = dt[,.(elements=unique(element)), by=.(element_eng, subpop)]
write.xlsx(export_file, paste0(dir, 'meta_data/translate/pnls_elements_vct_check_translations.xlsx'))

#---------------------------------------------------------------------
# drop the useless element (these two elements have unique ids but identical variable names)

dt = dt[element_eng!='DROP' & element_eng!='Patients admitted to the hospital']

#---------------------------------------------------------------------
# fill in couples and patients for the missing sub-populations

dt[grepl("couple", tolower(element)) & is.na(subpop), subpop:='couple']
dt[grepl("malade", tolower(element)) & is.na(subpop), subpop:='patient']
dt[subpop=='serodisc', subpop:='couple'] # count serodiscordant among couples

#---------------------------------------------------------------------
# collapse to the appropriate values 

byVars = names(dt)[names(dt)!='element' & names(dt)!='element_id' & names(dt)!='last_update' & names(dt)!='coordinates' & names(dt)!='org_unit_type']
dt = dt[ ,.(value=sum(value)), by=byVars]

#---------------------------------------------------------------------
# create a funder binary
dt[dps=='Haut Katanga' | dps=='Lualaba', funder:='PEPFAR']
dt[!(dps=='Haut Katanga' | dps=='Lualaba'), funder:='The Global Fund']

#---------------------------------------------------------------------
# subset to only 2017 and 2018

dt = dt[date < '2019-01-01']
#---------------------------------------------------------------------
# format the data set according to plan

# change the variable name to the english versions
setnames(dt, 'element_eng', 'variable')

# re-arrange variables and drop set
dt = dt[ ,.(org_unit_id, org_unit, date, sex, age, subpop, variable, value, 
            facility_level, dps, health_zone, health_area, funder)]

# change the translation of client
dt[subpop=='customer', subpop:='client']

#---------------------------------------------------------------------
# export the final data 

saveRDS(dt, paste0(dir, "prepped/pnls_final/pnls_vct_final.rds"))
#---------------------------------------------------------------------




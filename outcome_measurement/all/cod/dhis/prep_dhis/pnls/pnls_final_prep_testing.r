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
dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/')

#---------------------------------------
# load the file that represents a subset (no sex, age, or support)
dt = data.table(readRDS(paste0(dir, 'prepped/pnls_sets/pnls_vct_2017_01_01_2019_02_01.rds')))

# missing sex and subpops
dt[is.na(sex), sex:='Couple']
dt[is.na(subpop) & grepl("Couple", element), sex:='Couple']

#---------------------------------------

dt[is.na(subpop) & grepl('Couples', element), subpop:='couples']

#---------------------------------------
# export the abbreviated elements for translation

# to do this on the cluster, you must export as an RDS, then use local code to save
elements = dt[ ,.(element = unique(element)), by=.(element_id)]
set_name = dt[ ,tolower(unique(pnls_set))]

# save the list as an excel file 
write.xlsx(elements, paste0(dir,'meta_data/translate/pnls_elements_to_translate_', set_name, '_june_download.xlsx' )) 

# translate using onlinedoctranslator.com and save as file path below
# should be: pnls_elements_translations, set, .xlsx

#---------------------------------------

# import the translated elements
new_elements = data.table(read.xlsx(paste0(dir, 'meta_data/translate/pnls_elements_translations_', set_name, '.xlsx' )))

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
# Make sure that the totals are correct for the new file 

# skipped one subpopulation
dt[is.na(subpop) & grepl("Couples", element), subpop:='couple']

# check that the elements were identified correctly
# View(dt[ , unique(element_eng), by=.(element, subpop)])
#--------------------------------------------------------------------
# # add a binary for testing type
# drop as we are not interested in this issue
# 
dt[grep('CDIV', element), test_type:='provider initiated']
dt[grep('CDV', element), test_type:='voluntary']

#----------------------------------------
# final collapse 

Vars = names(dt)[names(dt)!='value' & names(dt)!='element_id' & names(dt)!='element' & names(dt)!='last_update' & names(dt)!='coordinates']
dt = dt[,.(value=sum(value)), by=Vars]

#-----------------------------------
#Save the final file 
saveRDS(dt, paste0(dir, "prepped/pnls_final/pnls_vct_final.rds"))

#-----------------------------------





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
setwd(dir)

#---------------------------------------
# load the file that represents a subset (no sex or )
dt = readRDS(paste0(dir, 'prepped/pnls_sets/pnls_vct_2017_01_01_2018_12_01.rds'))

#------------------------------------
#Edit subpops where needed
dt[element_id=='bJSh3ipNspj', subpop:=NA] #Malades reçus en hospitalisation (Pédiatrie,MI, Chirurgie, gynécologie et autres)

#-----------------------------
# export the abbreviated elements for translation

# to do this on the cluster, you must export as an RDS, then use local code to save
elements = dt[ ,.(element = unique(element)), by=.(element_id)]
set = dt[ ,tolower(unique(set))]

# save the list as an excel file 
write.xlsx(elements, paste0(dir,'meta_data/translate/pnls_elements_to_translate_', set, 'jan_download.xlsx' )) #Leaving in for documentation; don't need to write over this file. 

# translate using onlinedoctranslator.com and save as file path below
#---------------------

# import the translated elements
new_elements = read.xlsx(paste0(dir,
                'meta_data/translate/pnls_elements_translations_', set, '.xlsx' ))
setDT(new_elements)

# drop out french elements for the merge 
new_elements[ ,element:=NULL]

# check to make sure you didn't include additional spaces
new_elements[, element_eng:=trimws(element_eng)]
#---------------------------------------

#---------------------------------------------------------------------
# Merge the data and the new English elements together 

# drop the previous english elements out the data for the corrected elements
dt[ ,c("element_eng", "org_unit_type", "mtk", "maternity", "case",
       "stock_category", "tb"):=NULL] # all org_units are facilities

dt = merge(dt, new_elements, by='element_id', all.x = TRUE)
#---------------------------------------------------------------------
# Make sure that the totals are correct for the new file 

# skipped one subpopulation
dt[is.na(subpop) & grepl("Couples", element), subpop:='couple']

# check that the elements were identified correctly
dt[ ,unique(element_eng), by=.(element, subpop)]
#--------------------------------------------------------------------
# add a binary for testing type

dt[grep('CDIV', element), test_type:='provider initiated']
dt[grep('CDV', element), test_type:='voluntary']

#----------------------------------------
# final collapse 

Vars = names(dt)[names(dt)!='value' & names(dt)!='element_id' & names(dt)!='element']
dt = dt[,.(value=sum(value)), by=Vars]

#-----------------------------------
#Save the final file 
saveRDS(dt, paste0(dir, "prepped/pnls_final/pnls_vct_final.rds"))

#-----------------------------------



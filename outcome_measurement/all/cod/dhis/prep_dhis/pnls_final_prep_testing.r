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

#-----------------------------
# export the abbreviated elements for translation

# to do this on the cluster, you must export as an RDS, then use local code to save
elements = dt[ ,.(element = unique(element)), by=.(element_id)]
set = dt[ ,tolower(unique(set))]

# save the list as an excel file 
write.xlsx(elements, paste0(dir,'meta_data/translate/pnls_elements_to_translate_', set, '.xlsx' )) #Leaving in for documentation; don't need to write over this file. 

# translate using onlinedoctranslator.com and save as file path below
#---------------------

# import the translated elements
new_elements = read.xlsx(paste0(dir,
                'meta_data/translate/pnls_elements_translations_', set, '.xlsx' ))
setDT(new_elements)
setnames(new_elements, 'element', 'element_eng')

# be sure 
x = merge(elements, new_elements, by=c('element_id'), all.x=T )
setDT(x)
#---------------------------------------

#---------------------------------------------------------------------
# Merge the data and the new English elements together 
new_elements = new_elements[, .(element_id, element_eng)]
new_elements[, element_eng:=trimws(element_eng)]
dt = dt[, -c('element_eng')] #Not sure why this is already in here? 

dt = merge(dt, new_elements, by='element_id', all.x = TRUE)
#---------------------------------------------------------------------


#--------------------------------------------------------------------
# Make sure that the totals are correct for the new file 
unique(dt[, .(subpop, element_eng, element)][order(subpop)]) #Visual checks
unique(dt[, .(element_eng)]) #Got this down to 19. 


#---------------------------------------------------------------------

#Save the final file 
saveRDS(dt, paste0(dir, "prepped/pnls_sets/pnls_", set, "_prepped_2017_01_01_2018_12_01.rds"))



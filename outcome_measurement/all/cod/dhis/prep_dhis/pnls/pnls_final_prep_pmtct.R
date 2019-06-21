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

dt = readRDS(paste0(dir, 'prepped/pnls_sets/pnls_pmtct_2017_01_01_2019_02_01.rds'))

#----------------------------------------------
#Add in subpops that weren't captured before
dt[grep("AMF", element), subpop:='AMF'] #Autres membres de familles
dt[element_id=='eVULOZGDgFZ', subpop:='plw']

#--------------------------------------------

#---------------------------------------------------
# Make a binary variable for EEV
dt[, eev:=grepl("EEV|Enfant", element)]
#---------------------------------------------------


#-----------------------------
# check the subpops and sexes in the indicators are all captured
# then strip them from the indicators

############## DOESN'T NEED TO BE RE-RUN, BUT LEAVING FOR DOCUMENTATION EKL 3/26/19 #########################
# dt[, unique(element)]
# unique(dt[is.na(subpop), .(element)])
# 
# #Remove subpop strings to review 
# dt[ ,element1:=tolower(element)]
# dt[ ,element1:=gsub("femmes enceintes ou allaitantes", "", element1)]
# dt[, element1:=gsub("femmes enc. ou allaitantes", "", element1)]
# dt[, element1:=gsub("femmes enceintes et allaitantes", "", element1)]
# dt[, element1:=gsub("femmes enceintes ouallaitantes", "", element1)]
# dt[, element1:=gsub("femmes enceintes", "", element1)]
# dt[ ,element1:=gsub("eev", "", element1)]
# dt[, element1:=gsub("partenaires masculins", "", element1)]
# dt[, element1:=gsub("enfants exposés", "", element1)]
# dt[, element1:=gsub("couples discordants", "", element1)]
# dt[, element1:=gsub("amf", "", element1)]
# 
# #
# 
# dt[, element1:=trimws(element1)]
# 
# dt[, unique(element1)]
###############################################################################################################

#-----------------------------
# export the abbreviated elements for translation

# to do this on the cluster, you must export as an RDS, then use local code to save
elements = dt[ ,.(element = unique(element)), by=.(element_id)]
set = "pmtct"

# save the list as an excel file 
#write.xlsx(elements, paste0(dir,'meta_data/translate/pnls_elements_to_translate_', set, '.xlsx' )) #Leaving in for documentation; don't need to write over this file. 

# translate using onlinedoctranslator.com and save as file path below
#---------------------

# import the translated elements
new_elements = read.xlsx(paste0(dir, 'meta_data/translate/pnls_elements_translations_', set, '.xlsx' ))
setDT(new_elements)

# be sure 
x = merge(elements, new_elements, by=c('element_id', 'element'), all.x=T )
setDT(x)
stopifnot(nrow(x[is.na(element_eng)])==0) #Make sure everything merged. 
#---------------------------------------

#---------------------------------------------------------------------
# Merge the data and the new English elements together 
new_elements = new_elements[, .(element_id, element_eng, eev_test)]
new_elements[, element_eng:=trimws(element_eng)]

dt = merge(dt, new_elements, by='element_id', all.x = TRUE)
stopifnot(nrow(dt[is.na(element_eng)])==0)
#---------------------------------------------------------------------


#--------------------------------------------------------------------
# Make sure that the totals are correct for the new file 
unique(dt[, .(subpop, element_eng)][order(subpop, element_eng)]) #Visual checks
unique(dt[, .(element_eng)])

#Check what's going on with 'service' variable, and then drop it.
dt[element=='"Femmes enc. ou allaitantes informées des résultats-service', sum(value, na.rm = TRUE)]
dt[element=="Femmes enc. ou allaitantes informées des résultats", sum(value, na.rm = TRUE)]

dt[element=='Femmes enc. ou allaitantes testées-service', sum(value, na.rm = TRUE)] #Flag - these are different values!
dt[element=='Femmes enceintes ou allaitantes testées', sum(value, na.rm = TRUE)]

#Do a deeper dive
table(dt[element=='Femmes enceintes ou allaitantes testées', .(value, na.rm=T)])
table(dt[element=='Femmes enc. ou allaitantes testées-service', .(value, na.rm=T)])

#One more pair 
dt[element=='Femmes enc. ou allaitantes conseillées-service', sum(value, na.rm=TRUE)] #These are also slightly off. 
dt[element=='Femmes enceintes ou allaitantes conseillées', sum(value, na.rm = TRUE)]


#Drop out the variables with "service" at the end - doing this by element ID because they don't all have hyphens. 
unique(dt[grep("service", element), .(element, element_id)])
dt = dt[!(element_id == "PnwLDzr2HX8" | element_id == "sIauzwAH8Oo" | element_id == "umSBpgIOOU1" | element_id == "vRJFy0MuKiw" | element_id == "wlm4P0V0ySD")]
unique(dt[grep("service", element), .(element, element_id)])
#---------------------------------------------------------------------


#------------------------------------------------------------------
# Collapse the variables 
print(paste0("There were ", nrow(dt), " rows of data before collapse."))

Vars = names(dt)[names(dt)!='value' & names(dt)!='element_id' & names(dt)!='element' & names(dt)!='last_update' & names(dt)!='coordinates']
dt = dt[,.(value=sum(value)), by=Vars]

print(paste0("There are ", nrow(dt), " rows of data after collapse."))


#Save the final file 
saveRDS(dt, paste0(dir, 'prepped/pnls_final/pnls_pmtct.rds'))

#------------------------------------------------------------------------------------------
#Save one version of the dataset with just HIV+ and tested for Caitlin to merge on VCT data 

for_vct_merge = dt[subpop%in%c('plw', 'exposed_infant')&element_eng%in%c('HIV+', 'Tested', 'Counseled'), 
                   .(value=sum(value, na.rm=T)), by=c('org_unit_id', 'date', 'org_unit', 'dps', 'health_zone', 
                                                       'health_area', 'org_unit_type', 'facility_level', 'pnls_set', 
                                                       'subpop', 'sex', 'age', 'element_eng')]
saveRDS(for_vct_merge, paste0(dir, 'prepped/pnls_final/pmtct_for_vct_merge.rds'))



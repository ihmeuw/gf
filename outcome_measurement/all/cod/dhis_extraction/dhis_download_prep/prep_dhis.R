# Prep the data from SNIS DHIS2 
# Includes outlier screening using quantile regression

# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 9/8/2018
#
# Upload the merged RDS data sets from DHIS2 
# prep the data sets for analysis 

# ----------------------------------------------
# shell script to 
# sh /share/singularity-images/rstudio/shells/rstudio_qsub_script.sh -p 1327 -s 10 -P snis

# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2)
library(dplyr)
library(stringr) 
# --------------------

# --------------------
# set working directories 

# detect if operating on windows or on the cluster 
root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set the directory for input and output
dir <- paste0(root, '/Project/Evaluation/GF/outcome_measurement/cod/dhis/')

# set the folder for 
folder <- 'pre_prep/merged/'

#--------------------
# upload the data set and convert it to a data table

# change the file to the file you want to upload!
# base, sigl, or pnls file to upload, clean, and prep
file <- 'pnls_drc_01_2017_07_2018'
date_end = '2018-08-01'

# import the data set for cleaning and prep 
dt <- readRDS(paste0(dir, folder, file, '.rds'))

#-----------------------------------------
# convert value to a numeric 
dt[ ,value:=as.numeric(as.character(value))]
#------------------------------------------

# merge in the english names for the data elements and element type

# read in the csv with english translations 
elements <- data.table(read.csv(paste0(dir, 'catalogues/data_elements_cod.csv'), stringsAsFactors=F))

# reset the names for the merge
setnames(elements, 'element', 'element_eng')
elements[ , c('data_set_id', 'data_set_fr', 'element_fr', 'data_set'):=NULL]

# merge in the english names for the data elements and element type
dt <- merge(dt, elements, by='element_id', all.x=TRUE )

#-----------------------------------------------
# subset to only the variables you want to keep
dt <- dt[keep==1]
dt[ , keep:=NULL]

#-----------------------------------------------
# convert last_update and opening_date to date variables
dt[ , last_update:=as.character(last_update)]
dt[ , opening_date:=as.character(opening_date)]

dt$last_update <- unlist(lapply(strsplit(dt$last_update, "T"), "[", 1))
dt$opening_date <- unlist(lapply(strsplit(dt$opening_date, "T"), "[", 1))

#--------------------------------------
# replace the dps/hz with just the name, excluding the code and word 'province'

# replace dps
dt$dps1 <- unlist(lapply(strsplit(dt$dps, " "), "[", 2))
dt$dps2 <- unlist(lapply(strsplit(dt$dps, " "), "[", 3))
dt[dps2!='Province', dps:=paste(dps1, dps2)]
dt[dps2=='Province', dps:=dps1]
dt[ , c('dps1', 'dps2'):=NULL]

# replace health zone
dt$health_zone1 <- unlist(lapply(strsplit(dt$health_zone, " "), "[", 2))
dt$health_zone2 <- unlist(lapply(strsplit(dt$health_zone, " "), "[", 3))
dt[health_zone2!='Zone', health_zone:=paste(health_zone1, health_zone2) ]
dt[health_zone2=='Zone', health_zone:=health_zone1]
dt[ , c('health_zone1', 'health_zone2'):=NULL]

#---------------------
# replace health zone when the facility is a health zone 
# when data are reported at the hz level, health zone is missing

dt[level=='health_zone', health_zone1:=org_unit]

# replace health zone
dt$health_zone2 <- unlist(lapply(strsplit(dt$health_zone1, " "), "[", 2))
dt$health_zone3 <- unlist(lapply(strsplit(dt$health_zone1, " "), "[", 3))
dt[health_zone3!='Zone', health_zone1:=paste(health_zone2, health_zone3) ]
dt[health_zone3=='Zone', health_zone1:=health_zone2]

dt[level=='health_zone' ,health_zone:=health_zone1]
dt[ , c('health_zone1', 'health_zone2', 'health_zone3'):=NULL]



#-----------------------------------------------
# add a variable to demarcate the provincial approach provinces
dt[dps=='Maniema' | dps=='Tshopo' | dps=="Kinshasa", mtk:='Yes']
dt[is.na(mtk), mtk:='No']

#-----------------------------------------------
# organize the data table in an intuitive order 
dt <- dt[ ,.(element, element_eng, org_unit, date, category, value, 
             dps, health_zone, health_area, level, org_unit_type, 
              type, mtk, drug, tableau, coordinates, opening_date, last_update, 
               data_set, org_unit_id=id, element_id, month, year)]


#--------------------------------
# save the prepped data up to outlier detection 
saveRDS(dt, paste0(dir, 'prepped/', file, '_prepped.rds'))

#-----------------------------------------------
# save a prepped tableau data set, 2017 - present
tabl <- dt[tableau==1 & (year=='2017' | year=='2018')]
tabl <- tabl [date < date_end]

# get the name for the file
name <- strsplit(file, '_')[[1]][1]

# eliminate the extraneous categories from pnls elements 
if (name=='pnls') {

sortie = tabl[element_id=='jJuipTLZK4o' & category=='Sortie']
tabl = tabl[element_id!='jJuipTLZK4o']
tabl = rbind(tabl, sortie)

return(table)

}

# save the file
saveRDS(tabl, paste0(dir, 'prepped/tabl_', name, '.rds'))

#--------------------------------------------------







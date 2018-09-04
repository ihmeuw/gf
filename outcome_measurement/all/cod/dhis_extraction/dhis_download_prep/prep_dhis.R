# Prep the data from SNIS DHIS2 

# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 9/4/2018
#
# Upload the merged RDS data sets from DHIS2 
# prep the data sets for analysis 

# ----------------------------------------------

# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(jsonlite)
library(httr)
library(ggplot2)
library(dplyr)
library(xlsx) # does not work on the cluster
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
file <- 'sigl_drc_01_2015_07_2018'
set_name <- 'sigl'  # change the name to the correct data set for the saved file

# import the data set for cleaning and prep 
dt <- readRDS(paste0(dir, folder, file, '.rds'))

#-----------------------------------------
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
setnames(dt, 'type', 'org_unit_type')

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

#-----------------------------------------------
# add a variable to demarcate the provincial approach provinces
dt[dps=='Maniema' | dps=='Tshopo' | dps=="Kinshasa", mtk:='Yes']
dt[is.na(mtk), mtk:='No']

#-----------------------------------------------
# organize the data table in an intuitive order 





#-------------------------
# save the final data set

saveRDS(dt, paste0(dir, 'prepped_data/', set_name, '.rds'))

#-------------------------------------------------------------------------
# TABLEAU
# create a tableau-specific data set and save it

# check that dt has all the correct tableau elements
dt[tableau==1, unique(element_eng)]

# subset to the relevant variables for tableau
tabl <- dt[tableau==1]
tabl <- tabl[year=='2017' | year=='2018']

#----------------------------
# create age and sex variables 
tabl[ ,unique(category)] # check that no new categories have been added

# over 5 years of age
tabl[category==">5 ans" , age:='5 +']
tabl[category=="Féminin, 5 ans et plus", age:='5 +']
tabl[category=="Masculin, 5 ans et plus" , age:='5 +']

# under 5 years of age
tabl[category=="<5 ans" , age:='Under 5']
tabl[category=="Féminin, Moins de 5 ans", age:='Under 5']
tabl[category=="Masculin, Moins de 5 ans", age:='Under 5']

# create a sex variable
tabl[category=="Féminin, 5 ans et plus" | category=="Féminin, Moins de 5 ans", sex:='Female']
tabl[category=="Masculin, 5 ans et plus" | category=="Masculin, Moins de 5 ans", sex:='Male']

#------------------------
# test graph of tableau elements to confirm it worked 
test_tabl <- tabl[ ,.(count=sum(value)), by=.(element_eng, date)]

ggplot(test_tabl, aes(x=date, y=count)) +
  geom_point() +
  geom_line() +
  facet_wrap(~element_eng) +
  theme_bw() +
  scale_y_continuous(labels = scales::comma)

#----------------------
# save the tableau data set
# this data set will be merged with other tableau data 
saveRDS(tabl, paste0(dir, 'tableau/', set_name, '_tabl.rds'))

#----------------------
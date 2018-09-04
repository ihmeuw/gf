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

# import the data set for cleaning and prep 
dt <- readRDS(paste0(dir, folder, file, '.rds'))

#-----------------------------------------
# merge in the english names for the data elements and element type

# read in the csv with english translations 
elements <- data.table(read.csv(paste0(dir, 'catalogues/data_elements_cod.csv'), stringsAsFactors=F))

# reset the names for the merge
setnames(elements, 'element', 'element_eng')
elements[ ,c('data_set_id', 'data_set_fr', 'element_fr', 'data_set'):=NULL]
setnames(dt, 'data_element_ID', 'element_id')

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
# create province and health zone names without the province code or word 'province'
dt$dps1 <- unlist(lapply(strsplit(dt$dps, " "), "[", 2:3))









dt$ health_zone <- unlist(lapply(strsplit(dt$health_zone, " "), "[", 2))

# add code to fix health zone also#
dt[ , health_zone:=(substr(dt$dps, 1, 2))]

#-----------------------------------------------
# add a variable to demarcate the provincial approach provinces
dt[dps=='Maniema' | dps=='Tshopo' | dps=="Kinshasa", mtk:='Yes']
dt[is.na(mtk), mtk:='No']

#-----------------------------------------------
# organize the data table in an intuitive order 

#-------------------------
# save the final data set

saveRDS(dt, paste0(dir, 'prepped_data/dt.rds'))

#-------------------------------------------------------------------------
# TABLEAU
# create a tableau-specific data set and save it

# check that dt has all the correct tableau elements
dt[tableau==1, unique(element_eng)]

# subset to the relevant variables for tableau
tabl <- dt[tableau==1]

#----------------------------
# create age and sex variables 
tabl[ ,unique(category)]

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

# create a data table just for the tableau elements and save it
tabl_dt <- dt[tableau==1]
tabl_dt <- tabl_dt[year==2017 | year==2018]

saveRDS(tabl_dt, paste0(dir, 'tableau/tabl_dt.rds'))

#------------------------
# test graph of tableau elements to confirm it worked 
try <- tabl_dt[ ,.(count=sum(value)), by=.(element_eng, date)]

ggplot(try, aes(x=date, y=count)) +
  geom_point() +
  geom_line() +
  facet_wrap(~element_eng) +
  theme_bw() +
  scale_y_continuous(labels = scales::comma)

#----------------------


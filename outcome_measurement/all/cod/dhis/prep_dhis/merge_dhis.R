# Merge the Base Services, SIGL, and PNLS data downloaded from DHIS2 DRC (SNIS)
# Caitlin O'Brien-Carelli
#
# 1/14/2018
#
# Upload the RDS data from DHIS2 and merge with the meta data 
# prep the data sets for analysis and the Tableau Dashboard
# ----------------------------------------------

# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2)
library(dplyr)
library(stringr) 
# --------------------
# merge on the cluster
# files take a long time to load - merge in a cluster IDE

# sh /share/singularity-images/rstudio/shells/rstudio_qsub_script.sh -p 1527 -s 1 -P snis_merge

# ---------------------------------
# set working directories

# detect if operating on windows or on the cluster 
root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set the directory for input and output
dir = paste0(root, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/')

# source the merge functions 
source("C:/Users/ccarelli/local/gf/outcome_measurement/all/cod/dhis/prep_dhis/merge_functions.R")
#---------------------------------

#---------------------------------
# change the arguments to upload the data sets 

# change the folder to the name of the data set you want to merge
folder = 'base'

# set the working directory and read in the files
setwd(paste0(dir, 'pre_prep/', folder, '/'))

# list the files in the working directory
files = list.files('./', recursive=TRUE)

# read in the files 
i = 1
for(f in files) {
  #load the RDs file
  vec = f
  current_data = data.table(readRDS(f))
  current_data[ ,file:=vec]
  
  # append to the full data 
  if(i==1) dt = current_data
  if(i>1)  dt = rbind(dt, current_data)
  i = i+1
}

#---------------------------------
# delete overlapping dates 

# create a date variable 
dt[ , period:= as.character(period)]
dt[ , year:=substr(period, 1, 4)]
dt[ , month:=substr(period, 5, 6)]
dt[ , date:=as.Date(paste(year, month, '01', sep='-'), '%Y-%m-%d')]
dt[ , c('period', 'year', 'month'):=NULL]

# drop out entries for which date is missing
dt = dt[!is.na(date)]

# view dates and determine which dates overlap
dt[ ,unique(file)]

# eliminate the overlapping dates 
test = dt[ ,.(date=unique(date)), by=file]
test = test[duplicated(date)]
print(paste('The following dates overlap:', test))

# remove overlapping dates - automate 

dt[ , value:=as.character(value)] # remove the factor to avoid errors
#---------------------------------------------
# merge in the meta data 











# save a merged rds file 

min = dt[ , min(date)]
min = gsub('-', '_', min)
max = dt[ , max(date)]
max = gsub('-', '_', max)

saveRDS(dt, paste0('pre_prep/merged/', folder, '_drc_', min, '_', max, '.rds' ))


# base services 

# remove the overlapping dates
dt = as.Date('2018-01-01', '%Y-%m-%d') # dt represents the first month of overlap
base1 = overlap(base1, dt)




# Merge the PNLS data 

# input the name of the most recently merged data set (change file path to 'merged' folder)
pnls1 = data.table(readRDS(paste0(dir, 'pre_prep/pnls/pnls_drc_01_2017_04_2018.rds')))

# load the newest data set
pnls2 = data.table(readRDS(paste0(dir, 'pre_prep/pnls/pnls_drc_01_2018_07_2018.rds')))

#---------------------------------
# remove the overlapping dates
# dt should represent the first month of overlap
# run the overlap function on the least recent of the overlapping sets 
dt = as.Date('2018-01-01', '%Y-%m-%d') 
pnls1 = overlap(pnls1, dt)

#----------------------------------
# merge the previously downloaded data set with the new download
pnls = rbind(pnls1, pnls2)

# merge in the meta data 
pnls = merge_meta_data(pnls)

# save the merged data
# alter the file name to include all included dates - pnls is only 2017 on currently
saveRDS(pnls, paste0(dir, 'pre_prep/merged/pnls_drc_01_2017_07_2018.rds'))

# once pnls has earlier data downloaded (2015/16) add code to save a 2017/18 subset

#------------------------------------------------ 
# Merge the PNLT data 

# input the name of the most recently merged data set (change file path to 'merged' folder)
pnlt1 <- data.table(readRDS(paste0(dir, 'pre_prep/pnlt/pnlt_drc_01_2017_12_2017.rds')))

# load the newest data set
pnlt2 <- data.table(readRDS(paste0(dir, 'pre_prep/pnlt/pnlt_drc_01_2018_03_2018.rds')))

# merge the previously downloaded data set with the new download
pnlt <- rbind(pnlt1, pnlt2)

# merge in the meta data 
pnlt <- merge_meta_data(pnlt)

# date does not function in pnlt - quarterly 
pnlt[ ,date:=NULL]
pnlt[month=='Q1', month:='01']
pnlt[month=='Q2', month:='04']
pnlt[month=='Q3', month:='07']
pnlt[month=='Q4', month:='10']
pnlt[ , date:=as.Date(paste(year, month, '01', sep='-'), '%Y-%m-%d')]

# save the merged data
# alter the file name to include all included dates - pnls is only 2017 on currently
saveRDS(pnlt, paste0(dir, 'pre_prep/merged/pnlt_drc_01_2017_03_2018.rds'))

#-----------------------------------------


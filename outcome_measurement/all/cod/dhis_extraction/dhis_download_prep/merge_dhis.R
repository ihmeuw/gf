# Merge the Base Services, SIGL, and PNLS data downloaded from DHIS DRC (SNIS)
# Caitlin O'Brien-Carelli
#
# 9/4/2018
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

# --------------------
# set working directories

# detect if operating on windows or on the cluster 
root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set the directory for input and output
dir <- paste0(root, '/Project/Evaluation/GF/outcome_measurement/cod/dhis/')

#--------------------------------------------------------
# create a function that uploads the meta data and merges it into the data table

merge_meta_data <- function(x) { 

# import the meta data for the merge
facilities <- data.table(readRDS(paste0(dir, 'all_units/master_facilities.rds')))

data_elements <- data.table(readRDS(paste0(dir, 'meta_data/updated_data_elements.rds')))
data_elements[ ,url_list:=NULL]

data_elements_categories <- data.table(readRDS(paste0(dir, 'meta_data/data_elements_categories.rds')))
data_elements_categories[ ,url_list:=NULL]

org_units_description <- data.table(readRDS(paste0(dir, 'meta_data/org_units_description.rds')))
org_units_description[ ,c('.id', 'active', 'parent_id'):=NULL]

#-------------------
# change the names of the ID variables in element categories and descriptions to match for the merge
setnames(data_elements, 'displayName', 'element')
setnames(data_elements_categories, c('ID', 'displayName'), c('category', 'category_name'))

#-------------------
# merge in the meta data 

  # change the organisational unit id to be called 'id'
  setnames(x, 'org_unit_ID', 'id')
  x[ ,group:=NULL]
  
  # create a date variable
  x[ , period:= as.character(period)]
  x[ , year:=substr(period, 1, 4)]
  x[ , month:=substr(period, 5, 6)]
  x[ , date:=as.Date(paste(year, month, '01', sep='-'), '%Y-%m-%d')]
  x[ , period:=NULL]
  
  # merge in the meta data 
  y <- merge(x, facilities, by='id')
  y <- merge(y, data_elements, by='data_element_ID')
  y <- merge(y, data_elements_categories, by='category')
  y <- merge(y, org_units_description, by='id')
  y <- data.table(y)
 
  # fix names for the prep
  y [ , c('category', 'name', 'datasets_ID'):=NULL] 
  setnames(y, 'category_name', 'category')
  setnames(y, 'datasets_name', 'data_set')
  setnames(y, 'data_element_ID', 'element_id')
  setnames(y, 'type', 'org_unit_type')
  
  return(y)
  
}

#----------------------------------
# create a function to remove overlapping dates - keep all dates before dt
# where x is the data table and dt is the first date you want to remove

overlap <- function(x, dt) { 

# function to delete overlapping dates 
x[ , period1:= as.character(period)]
x[ , year:=substr(period, 1, 4)]
x[ , month:=substr(period, 5, 6)]
x[ , date:=as.Date(paste(year, month, '01', sep='-'), '%Y-%m-%d')]

x <- x[date < dt]
print(unique(x$date))
x[ , c('period1', 'year', 'month', 'date'):=NULL]

return(x) }


#--------------------
# Merge the base services data sets you have downloaded

# input the file name of the most recently merged data set (change file path to 'merged' folder)
base1 <- data.table(readRDS(paste0(dir, 'pre_prep/base/base_services_drc_01_2015_04_2018.rds')))

# load the newest set of data 
base2  <- data.table(readRDS(paste0(dir, 'pre_prep/base/base_drc_02_2018_09_2018.rds')))

#---------------------------------
# remove the overlapping dates
dt <- as.Date('2018-01-01', '%Y-%m-%d') # dt represents the first month of overlap
base1 <- overlap(base1, dt)
#------------------------------------

# merge the previously downloaded set with the new download
base <- rbind(base1, base2)

# merge in the meta data 
merge_base <- merge_meta_data(base)

# save the merged data 
# alter the file name to include all included dates
saveRDS(merge_base, paste0(dir, 'pre_prep/merged/base_services_drc_01_2015_09_2018.rds'))

# create a data set of only 2017 - present data 
base_new <- merge_base[year=='2017' | year=='2018']
saveRDS(base_new, paste0(dir, 'pre_prep/merged/base_services_drc_01_2017_09_2018.rds'))
#----------------------------------------------
# Merge the SIGL data 

# input the name of the most recently merged data set (change file path to 'merged' folder)
sigl1 <- data.table(readRDS(paste0(dir, 'pre_prep/sigl/sigl_drc_01_2015_05_2018.rds')))

# load the newest data set
sigl2 <- data.table(readRDS(paste0(dir, 'pre_prep/sigl/sigl_drc_01_2018_07_2018.rds')))

#------------------------
# remove overlap - only if data sets overlap, use most recent data 
dt <- as.Date('2018-01-01', '%Y-%m-%d') # all dates before dt
sigl1 <- overlap(sigl1, dt)
#-------------------------

# merge the previously downloaded data set with the new download
sigl <- rbind(sigl1, sigl2)

# merge in the meta data 
sigl <- merge_meta_data(sigl)

# save the merged data
# alter the file name to include all included dates
saveRDS(sigl, paste0(dir, 'pre_prep/merged/sigl_drc_01_2015_07_2018.rds'))

# create a data set of only 2017 - present data 
sigl_new <- sigl[year=='2017' | year=='2018']
saveRDS(sigl_new, paste0(dir, 'pre_prep/merged/sigl_drc_01_2017_07_2018.rds'))

#------------------------------------------------ 
# Merge the PNLS data 

# input the name of the most recently merged data set (change file path to 'merged' folder)
pnls1 <- data.table(readRDS(paste0(dir, 'pre_prep/pnls/pnls_drc_01_2017_04_2018.rds')))

# load the newest data set
pnls2 <- data.table(readRDS(paste0(dir, 'pre_prep/pnls/pnls_drc_01_2018_07_2018.rds')))

#---------------------------------
# remove the overlapping dates
dt <- as.Date('2018-01-01', '%Y-%m-%d') # dt represents the first month of overlap
pnls1 <- overlap(pnls1, dt)
#------------------------------------

# merge the previously downloaded data set with the new download
pnls <- rbind(pnls1, pnls2)

# merge in the meta data 
pnls <- merge_meta_data(pnls)

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


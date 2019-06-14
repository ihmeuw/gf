# Merge the Base Services, SIGL, and PNLS data downloaded from DHIS2 DRC (SNIS)
# Audrey Batzel from Caitlin's code
#
# updated 06/05/2019
#
# Upload the RDS data from DHIS2 and merge with the meta data 
# prep the data sets for analysis and the Tableau Dashboard
# ----------------------------------------------

# --------------------
# Set up R
# --------------------
rm(list=ls())
library(data.table)
library(ggplot2)
library(dplyr)
library(stringr) 
library(openxlsx)
library(lubridate)

# --------------------

#---------------------------------
# merge on the cluster
# files take a long time to load - merge in a cluster IDE

# script to open a long-lasting large IDE
# qsub -terse -N rst_ide -q all.q -l fthread=10 -l m_mem_free=30G -l h_rt=70:00:00 -l archive=TRUE -P proj_pce /ihme/code/jpy_rstudio/jpy_rstudio_shell.sh -i /ihme/singularity-images/rstudio/ihme_rstudio_3501.img -t rstudio -p 1247 -o 1 -G r

# change the folder to the name of the data set you want to merge
# this is the only argument to change 

folder = 'sigl'
#---------------------------------

# --------------------------------
# set working directories
#---------------------------------
# detect if operating on windows or on the cluster 
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set the directory for input and output
dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/')
dir_pre_prep = paste0(dir, 'pre_prep/', folder, '/')

# source the merge and prep functions from the J Drive
source(paste0(dir, 'code/merge_functions_for_new_download.R'))
#---------------------------------

#---------------------------------
# read in the file
#---------------------------------
# list the files in the working directory
files = list.files(dir_pre_prep, recursive=TRUE)
file = files[grepl("combined", files)]
file = file[!grepl("archive", file)]

# read in the file
dt = data.table(readRDS(paste0(dir_pre_prep, file)))
#---------------------------------

#---------------------------------
# create a vector of variables to subset the larger data sets
#---------------------------------
if (folder=='base' | folder=='sigl') {
  keep_vars = read.xlsx(paste0(dir, 'catalogues/data_elements_cod.xlsx'))
  keep_vars = data.table(keep_vars)
  keep_vars[ , keep:=as.numeric(keep)]
  keep_vars = keep_vars[keep==1, element_id]
}

# subset to only the variables needed since they are large data sets
if (folder=='base' | folder=='sigl') {
  dt[ , data_element_ID:=as.character(data_element_ID)]
  dt = dt[data_element_ID %in% keep_vars]
}
#---------------------------------

#---------------------------------
# drop diacritical marks
# sourcing this function on the cluster fails 
#---------------------------------
fix_diacritics = function(x) {
  replacement_chars = list('S'='S', 's'='s', 'Z'='Z', 'z'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
                           'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
                           'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
                           'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
                           'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y')
  
  replace_me = paste(names(replacement_chars), collapse='')
  replace_with = paste(replacement_chars, collapse = '')
  return(chartr(replace_me, replace_with, x))
}
#---------------------------------

#---------------------------------
# do some initial formatting 
#---------------------------------
# remove the factoring of value to avoid errors
# introduces some NAs as some values are NULL
dt[ , value:=as.character(value)]
dt[ , value:=as.numeric(value)]
print(paste0("There are ", nrow(dt[is.na(value), ]), " missing values in the raw data."))

# add a date variable
dt[ , date:=ymd(paste0(as.character(period), '01'))]

# create a date variable based on last update 
dt[ , last_update:=as.character(last_update)]
dt[ , last_update:=sapply(str_split(last_update, 'T'), '[', 1)]
#---------------------------------

#---------------------------------
# save the interim raw data before the merge with the meta data 
#---------------------------------
# include the date range in the file name
min_date = dt[ , min(date)]
min_date = gsub('-', '_', min_date)
max_date = dt[ , max(date)]
max_date = gsub('-', '_', max_date)

# save the raw data before the merge 
saveRDS(dt, paste0(dir, 'pre_prep/', folder, '/', folder, '_', min_date, '_', max_date, '_full_initial_prep.rds'))
#dt = readRDS(paste0(dir, 'pre_prep/base/base_2018_01_01_2019_04_01_full_initial_prep.rds'))
#---------------------------------

#---------------------------------
# check unique identifiers:
#---------------------------------
byVars = names(dt)[! names(dt) %in% c('download_number', 'file', 'value')]
if( nrow(unique(dt[, byVars, with = FALSE])) != nrow(dt)) stop( 'Unique identifiers do not uniquely identify the data...')
#---------------------------------

#---------------------------------
# merge data before 2017 from previous download (will need to check this data later 
# because the data in the same folder might be from before the download was fixed...)
#---------------------------------
if (folder == "base"){
  # instead of using the overlap() function, just do this manually
  dt2 = readRDS(paste0( dir_pre_prep, 'base_01_2016_01_2019.rds'))
  
  dt2 = as.data.table(dt2)
  dt2[ , data_element_ID:=as.character(data_element_ID)]
  dt2 = dt2[data_element_ID %in% keep_vars]
  dt2[ , date:=ymd(paste0(as.character(period), '01'))]
  
  dt2 = dt2[date <= "2017-12-01", ]
  
} else if (folder == "sigl"){
  # instead of using the overlap() function, just do this manually
  dt2 = readRDS(paste0( dir_pre_prep, 'sigl_01_2016_01_2019.rds'))
  
  dt2 = as.data.table(dt2)
  dt2[ , data_element_ID:=as.character(data_element_ID)]
  dt2 = dt2[data_element_ID %in% keep_vars]
  dt2[ , date:=ymd(paste0(as.character(period), '01'))]
  
  dt2 = dt2[date <= "2016-12-01", ]
  
}

dt2[ , value:=as.character(value)]
dt2[ , value:=as.numeric(value)]
dt2[ , last_update:=as.character(last_update)]
dt2[ , last_update:=sapply(str_split(last_update, 'T'), '[', 1)]

vars = names(dt2)
dt = dt[, vars, with = FALSE]

dt = rbind(dt2, dt)

# # do still need this part of overlap():
# if(folder=='pnlt' | folder=='tb_pati_v_registered' | folder=='tb_pati_v_result') {
#   # pnlt data is quarterly - create date from qurter (start month)
#   x[ , period:= as.character(period)]
#   x[ , year:=substr(period, 1, 4)]
#   x[ , quarter:=substr(period, 5, 6)]
#   x[quarter=='Q1', month:='01']
#   x[quarter=='Q2', month:='04']
#   x[quarter=='Q3', month:='07']
#   x[quarter=='Q4', month:='10']
#   x[ , date:=as.Date(paste(year, month, '01', sep='-'), '%Y-%m-%d')]
#   x[ , c('period', 'year', 'month'):=NULL]
# } 
#---------------------------------

#---------------------------------
# merge in the meta data 
# includes english translations

# dt = merge_meta_data(dt)
x = copy(dt)

# this used to be in the meta data function, but it isn't working so I'm running it step by step:
#------------------
# import the meta data for the merge

# once the master facilities list if updated, read in master facilities
facilities = data.table(readRDS(paste0(dir, 'meta_data/master_facilities.rds')))
data_elements = data.table(readRDS(paste0(dir, 'meta_data/updated_data_elements.rds')))
categories = data.table(readRDS(paste0(dir, 'meta_data/data_elements_categories.rds')))

# drop unecessary variables in meta data data sets
data_elements[ , c('datasets_url', 'data_element_url'):=NULL]
categories[ , url_list:=NULL]

# change the names of vars in dt so they match with meta data
setnames(x, 'category', 'category_id')
setnames(x, 'org_unit_ID', 'org_unit_id')
setnames(x, 'data_element_ID', 'data_element_id')

# change the names of vars in meta data so they match with dt
setnames(categories, 'ID', 'category_id')
setnames(categories, 'displayName', 'category')
setnames(data_elements, 'datasets_ID', 'data_set_id')

x[ , group:=NULL]
x[ , data_element_id:=as.character(data_element_id)]
x[ , category_id:=as.character(category_id)]
x[ , org_unit_id :=as.character(org_unit_id)]
x[ , last_update:=as.character(last_update)]
data_elements[, data_set_id:=as.character(data_set_id)]
data_elements[, data_element_id:=as.character(data_element_id)]

# merge in the facilities meta data 
y = merge(x, facilities, by='org_unit_id', all.x=TRUE)

# merge in the data elements
# some data elements contain duplicate ids - set if statements for these sets
if (folder=='pnls') {
  y[ , data_set_id:='wIMw0dzITTs']
  y = merge(y, data_elements, by=c('data_set_id', 'data_element_id'), all.x=TRUE)
} else if (folder=='base') { 
  y[ , data_set_id:='pMbC0FJPkcm']
  y = merge(y, data_elements, by=c('data_set_id', 'data_element_id'), all.x=TRUE)
} else { y = merge(y, data_elements, by='data_element_id', all.x=TRUE) }

# merge in the categories
y = merge(y, categories, by='category_id', all.x=TRUE)

# change last update to be a data variable
y[ , last_update:=as.Date(last_update)]

# rename variables and place in an intuitive order 
# check if the data table contains quarterly data 
setnames(y, "data_element_id", "element_id")
setnames(y, "datasets_name", "data_set")
setnames(y, "data_element_name", "element")

y[, c("period", "data_set_id") := NULL]

dt = copy(y)
#---------------------------------

#---------------------------------
# # run the prep function to prepare some variables for use
# dt = prep_dhis(dt)

# setwd("/ihme/code/abatzel/gf/")
# source("./core/standardizeDPSNames.r")
# source("./core/standardizeHZNames.R") # can't run on the cluster with accented characters

# this used to be in the prep_dhis function, but it isn't working so I'm running it step by step:
# replace the dps/hz with just the name, excluding the code and word 'province'
# some health zones and provinces have two names before 'province'
x = copy(dt)

# replace dps with the name only
x$dps1 = unlist(lapply(strsplit(x$dps, " "), "[", 2))
x$dps2 = unlist(lapply(strsplit(x$dps, " "), "[", 3))
x[dps2!='Province', dps:=paste(dps1, dps2)]
x[dps2=='Province', dps:=dps1]
x[ , c('dps1', 'dps2'):=NULL]

# replace health zone with the name only
x$health_zone1 = unlist(lapply(strsplit(dt$health_zone, " "), "[", 2))
x$health_zone2 = unlist(lapply(strsplit(dt$health_zone, " "), "[", 3))
x$health_zone3 = unlist(lapply(strsplit(dt$health_zone, " "), "[", 4))
x[health_zone3 != 'Zone' & health_zone2 != 'Zone', health_zone:=paste(health_zone1, health_zone2, health_zone3) ]
x[health_zone3=='Zone', health_zone:=paste(health_zone1, health_zone2)]
x[health_zone2=='Zone', health_zone:=health_zone1]
x[ , c('health_zone1', 'health_zone2', 'health_zone3'):=NULL]

dt = copy(x)
#---------------------------------

#---------------------------------
# save the merged / prepped rds file 
# arguments for the save
min = dt[ , min(date)]
min = gsub('-', '_', min)
max = dt[ , max(date)]
max = gsub('-', '_', max)

# save a merged rds file 
saveRDS(dt, paste0(dir, 'pre_prep/merged/', folder,'_', min, '_', max, '.rds' ))
#---------------------------------











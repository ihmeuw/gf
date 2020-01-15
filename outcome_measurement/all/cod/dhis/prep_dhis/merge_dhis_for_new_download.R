# Merge the Base Services, SIGL, and PNLS data downloaded from DHIS2 DRC (SNIS)
# Audrey Batzel from Caitlin's code
#
# updated 01/13/20
#
# Upload the RDS data from DHIS2 and merge with the meta data 
# prep the data sets for analysis and the Tableau Dashboard
# ----------------------------------------------

# --------------------
# Set up R
# --------------------
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
# qsub -terse -N rst_ide_19_05_14_160329 -q long.q -l fthread=2 -l m_mem_free=60G -l h_rt=85:00:00 -l archive=TRUE -P proj_pce /ihme/code/jpy_rstudio/jpy_rstudio_shell.sh -i /ihme/singularity-images/rstudio/ihme_rstudio_3501.img -t rstudio -p 7513 -o 1 -G r

# change the folder to the name of the data set you want to merge
# this is the only argument to change 

set = 'base'
#---------------------------------

# --------------------------------
# set working directories
#---------------------------------
# detect if operating on windows or on the cluster 
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set the directory for input and output
dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/')
dir_download = paste0(dir, '1_initial_download/', set, '/')
#---------------------------------

#---------------------------------
# read in the most recently downloaded file
#---------------------------------
# list the files in the working directory
files = list.files(dir_download, recursive=TRUE)
files = files[grepl("aggregated", files, fixed = TRUE)]

# keep just the most recently downloaded file:
years = lapply(files, function (x) { str_split(x, '_')[[1]][5] })
years = as.numeric(years)
months = lapply(files, function (x) { str_split(x, '_')[[1]][4] })
months = as.numeric(months)

max_yr = as.character(max(years))
max_mo = as.character(max(months))

files = files[lapply(files, function (x) { str_split(x, '_')[[1]][5] }) == max_yr]
file = files[lapply(files, function (x) { str_split(x, '_')[[1]][4] }) == max_mo]

# read in the file
dt = data.table(readRDS(paste0(dir_download, file)))
#---------------------------------

#---------------------------------
# create a vector of variables to subset the larger data sets
#---------------------------------
if (set == 'base' | set == 'sigl') {
  keep_vars = read.xlsx(paste0(dir, 'meta_data/catalogues/data_elements_cod.xlsx'))
  keep_vars = data.table(keep_vars)
  keep_vars[ , keep:=as.numeric(keep)]
  keep_vars = keep_vars[keep==1, element_id]
}

# subset to only the variables needed since they are large data sets
if (set=='base' | set=='sigl') {
  dt[ , data_element_ID:=as.character(data_element_ID)]
  dt = dt[data_element_ID %in% keep_vars]
}
#---------------------------------

#---------------------------------
# drop diacritical marks
# sourcing this function on the cluster fails 
#---------------------------------
fix_diacritics = function(x) {
  replacement_chars = list('S'='S', 's'='s', 'Z'='Z', 'z'='z', '?'='A', '?'='A', '?'='A', '?'='A', '?'='A', '?'='A', '?'='A', '?'='C', '?'='E', '?'='E',
                           '?'='E', '?'='E', '?'='I', '?'='I', '?'='I', '?'='I', '?'='N', '?'='O', '?'='O', '?'='O', '?'='O', '?'='O', '?'='O', '?'='U',
                           '?'='U', '?'='U', '?'='U', '?'='Y', '?'='B', '?'='Ss', '?'='a', '?'='a', '?'='a', '?'='a', '?'='a', '?'='a', '?'='a', '?'='c',
                           '?'='e', '?'='e', '?'='e', '?'='e', '?'='i', '?'='i', '?'='i', '?'='i', '?'='o', '?'='n', '?'='o', '?'='o', '?'='o', '?'='o',
                           '?'='o', '?'='o', '?'='u', '?'='u', '?'='u', '?'='y', '?'='y', '?'='b', '?'='y')
  
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
saveRDS(dt, paste0(dir, '1_initial_download/', set, '/', set, '_', min_date, '_', max_date, '_subset.rds'))
#dt = readRDS(paste0(dir, '1_initial_download/base/base_2018_01_01_2019_04_01_full_initial_prep.rds'))
#---------------------------------

#---------------------------------
# check unique identifiers:
#---------------------------------
# byVars = names(dt)[! names(dt) %in% c('download_number', 'file', 'value')]
byVars = c('data_element_ID', 'org_unit_ID', 'category', 'date')
if( nrow(unique(dt[, byVars, with = FALSE])) != nrow(dt)) stop( 'Unique identifiers do not uniquely identify the data...')
#---------------------------------

#---------------------------------
# merge the meta data.
#---------------------------------
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

# merge in the meta data - includes english translations
x = copy(dt)
# this used to be in the merge_meta_data() function, but it isn't working so I'm running it step by step:
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
x[ , category_id:=as.character(category_id)]
x[ , org_unit_id :=as.character(org_unit_id)]
x[ , period :=as.character(period)]
x[ , last_update:=as.Date(last_update)]

data_elements[, data_set_id:=as.character(data_set_id)]
data_elements[, data_element_id:=as.character(data_element_id)]
data_elements[, data_element_name:=as.character(data_element_name)]
data_elements[, datasets_name:=as.character(datasets_name)]

categories[, category_id:=as.character(category_id)]
categories[, category:=as.character(category)]

# merge in the facilities meta data 
y = merge(x, facilities, by='org_unit_id', all.x=TRUE)

# merge in the data elements
# some data elements contain duplicate ids - set if statements for these sets
if (set=='pnls') {
  y[ , data_set_id:='wIMw0dzITTs']
  y = merge(y, data_elements, by=c('data_set_id', 'data_element_id'), all.x=TRUE)
} else if (set=='base') { 
  y[ , data_set_id:='pMbC0FJPkcm']
  y = merge(y, data_elements, by=c('data_set_id', 'data_element_id'), all.x=TRUE)
} else { y = merge(y, data_elements, by='data_element_id', all.x=TRUE) }

# merge in the categories
y = merge(y, categories, by='category_id', all.x=TRUE)

# rename variables and place in an intuitive order 
# check if the data table contains quarterly data 
setnames(y, "data_element_id", "element_id")
setnames(y, "datasets_name", "data_set")
setnames(y, "data_element_name", "element")

y[, c("period", "data_set_id") := NULL]

dt = copy(y)

rm(list = c('categories', 'facilities', 'data_elements', 'x', 'y'))
#---------------------------------

#---------------------------------
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
saveRDS(dt, paste0(dir, '2_merged_with_metadata/', folder,'_', min, '_', max, '.rds' ))
#---------------------------------











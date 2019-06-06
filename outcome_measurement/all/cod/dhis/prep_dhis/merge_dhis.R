# Merge the Base Services, SIGL, and PNLS data downloaded from DHIS2 DRC (SNIS)
# Caitlin O'Brien-Carelli
#
# 5/29/2019
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
library(openxlsx)
library(lubridate)
# --------------------
# merge on the cluster
# files take a long time to load - merge in a cluster IDE

# script to open a long-lasting large IDE
# qsub -terse -N rst_ide_19_05_14_160329 -q long.q -l fthread=20 -l m_mem_free=20G -l h_rt=70:00:00 -e archive=TRUE -P proj_pce /ihme/code/jpy_rstudio/jpy_rstudio_shell.sh -i /ihme/singularity-images/rstudio/ihme_rstudio_3501.img -t rstudio -p 1247 -o 1 -G r

# ---------------------------------
# set working directories

# detect if operating on windows or on the cluster 
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set the directory for input and output
dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/')

# source the merge and prep functions from the J Drive
source(paste0(dir, 'code/merge_functions.R'))

#---------------------------------

#---------------------------------
# change the folder to the name of the data set you want to merge
# this is the only argument to change 

folder = 'pnls'
#---------------------------------
# create a vector of variables to subset the larger data sets 

if (folder=='base' | folder=='sigl') {
  keep_vars = read.xlsx(paste0(dir, 'catalogues/data_elements_cod.xlsx'))
  keep_vars = data.table(keep_vars)
  keep_vars[ , keep:=as.numeric(keep)]
  keep_vars = keep_vars[keep==1, element_id]
}

#---------------------------------
# drop diacritical marks
# leave in script: sourcing this function on the cluster alters the function

fix_diacritics = function(x) {
  replacement_chars = list('S'='S', 's'='s', 'Z'='Z', 'z'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
                           'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
                           'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
                           'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
                           'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y')
  
  replace_me = paste(names(replacement_chars), collapse='')
  replace_with = paste(replacement_chars, collapse = '')
  return(chartr(replace_me, replace_with, x)) }

#---------------------------------
# # set the working directory and read in the files
setwd(paste0(dir, 'pre_prep/', folder, '/intermediate_data/'))

# list the files in the working directory
files = list.files('./', recursive=TRUE)

# read in the files
i = 1
for(f in files) {
 
  #load the RDs file
  file_name = f
  current_data = data.table(readRDS(f))
  current_data[ , file:=file_name]
  
  # add download number if it is not already included
  download = str_split(file_name, '_')[[1]][6]
  if (download=='first') current_data[ , download_number:=1]
  if (download=='second') current_data[ , download_number:=2]

  # subset to only the variables needed for large data sets
  if (folder=='base' | folder=='sigl') {
  current_data[ , data_element_ID:=as.character(data_element_ID)]
  current_data = current_data[data_element_ID %in% keep_vars]
  }
  
  # append to the full data
  if(i==1) dt = current_data
  if(i>1)  dt = rbind(dt, current_data)
  print(paste("Rbound", file_name, "to the full data"))
  print(i)
  i = i+1
}


# #---------------------------------
# # do some initial formatting 

# remove the factoring of value to avoid errors
# introduces some NAs as some values are NULL
dt[ , value:=as.numeric(as.character(value))]
print(paste0("There are ,", dt[is.na(value), nrow(value)], " missing values in the raw data."))
dt = dt[!is.na(value)]

# add a date variable
dt[ , date:=ymd(paste0(as.character(period), '01'))]

# create a date variable based on last update
dt[ , last_update:=as.character(last_update)]
dt[ , last_update:=sapply(str_split(last_update, 'T'), '[', 1)]

#---------------------------------
# save the interim raw data before the merge with the meta data 

# include the date range in the file name
min_date = dt[ , min(date)]
min_date = gsub('-', '_', min_date)
max_date = dt[ , max(date)]
max_date = gsub('-', '_', max_date)

# save the raw data before the merge 
saveRDS(paste0(dir, 'pre_prep/', folder, '/', folder, min_date, '_', max_date, 'full.rds'))

#---------------------------------
# collapse across the file names 

byVars = names(dt)[names(dt)!='download_number' & names(dt)!='file']
dt[ , .(value=sum(value)), by=byVars]

#---------------------------------
# merge in the meta data 
# includes english translations to be formatted later

dt = merge_meta_data(dt)
#---------------------------------
# run the prep function to prepare some variables for use

dt = prep_dhis(dt)
#--------------------------------------
# save the merged rds file 

# arguments for the save
min = dt[ , min(date)]
min = gsub('-', '_', min)
max = dt[ , max(date)]
max = gsub('-', '_', max)

# save a merged rds file 
saveRDS(dt, paste0(dir, 'pre_prep/merged/', folder,'_full_', min, '_', max, '.rds' ))

#--------------------------------------
# save a subsetted version of pnls with only relevant variables
# variables in pnls are repeated based on stratification 
# drop duplicates and save

if (folder=='pnls') {
  dt = pnls_subset(dt)
  saveRDS(dt, paste0(dir, 'pre_prep/merged/', folder,'_subset_', min, '_', max, '.rds' ))
}

#---------------------------------------


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
# setwd(paste0(dir, 'pre_prep/', folder, '/'))
# 
# # list the files in the working directory
# files = list.files('./', recursive=TRUE)
# 
# # read in the files 
# i = 1
# for(f in files) {
#   #load the RDs file
#   vec = f
#   current_data = data.table(readRDS(f))
#   current_data[ ,file:=vec]
#   
#   # subset to only the variables needed for large data sets
#   if (folder=='base' | folder=='sigl') {
#   current_data[ , data_element_ID:=as.character(data_element_ID)]
#   current_data = current_data[data_element_ID %in% keep_vars]  
#   } 
# 
#   # append to the full data 
#   if(i==1) dt = current_data
#   if(i>1)  dt = rbind(dt, current_data)
#   i = i+1
# }

#---------------------------------
# load the newly downloaded, combined data sets

if (folder=='pnls') {
  dt = readRDS(paste0(dir, '/pre_prep/pnls/pnls_01_2018_04_2019_combined_download.rds'))
  dt[ , download_number:=NULL] }

#---------------------------------
# eliminate overlapping dates
# this willwork even on a single, combined file

# check for overlapping dates
dt = dt[!is.na(period)]
dt = overlap(dt)
#---------------------------------
# remove the factoring of value to avoid errors
#introduces some NAs as some values are NULL

dt[ , value:=as.numeric(as.character(value))] 
dt = dt[!is.na(value)]
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











# Merge the Base Services, SIGL, and PNLS data downloaded from DHIS2 DRC (SNIS)
# Caitlin O'Brien-Carelli
#
# 1/25/2019
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

# sh /share/singularity-images/rstudio/shells/rstudio_qsub_script.sh -p 1247 -s 10 -P snis_merge

# ---------------------------------
# set working directories

# detect if operating on windows or on the cluster 
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set the directory for input and output
dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/')

# source the merge and prep functions from the J Drive
source(paste0(dir, 'code/merge_functions_for_new_download.R'))
source(paste0(dir, 'code/pnls_function.R'))

#---------------------------------

#---------------------------------
# change the folder to the name of the data set you want to merge
# this is the only argument to change 

folder = 'base'
#---------------------------------

#---------------------------------
# commented out in this version because this was already done in the download file:
# # create a vector of variables to subset the larger data sets 
# 
# if (folder=='base' | folder=='sigl') {
#   keep_vars = read.xlsx(paste0(dir, 'catalogues/data_elements_cod.xlsx'))
#   keep_vars = data.table(keep_vars)
#   keep_vars[ , keep:=as.numeric(keep)]
#   keep_vars = keep_vars[keep==1, element_id]
# }
#---------------------------------

#---------------------------------
# drop diacritical marks
# sourcing this function on the cluster fails 

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
# set the working directory and read in the files
setwd(paste0(dir, 'pre_prep/', folder, '/'))

# list the files in the working directory
files = list.files('./', recursive=TRUE)
file = files[grepl("combined", files)]

# read in the file
dt = data.table(readRDS(file))
#---------------------------------

#---------------------------------
# commented out in this version... going to just use the combined file for now, and for future downloads will come up
# with a new way to use this function with those
# # eliminate overlapping dates
# dt = overlap(dt)

x <- copy(dt)
# do still need this part of overlap():
if(folder=='pnlt' | folder=='tb_pati_v_registered' | folder=='tb_pati_v_result') {
  # pnlt data is quarterly - create date from qurter (start month)
  x[ , period:= as.character(period)]
  x[ , year:=substr(period, 1, 4)]
  x[ , quarter:=substr(period, 5, 6)]
  x[quarter=='Q1', month:='01']
  x[quarter=='Q2', month:='04']
  x[quarter=='Q3', month:='07']
  x[quarter=='Q4', month:='10']
  x[ , date:=as.Date(paste(year, month, '01', sep='-'), '%Y-%m-%d')]
  x[ , c('period', 'year', 'month'):=NULL]
} else {
  
  # create a date variable 
  x[ , period:= as.character(period)]
  x[ , year:=substr(period, 1, 4)]
  x[ , month:=substr(period, 5, 6)]
  x[ , date:=as.Date(paste(year, month, '01', sep='-'), '%Y-%m-%d')]
  x[ , c('period', 'year', 'month'):=NULL] }

dt <- copy(x)
#---------------------------------

#---------------------------------
# remove the factoring of value to avoid errors
dt[ , value:=as.character(value)] 
dt = dt[!is.na(period)]
#---------------------------------

#---------------------------------
# merge in the meta data 
# includes english translations
dt = merge_meta_data(dt)
#---------------------------------

#---------------------------------
# run the prep function to prepare some variables for use
dt = prep_dhis(dt)
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

#---------------------------------
# save a subsetted version of pnls with only relevant variables
# variables in pnls are repeated based on stratification 
# drop duplicates and save

if (folder=='pnls') {
  dt = pnls_subset(dt)
  saveRDS(dt, paste0(dir, 'pre_prep/merged/', folder,'_subset_', min, '_', max, '.rds' ))
}
#---------------------------------------











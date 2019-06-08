# DHIS2 Extraction for DRC - SNIS - append extracted files
# Extracts Data from: https://www.snisrdc.com/dhis-web-commons/security/login.action
# Sources dhis_extracting_functions.R on J Drive for dhisextractr package and extraction functions
# Website re-reroutes from https://www.snisdrc.com to https://snisdrc.com

#---------------------------
# Caitlin O'Brien-Carelli
# 1/15/2018

# Audrey Batzel
# 2/22/19 - updated to work around problem of some facilities not downloading still even with changed pace
#---------------------------
# Extract single data sets by specifying the data set number
# Data will be merged with the meta data to create complete data sets
# You must have the source functions and meta data accessible to run 
#---------------------------

#---------------------------
# To run on the cluster:
# Copy shell script into a qlogin session to open an IDE
# request at least 10 slots for extractions 

# sh /ihme/code/jpy_rstudio/jpy_rstudio_qsub_script.sh -i /ihme/singularity-images/rstudio/ihme_rstudio_3501.img -l m_mem_free=50G -l fthread=20 -P proj_pce -q all -t rstudio -l archive=TRUE -l h_rt=72:00:00
#---------------------------
# Set up R
rm(list=ls())
library(data.table)
library(jsonlite)
library(httr)
library(ggplot2)
library(stringr) 
library(RCurl)
library(XML)
library(plyr)
library(openxlsx)
#---------------------------
# Set the directory to download the data
# detect if operating on windows or on the cluster 
root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

#---------------------------
# define main directory

dir = paste0(root, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/')
setwd(dir)
#---------------------------

#---------------------------
# Set these to the correct values for the extraction:

# select the start year and end year for the download
start_year = '2017'
end_year = '2018'
start_month = '01'
end_month = '01' # start month is inclusive, end month is exclusive

# change set_name to the name of the data set you are downloading 
# set_name will change the file names for saving the data
set_name = 'pnls'

#------------------------
# read in all files and rbind together to save one file of data
files = list.files( paste0('./pre_prep/', set_name, '/intermediate_data/'), recursive=TRUE)

# if (set_name=='base' | set_name=='sigl') {
#   keep_vars = read.xlsx(paste0(dir, 'catalogues/data_elements_cod.xlsx'))
#   keep_vars = data.table(keep_vars)
#   keep_vars[ , keep:=as.numeric(keep)]
#   keep_vars = keep_vars[keep==1, element_id]
# }

dt = data.table()
# read in the files 
i = 1
for(f in files) {
  #load the RDs file
  vec = f
  current_data = data.table(readRDS(paste0(dir, 'pre_prep/', set_name, '/intermediate_data/', f)))
  current_data[ , file:=vec ]
  
  # # subset to only the variables needed for large data sets
  # if (folder=='base' | folder=='sigl') {
  #   current_data[ , data_element_ID:=as.character(data_element_ID)]
  #   current_data = current_data[data_element_ID %in% keep_vars]
  # }
  
  # append to the full data 
  if(i==1) dt = current_data
  if(i>1)  dt = rbind(dt, current_data)
  i = i+1 }

# save the data table in its individual folder in 'pre_prep' for merge and prep:
saveRDS(extracted_data, paste0(dir, 'pre_prep/', set_name, '/', set_name, '_', 
                               start_month, '_', start_year, '_', end_month, '_', end_year, '.rds'))

#-------------------------

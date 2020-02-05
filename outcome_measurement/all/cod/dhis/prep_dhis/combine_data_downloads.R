# Combine DHIS2 downloads for a given set so we have a full time series to run outlier detection on
# 2/3/20 
# Audrey Batzel

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
# NOTE: set variable passed in from master script
#---------------------------------

# --------------------------------
# set working directories
#---------------------------------
# detect if operating on windows or on the cluster 
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set the directory for input and output
dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/')
dir_files = paste0(dir, '2_merged_with_metadata/', set, '/')

outFile = paste0(dir, '3_prepped/', set, '/', set, '_prepped.rds')
#---------------------------------

#---------------------------------
# read in the most recently downloaded file
#---------------------------------
# list the files in the working directory
files = list.files(dir_files, recursive=TRUE)
files = files[grepl(set, files)]
files = files[!grepl('archive', files)]

dt_files = data.table(file = c(files))
dt_files[, c('set', 'start_year', 'start_month', 'start_day', 'end_year', 'end_month', 'end_day') := transpose(str_split(file, '_')) ]
dt_files[, end_day := gsub('.rds', '', end_day)]
dt_files[, start_date := as.Date(paste0(start_year, '-', start_month, '-', start_day))]
dt_files[, end_date := as.Date(paste0(end_year, '-', end_month, '-', end_day))]

# read in the most recently downloaded file to start:
inFile = dt_files[end_date == max(end_date), file]
dt = data.table(readRDS(paste0(dir_files, inFile)))

while( min(dt_files$start_date) < min(dt$date)){
  # remove the previously read in file from dt_files
  remove_row = which(dt_files[, end_date] == max(dt_files[, end_date]))
  dt_files = dt_files[-remove_row, ]
  
  # get the next-most-recent download
  inFile = dt_files[end_date == max(end_date), file]
  dt2 = data.table(readRDS(paste0(dir_files, inFile)))
  
  # truncate dt2 so it is less than the min date of dt
  dt2 = dt2[date < min(dt$date)]
  
  # add dt2 to dt
  dt = rbindlist(list(dt, dt2), use.names = TRUE, fill = TRUE)
}

# cut dt off before 2017, where it was less complete:
dt = dt[ date >= '2017-01-01']

saveRDS(dt, outFile)
#---------------------------------




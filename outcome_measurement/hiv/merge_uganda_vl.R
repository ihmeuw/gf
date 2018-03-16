# ----------------------------------------------
# David Phillips, Caitlin O'Brien-Carelli
#
# 12/21/2018
# To acombine the data sets from the Uganda Viral Load Dashboard into a single data set
# ----------------------------------------------

# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(jsonlite)
library(httr)
library(stringr) # to help extract meta data from file names

# --------------------


# ----------------------------------------------
# Files and directories

# data directory

# output file
#dir = '/home/j/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard'
# ----------------------------------------------


# ----------------------------------------------
# Load/prep the list of facility names and id#s to merge with viral load data

# store url
#url = 'https://vldash.cphluganda.org/other_data'

# load
#data = fromJSON(url)

#data.table(rbindlist(lapply(1:length(data$facilities), function(x) data$facilities[[x]])))
#facilities = data.table(rbindlist(lapply(1:length(data$facilities), function(x) data$facilities[[x]])))      

#facilitity ids are stored as character strings; convert to integers 
#facility_id <- as.numeric(facilities$id)
#is.numeric(facility_id)
#facility_names <- data.table(cbind(facilities, facility_id))

# save raw output
#saveRDS(facility_names, file="J:/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/facility_names.rds")

# ----------------------------------------------


# ----------------------------------------------
# set files and directories for the viral load data

# change directory
setwd("J:/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/webscraped_data")

# list existing files
files <- list.files('./', recursive=TRUE)

# ----------------------------------------------
# add identifying variables to the existing data tables using file names
# add year, month, age group, sex, tb status

# loop over existing files
i = 1
for(f in files[1:20]) {

  #Load the RDs file
  jsonData = readRDS(f)
  
  # pull out relevant table
  current_data = data.table(jsonData$f_numbers)
  
  # skip to next if there was no data for this combination
  if (length(current_data)==0) next

  # extract meta data from file name
  meta_data = strsplit(f, '_')[[1]]
  current_data[, year:=as.numeric(substr(meta_data[4],3,4))]
  current_data[, month:=as.numeric(substr(meta_data[3],1,2))]
  current_data[, ages:=meta_data[4]]
  current_data[, sex:=meta_data[5]]
  current_data[, tb:=gsub('tb', '', meta_data[6])]
  current_data[, tb:=gsub('.rds', '', tb)]
  
  # append to the full data 
  if(i==1) full_data = current_data
  if(i>1) full_data = rbind(full_data, current_data)
  i = i+1
}


# ----------------------------------------------
# merge on district/facility names

#load the list of facility names and id#s for the merge
#readRDS("J:/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/facility_names.rds")

#merge the two files to create a single data set
#data = merge(full_data, facility_names, by.x='facility_id', by.y='facility_id', all.x=TRUE)

# ----------------------------------------------

#save the final data as an RDS
saveRDS(full_data, file="J:/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/uganda_test.rds")

# ----------------------------------------------
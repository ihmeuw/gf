# ----------------------------------------------
# David Phillips, Caitlin O'Brien-Carelli
#
# 5/23/2018
# To acombine the data sets from the Uganda Viral Load Dashboard into a single data set
# this code is now included in prep_uvl
# ----------------------------------------------

# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(jsonlite)
library(httr)
library(stringr) # to help extract meta data from file names

# --------------------
# detect if on windows or on the cluster 

if (Sys.info()[1] == 'Windows') {
  username <- "ccarelli"
  root <- "J:/"
} else {
  username <- Sys.getenv("USER")
  root <- "/home/j/"
}

# ----------------------------------------------
# Files and directories

# data directory

# set directory
  dir <- paste0(root, 'Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard')
# ----------------------------------------------
# set files and directories for the viral load data

# change working directory
setwd(paste0(root, 'Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/webscrape_agg/sex'))
  
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
  
  # add if tb status is included 
  #current_data[, tb:=gsub('tb', '', meta_data[6])]
  #current_data[, tb:=gsub('.rds', '', tb)]
  
  # append to the full data 
  if(i==1) full_data = current_data
  if(i>1) full_data = rbind(full_data, current_data)
  i = i+1
}


# ----------------------------------------------
# merge on district/facility names


# ----------------------------------------------

#save the final data as an RDS


# ----------------------------------------------
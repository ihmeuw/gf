# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# To comine the aggregate data sets (no filters) to check the monthly values, 2015
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
# set files and directories for the viral load data

# change directory
setwd("J:/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/webscrape_agg/sex")

# list existing files
files <- list.files('./', recursive=TRUE)
files

# ----------------------------------------------
# add identifying variables to the existing data tables using file names
# add year, month, age group, sex, tb status

# loop over existing files
i = 1
for(f in files[1:6]) {
  
  #Load the RDs file
  jsonData = readRDS(f)
  
  # pull out relevant table
  current_data = data.table(jsonData$f_numbers)
  
  # skip to next if there was no data for this combination
  if (length(current_data)==0) next

  #to check the position of variables
  # m = 3, y = 4
  #outFile = paste0(dir, '/facilities_suppression_', m,'_','20', y,'_',s,'_','.rds')
  
  # extract meta data from file name
  meta_data = strsplit(f, '_')[[1]]
  current_data[, year:=as.numeric(substr(meta_data[4],3,4))]
  current_data[, month:=as.numeric(substr(meta_data[3],1,2))]
  current_data[, sex:=(meta_data[5])]

  # append to the full data 
  if(i==1) full_data = current_data
  if(i>1) full_data = rbind(full_data, current_data)
  i = i+1
}

# ----------------------------------------------

#save the final data as an RDS
saveRDS(full_data, file="J:/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/webscrape_agg/sex_age/sex_age.rds")

# view it 
str(full_data)
class(full_data)
View(full_data)
# ----------------------------------------------

# run some stats to check it 
full_data[month==1, sum(samples_received)]
full_data[month==1 & sex=='x', .(total_samples=sum(samples_received))]






full_data[ , .(total_samples=sum(samples_received)), by=month]

full_data[ , .(total_sup=sum(suppressed)), by=month]






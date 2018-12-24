# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 12/24/2018
# Rbind the UVL data sets together
# ----------------------------------------------

# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(jsonlite)
library(httr)
library(ggplot2)
library(stringr) 
library(plyr)
# --------------------

# -----------------------------------------------
# detect if operating on windows or on the cluster 

root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# --------------
# set files and directories for the uganda viral load data

# set the working directory to loop over the downloaded files
dir = paste0(root, '/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/webscrape/sex/')
setwd(dir)

# list existing files
files = list.files('./', recursive=TRUE)
length(files)

# --------------

# ----------------------------------------------
# add identifying variables to the existing data tables using file names
# add year, month, sex

# loop over existing files
i = 1
for(f in files) {
  
  #Load the RDs file
  jsonData = readRDS(f)
  
  # pull out relevant table
  current_data = data.table(jsonData$f_numbers)
  
  # grab the facility and district ids
  setnames(current_data, '_id', 'id')
  
  district_id = unlist(current_data$id[[1]])
  district_id = data.table(district_id)

  hub_id = unlist(current_data$id[[2]])
  hub_id = data.table(hub_id)
  
  facility_id = unlist(current_data$id[[3]])
  facility_id = data.table(facility_id)
  
  current_data[ ,id:=NULL]
  
  current_data = cbind(current_data, district_id)
  current_data = cbind(current_data, hub_id)
  current_data = cbind(current_data, facility_id)
  
  # skip to next if there was no data for this combination
  if (length(current_data)==0) next
  
  # to check the position of variables: 
  # positions: year = 4; month = 3; sex=5, tb_status=6
  
  # extract meta data from file name
  meta_data = strsplit(f, '_')[[1]]
  current_data[, year:=as.numeric(substr(meta_data[4],1,4))]
  current_data[, month:=as.numeric(substr(meta_data[3],1,2))]
  current_data[, sex:=(meta_data[5])]
  current_data[ , sex:=(substr(current_data$sex, 1, 1))] # to remove .rds
  
  # add if tb status is included 
  current_data[, tb:=gsub('tb', '', meta_data[6])]
  current_data[, tb:=gsub('.rds', '', tb)]

  # append to the full data 
  if(i==1) full_data = current_data
  if(i>1) full_data = rbind(full_data, current_data)
  i = i+1
}

# view the final product of full_data
str(full_data)

# ----------------------------------------------
# create a date variable
full_data[, date:=as.Date(paste(year, month, '01', sep='-'), '%Y-%m-%d')]
full_data[ , month:=NULL]

# save the product
min = full_data[ , min(year)]
max = full_data[ , max(year)]

# save the product
saveRDS(full_data, 
        paste0(root, '/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/merged/vl_',
               min, '_', max, '.rds'))
# ----------------------------------------------




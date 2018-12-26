# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 12/24/2018
# Rbind the UVL data sets together
# Run dist_facilities_uvl.R to download facility and district names
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
  # current_data[, tb:=gsub('tb', '', meta_data[6])]
  # current_data[, tb:=gsub('.rds', '', tb)]

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

# save the date range for the file name
min = full_data[ , min(year)]
max = full_data[ , max(year)]

# ---------------------------
# merge in facility and district names

# reset directory
new_dir = paste0(root, '/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/')

# load facilities
facilities = readRDS(paste0(new_dir, 'webscrape/facilities.rds'))
facilities[ ,c('district_id', 'hub_id'):=NULL]
full_data = merge(full_data, facilities, by='facility_id', all.x=TRUE)

# some districts have multiple ids in the data set
full_data[district_id==30, district:='Kabale']
full_data[district_id==134, district:='Rakai']
full_data[district_id==89, district:='Gomba']
full_data[district_id==135, district:='Manafwa']
full_data[district_id==131, district:='Nebbi']
full_data[district_id==136, district:='Pallisa']

# add districts that failed to merge
districts = full_data[!is.na(district),.(district_alt=unique(district)), by=district_id]
districts = districts[!duplicated(district_alt)]

replace = merge(full_data[is.na(district)], districts, by='district_id', all.x=T)
replace[ , district:=district_alt]
replace[ , district_alt:=NULL]

# merge in the replacements
# automatically drops out facility left blanks
full_data = full_data[!is.na(district)]
full_data = rbind(full_data, replace)

# ---------------------------
# save the product

saveRDS(full_data, paste0(new_dir, 'merged/vl_', min, '_', max, '.rds'))

# ----------------------------------------------




# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 4/10/2019
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

j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# ---------------------------
# set files and directories for the uganda viral load data

# set the working directory to loop over the downloaded files
dir = ('/ihme/scratch/users/ccarelli/webscrape_uvl')
setwd(dir)

# set output directory
# set the output directory
metaDir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/meta_data/')
outDir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/prepped/')

# list existing files
files = list.files('./', recursive=TRUE)
length(files)

# source code
source('/ihme/code/ccarelli/gf/outcome_measurement/hiv/uga/viral_load/extract_prep_uvl/prep_uvl.R')
# ---------------------------

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
  
  # extract meta data from file name
  meta_data = strsplit(f, '_')[[1]]
  current_data[, month:=as.numeric(substr(meta_data[3],1,2))]
  current_data[, year:=as.numeric(substr(meta_data[4],1,4))]
  current_data[, sex:=(meta_data[5])]
  current_data[, age:=(meta_data[6])]
  
  # rename sex values
  current_data[sex=='m', sex:='Male']
  current_data[sex=='f', sex:='Female']
  current_data[sex=='x', sex:=NA]
  
  # create a clear age category
  age_start = str_split(unique(current_data$age), ',')[[1]][1]
  age_end = str_split(unique(current_data$age), ',')[[1]][5]
  current_data[ , age:=paste(age_start, '-', age_end)]
  
  # file name to check
  # current_data[ ,file_name:=f]
  
  # append to the full data 
  if(i==1) dt = current_data
  if(i>1) dt = rbind(dt, current_data)
  i = i+1
}

# view the final product of dt
str(dt)

# ----------------------------------------------
# create a date variable
dt[, date:=as.Date(paste(year, month, '01', sep='-'), '%Y-%m-%d')]
dt[ , c('month', 'year'):=NULL]

# ---------------------------
# merge in facility, hub, and district names

# facility names
facilities = readRDS(paste0(metaDir, 'facilities.rds'))
dt = merge(dt, facilities, by='facility_id', all.x=T)

# district names
districts = readRDS(paste0(metaDir, 'districts.rds'))
dt = merge(dt, districts, by='district_id', all.x=T)

# hub names
hubs = readRDS(paste0(metaDir, 'hubs.rds'))
dt = merge(dt, hubs, by='hub_id', all.x=T)
# ---------------------------
# fix failures to merge 

# facility left blank
dt[facility=='Facility Left Blank', facility:=NA]

# missing districts
dt[district_id==121, district:='Hoima']

# use the hub id from the meta data to identify missed merges
# second pass fixes a huge number of merge issues
dt[is.na(hub), hub_id:=meta_hub_id]
dt[ , hub:=NULL]
dt = merge(dt, hubs, by='hub_id', all.x=T)

# delete excess variables
dt[ ,c('meta_hub_id', 'meta_district_id'):=NULL]

# ---------------------------
# run final prep
# includes using mean imputation to replace missing sex

dt = prep_uvl(dt)
# ---------------------------

# ----------------------------------------------
# save the date range for the file name
min_date = dt[ , min(year(date))]
max_date = dt[ , max(year(date))]

# save merged data 
saveRDS(dt, paste0(outDir, 'uvl_prepped_', min_date, '_', max_date, '_.rds'))

# create a message saying the data was saved 
print(paste0("Saved file: ", outDir, 'uvl_prepped_', min_date, '_', max_date, '_.rds'))
# ----------------------------------------------


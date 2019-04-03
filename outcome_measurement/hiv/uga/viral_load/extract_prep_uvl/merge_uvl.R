# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 4/3/2019
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

# --------------
# set files and directories for the uganda viral load data

# set the working directory to loop over the downloaded files
dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/webscrape/age_sex_tb/')
setwd(dir)

# set output directory
out_dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/prepped/')

# list existing files
files = list.files('./', recursive=TRUE)
length(files)

# ---------------------------
# source the function 

source("C:/Users/ccarelli/local/gf/outcome_measurement/hiv/uga/viral_load/extract_prep_uvl/prep_uvl.R")

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
  current_data[, tb:=(meta_data[6])]
  current_data[, age:=(meta_data[7])]
  
  # create an age category
  age_start = trimws(str_split(unique(current_data$age), ',')[[1]][1])
  
  x = length(str_split(unique(current_data$age), ',')[[1]])
  age_end = trimws(str_split(unique(current_data$age), ',')[[1]][x])
  
  # replace the long character string with an age category
  age_replace = paste(age_start, '-', age_end)
  current_data[ , age:=age_replace]
  
  # rename sex values
  current_data[sex=='m', sex:='Male']
  current_data[sex=='f', sex:='Female']
  current_data[sex=='x', sex:=NA]
  
  # rename TB values as a logical 
  current_data[tb=='y', tb_status:=TRUE]
  current_data[tb=='n', tb_status:=FALSE]
  current_data[tb=='x', tb_status:=NA]
  current_data[ , tb:=NULL]

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

# save the date range for the file name
min_date = dt[ , min(year(date))]
max_date = dt[ , max(year(date))]

# ---------------------------
# merge in facility and district names

# reset directory
new_dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/meta_data')

# load facilities
facilities = readRDS(paste0(new_dir, '/facilities.rds'))

# merge in the facilities 
# use district and hub ids in the data, not metadata
facilities[ ,c('district_id', 'hub_id'):=NULL]
dt = merge(dt, facilities, by='facility_id', all.x=TRUE)

# some districts have multiple ids in the data set
dt[district_id==30, district:='Kabale']
dt[district_id==134, district:='Rakai']
dt[district_id==89, district:='Gomba']
dt[district_id==135, district:='Manafwa']
dt[district_id==131, district:='Nebbi']
dt[district_id==136, district:='Pallisa']

# add districts that failed to merge
districts = dt[!is.na(district),.(district_alt=unique(district)), by=district_id]
districts = districts[!duplicated(district_alt)]

replace_districts = merge(dt[is.na(district)], districts, by='district_id', all.x=T)
replace_districts[ , district:=district_alt]
replace_districts[ , district_alt:=NULL]

# merge in the replacements
# automatically drops out facility left blanks
dt = dt[!is.na(district)]
dt = rbind(dt, replace_districts)

# ---------------------------
# save the interim product (merged but not fully prepped)

# saveRDS(dt, paste0(out_dir, 'merged_vl_', min_date, '_', max_date, '.rds'))
# ---------------------------
# run final prep
# includes using mean imputation to replace missing sex

dt = prep_uvl(dt)
# ---------------------------
# save the final product 

saveRDS(dt, paste0(out_dir, 'uvl_prepped_', min_date, '_', max_date, '.rds'))
# ---------------------------






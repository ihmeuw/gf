# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# Combine the downloaded Uganda VL data w filters month, year, sex
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

# --------------------

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
  
  # skip to next if there was no data for this combination
  if (length(current_data)==0) next

  #to check the position of variables: 
  #outFile = paste0(dir, '/facilities_suppression_', m,'_','20', y,'_',s,'_',t,'_','.rds')
  # positions: year = 4; month = 3; sex=5, tb_status=6
  
  # extract meta data from file name
  meta_data = strsplit(f, '_')[[1]]
  current_data[, year:=as.numeric(substr(meta_data[4],1,4))]
  current_data[, month:=as.numeric(substr(meta_data[3],1,2))]
  current_data[, sex:=(meta_data[5])]

  # append to the full data 
  if(i==1) full_data = current_data
  if(i>1) full_data = rbind(full_data, current_data)
  i = i+1
}

# ----------------------------------------------

#save the final data as an RDS
saveRDS(full_data, file="J:/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/sex_data.rds")

# view it 
str(full_data)
class(full_data)

View(full_data)
# ----------------------------------------------

#  stats to check if the sex, tb data disaggregated data downloaded correctly 

# run some stats to check that the data downloaded correctly
full_data[, sum(samples_received), by=year]
full_data[month==1, sum(samples_received), by=year] # should be no samples in jan 2014

# check that no data downloaded for jan - july 2014 or after march 2018
# both should be 0
full_data[year==2014 & month!=8 & month!=9 & month!=10 & month!=11 & month!=12, sum(samples_received)]
full_data[year==2018 & month!=1 & month!=2 & month!=3, sum(samples_received)]

full_data[year==2014, sum(samples_received), by=.(month, sex)]
full_data[year==2014, sum(samples_received), by=month]
full_data[year==2018, sum(samples_received), by=.(month, sex)]

# check for substantial variability in the number of samples
full_data[, .(total_samples=sum(samples_received)), by=.(sex, year)]

# check for missing data in the unknown sex category
full_data[sex=='x', .(total_sup=sum(samples_received)), by=.(month, year)]
full_data[sex=='x', .(total_sup=sum(suppressed)), by=.(month, year)]

# ----------------------------------------------


# set directory
dir = "J:/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard"


# ---------------------
# upload both the webscrape data and the facility names as data tables

# upload the facilities names
facilities <- readRDS(paste0(dir, "/facilities.rds"))

# upload the uganda_vl data with month, year, sex filters
uganda_vl <- readRDS(paste0(dir, "/sex_data.rds"))


# ----------------------------------------------

# create an id variable in uganda_vl to merge on using facility_id
names(uganda_vl)
names(facilities)

uganda_vl[ , id:=facility_id]


# list unique facility ids
uganda_vl [,id, by=id] # 2042 values
facilities[, id, by=id] # 2012 values

# identify mismatches
uganda_vl$id[!uganda_vl$id %in% facilities$id]
facilities$id[!facilities$id %in% uganda_vl$id]

# format id as numeric
facilities[, id:=as.numeric(id)]

# add table 1 to uganda_vl 
# these values will repeat to match the number of values in the data table
uganda_vl <- merge(uganda_vl, facilities[,c('id','name')], by='id', all.x=TRUE)

# handle missing names
uganda_vl[is.na(name), name:=paste0('Facility #',id)]


# upload the district names
districts <- readRDS(paste0(dir, "/districts.rds"))

uganda_vl <- merge(uganda_vl, districts, by='district_id', all.x=TRUE)

# ----------------------------------------------

saveRDS(uganda_vl, paste0(dir,"/sex_data.rds"))



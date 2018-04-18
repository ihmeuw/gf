# ----------------------------------------------
# David, Phillips, Caitlin O'Brien-Carelli
#
# 4/11/2018
# To extract a list of districts and facilities from Uganda Viral Load Dashboard: https://vldash.cphluganda.org/
# ----------------------------------------------


# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(jsonlite)
library(httr)
library(tibble)
library(dplyr)
library(tm)

# --------------------


# ----------------------------------------------
# Files and directories

# data directory

# output file
dir = "J:/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/facilities"
# ----------------------------------------------


# ----------------------------------------------
# Load/prep data
# facilities and districts are stored in separate urls
            
  # store url
  url = 'https://vldash.cphluganda.org/other_data'
              
  # load
  data = fromJSON(url)
  
  
  # original function to extract the facility ids from the list
  #facilities_full <- data.table(rbindlist(lapply(1:length(data$facilities), function(x) data$facilities[[x]]))) 
 
 # create a data frame with only the facilities data 
 facilities_1 <- data$facilities
 
 # create a function that selects the facility names and ids
  select_names <- function(i) {
   y <- unlist(facilities_1[i])
   y <- y[c(2,3,4,5,7)]
   names(y) <- c("facility_id", "facility_name", "dhis2_name", "hub_id", "district_id")
   return(y)
  }
  
  # use lapply to run the select_names function and bind into a data table
  facilities <- lapply(1:length(data$facilities), function(x) select_names(x))
  facilities <- do.call('rbind', facilities)
  facilities <- data.table(facilities)
  
  #destring id #s
  facilities[,facility_id:=as.numeric(facility_id)]
  facilities[,hub_id:=as.numeric(hub_id)]
  facilities[,district_id:=as.numeric(district_id)]
  
  # 147 facilities are missing a district id
  facilities[is.na(district_id), length(unique(facility_id))]
  
  
# --------------------

# create a list of districts
districts_1 <- data$districts
  
  
  select_dist <- function(i) {
    y <- unlist(districts_1[i])
    y <- y[c(2:3)]
    names(y) <- c("district_id", "district_name")
    return(y)
  }
  
  # use lapply to run the select_names function and bind into a data table
  districts <- lapply(1:length(data$districts), function(x) select_dist(x))
  districts <- do.call('rbind', districts)
  districts <- data.table(districts)
  
  districts[, district_id:=as.numeric(district_id)]
  
  # drop 'District' from the district names
  districts[, district_name:=gsub('District','', district_name)]
  districts[, district_name:=gsub(' ','', district_name)]
  districts[, district_name]
  
  districts <- districts[order(district_id)]
  
  # eliminate duplicate district ids (error ids for 3 distrits)
  # no facilities listed under error ids
  districts <- districts[!(district_id==127| district_id==128 | district_id==129)]
  districts[duplicated(district_name), .(district_name, district_id)]
  
  #one district is named 'district left blank'... find a way to rename
  districts[district_name=="LeftBlank", district_id]
  
  
# ----------------------------------------------
  # save both files to merge in separately
  
  facilities <- facilities[ , .(facility_id=facility_id, facility_name=facility_name, dhis2name=dhis2_name)]

  # save full facilities and data to merge into downloads from Uganda VL
  saveRDS(facilities, file=paste0(dir,"/facilities.rds"))
  

  # save full facilities and data to merge into downloads from Uganda VL
  saveRDS(districts, file=paste0(dir,"/districts.rds"))
  
   
# ----------------------------------------------
  
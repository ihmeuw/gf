# ----------------------------------------------
# David, Phillips, Caitlin O'Brien-Carelli
#
# 3/26/2018
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
  
# --------------------
# add districts

  
  
  
  
  
  
  
  
  
  # save full facilities data in case needed later
  saveRDS(facilities, file=paste0(dir,"facility_merge/facilities_full.rds"))
  
  # create a data table of facilities for the merge
  facilities <- facilities_full[, .(facility_id=as.numeric(id), facility_name=name, 
                                    hub_id=as.numeric(hub_id), district_id=as.numeric(district_id))]
  

  # upload district data to merge with facilities
  districts <- readRDS(paste0(dir,"facility_merge/districts.rds"))
  
  # merge district and facility names and ids
  facilities <- merge(facilities, districts[,c('district_id','district_name')], by='district_id', all.x=TRUE)
  
    
  # save raw output
  saveRDS(facilities, file=paste0(dir,"facilities.rds"))
   

# ----------------------------------------------
  
  
  
  
  
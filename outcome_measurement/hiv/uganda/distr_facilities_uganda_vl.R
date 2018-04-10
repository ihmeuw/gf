# ----------------------------------------------
# David Phillips, Caitlin O'Brien-Carelli
#
# 3/26/2018
# To extract a list of districts and facilities from Uganda Viral Load Dashboard: https://vldash.cphluganda.org/
# Merge with Uganda VL data on facility ID in order to get names of districts, facilities
# ----------------------------------------------


# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(jsonlite)
library(httr)

# --------------------


# ----------------------------------------------
# Files and directories

# data directory

# output file
dir = "J:/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/"
# ----------------------------------------------


# ----------------------------------------------
# Load/prep data
# facilities and districts are stored in separate urls
            
  # store url
  url = 'https://vldash.cphluganda.org/other_data'
              
  # load
  data = fromJSON(url)
  
  #data.table(rbindlist(lapply(1:length(data$facilities), function(x) data$facilities[[x]])))
  facilities_full = data.table(rbindlist(lapply(1:length(data$facilities), function(x) data$facilities[[x]])))      
        
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
  
  
  
  
  
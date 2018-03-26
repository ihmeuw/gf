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
dir = '/home/j/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard'
# ----------------------------------------------


# ----------------------------------------------
# Load/prep data

            
  # store url
  url = 'https://vldash.cphluganda.org/other_data'
              
  # load
  data = fromJSON(url)
  
  #data.table(rbindlist(lapply(1:length(data$facilities), function(x) data$facilities[[x]])))
  facilities_full = data.table(rbindlist(lapply(1:length(data$facilities), function(x) data$facilities[[x]])))      
          
  facilities <- facilities_full[, .(facility_id=as.numeric(id), facility_name=name, hub_id, district_id) ]
  
    
  # save raw output
  saveRDS(facilities, file="J:/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/facility_merge/facilities.rds")
   

# ----------------------------------------------
  
  
  
  
  
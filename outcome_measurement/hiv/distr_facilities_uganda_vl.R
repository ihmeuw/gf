# ----------------------------------------------
# David Phillips, Caitlin O'Brien-Carelli
#
# 12/16/2018
# To extract a list of districts and facilities from Uganda Viral Load Dashboard: https://vldash.cphluganda.org/
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
  facilities = data.table(rbindlist(lapply(1:length(data$facilities), function(x) data$facilities[[x]])))      

            
  # save raw output
  saveRDS(facilities, file="J:/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/dist_facilities.rds")
   

# ----------------------------------------------
  
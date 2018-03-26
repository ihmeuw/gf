# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 3/26/2018
# Test downloads from the Uganda VL Dashboard w/o filters to see where the connection breaks
# Use uganda_vl_prep to download all filters
# this version download with sex and tb status filters only
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

# whether or not to re-download everything (or just new data)
reload_everything = FALSE

# data directory

# output file
# when running on home computer
#dir = 'J:/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/webscrape_agg/sex_tb'

# when running on the cluster
dir = '/home/j/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/webscrape_agg/sex_tb'

# ----------------------------------------------

# ----------------------------------------------
# Load/prep data

# loop for month and year filters ONLY

#loop over years - can be altered to run years separately
for(y in c('14', '15', '16', '17', '18')) {
  
  # loop over months
  for(m in c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')) {
      
      # do not loop over age groups
    
      # loop over tb groups - includes "unknown" option
      for(t in c('y','n','x')) { 
        
        # loop over sexes 
       for(s in c('m', 'f', 'x')) {
   
          # check if file exists first
          outFile = paste0(dir, '/facilities_suppression_', m,'_','20', y,'_', s,'_',t, '_', '.rds')
          check = file.exists(outFile)
      
          # only download if it doesn't already exist
          if (check==FALSE | reload_everything==TRUE) {
            
            # store url
            url = paste0('https://vldash.cphluganda.org/live?age_ids=%5B%5D&districts=%5B%5D&emtct=%5B%5D&fro_date=20', y, m,'&genders=%5B%22',s,'%22%5D&hubs=%5B%5D&indications=%5B%5D&lines=%5B%5D&regimens=%5B%5D&tb_status=%5B%22',t,'%22%5D&to_date=20',y,m)
            
            # load
            data = fromJSON(url)
            
            # save raw output
            saveRDS(data, file=outFile)
          }
        }
    
      }}}
      
# ----------------------------------------------


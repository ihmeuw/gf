# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 1/7/2019
# Loop to download data from the Uganda Early Infant Diagnosis Dashboard
# ----------------------------------------------

# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(jsonlite)
library(httr)
library(plyr)

# --------------------
# detect if on windows or on the cluster 
root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# ----------------------------------------------
# Files and directories

# whether or not to re-download everything (or just new data)
reload_everything = TRUE

# output directory
dir = paste0(root, '/Project/Evaluation/GF/outcome_measurement/uga/eid/sex_data')

# ----------------------------------------------

# ----------------------------------------------
# Load/prep data

# loop over years - can be altered to run years separately
y = c('14', '15', '16', '17', '18')
m = c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')
s = c('m', 'f', 'UNKNOWN')


# input arguments - if not including tb, drop t
arguments = expand.grid(y=y, m=m, s=s)


build_url = function(page_specs) {
  
  print(page_specs)
  
  y = page_specs$y
  m = page_specs$m
  s = page_specs$s
  
  # sex out file
  # outFile = paste0(dir, '/facilities_suppression_', m,'_','20', y,'_', s,'_.rds')
  
  # tb status file
  outFile = paste0(dir, '/eid_', m,'_','20', y,'_', s,'_.rds')
  
  check = file.exists(outFile)
  
  # only download if it doesn't already exist
  if (check==FALSE | reload_everything==TRUE) {
    
    url = paste0('https://edash.cphluganda.org/live?age_ids=%5B%5D&care_levels=%5B%5D&districts=%5B%5D&fro_date=20', y, m, '&genders=%5B%22',s,'%22%5D&hubs=%5B%5D&pcrs=%5B%5D&regions=%5B%5D&to_date=20', y, m)
    
  
    # to determine where errors occur
    
    # load
    existence = tryCatch(fromJSON(url), error = function(x){return("Does not exist")})
    
    # save raw output
    if (existence != 'Does not exist'){
      saveRDS(existence, file=outFile)
      existence = "Successfully saved"
    } 
    
    results = data.table("page_specs" = paste(y, m, s), 
                         "url" = url,
                         "existence" = existence)
    return(results)
  }
}

# run the function
urls = adply(arguments, 1, build_url)

#--------------------------------------------------
# download the meta data on districts and facilities

# downloads meta data and saves automatically
# src_dir = paste0(root, '/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/dist_facilities_uvl.R')

# download the facilities data 
# source(src_dir)
#--------------------------------------------------

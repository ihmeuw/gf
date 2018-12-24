# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 12/21/2018
# Loop to download data from the Uganda Viral Load Dashboard
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
dir = paste0(root, '/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/webscrape/sex_tb')

# ----------------------------------------------

# ----------------------------------------------
# Load/prep data

# loop over years - can be altered to run years separately
y <- c('14', '15', '16', '17', '18')
m <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')
t <- c('y','n','x')
s <- c('m', 'f', 'x')

# input arguments - if not including tb, drop t
arguments = expand.grid(y=y, m=m, t=t, s=s)


build_url = function(page_specs) {
  
  print(page_specs)
  
  y = page_specs$y
  m = page_specs$m
  t = page_specs$t
  s = page_specs$s
  
  # sex out file
  # outFile = paste0(dir, '/facilities_suppression_', m,'_','20', y,'_', s,'_.rds')
  
  # tb status file
   outFile = paste0(dir, '/facilities_suppression_', m,'_','20', y,'_', s,'_',t, '_.rds')
  
  check = file.exists(outFile)
  
  # only download if it doesn't already exist
  if (check==FALSE | reload_everything==TRUE) {
    
    # sex filter url
    # url = paste0('https://vldash.cphluganda.org/live?age_ids=%5B%5D&districts=%5B%5D&emtct=%5B%5D&fro_date=20', y, m,'&genders=%5B%22',s,'%22%5D&hubs=%5B%5D&indications=%5B%5D&lines=%5B%5D&regimens=%5B%5D&tb_status=%5B%5D&to_date=20', y, m)
    
    # tb status url
    url = paste0('https://vldash.cphluganda.org/live?age_ids=%5B%5D&districts=%5B%5D&emtct=%5B%5D&fro_date=20', y, m,'&genders=%5B%22',s,'%22%5D&hubs=%5B%5D&indications=%5B%5D&lines=%5B%5D&regimens=%5B%5D&tb_status=%5B%22', t, '%22%5D&to_date=20',y,m)
    
    # to determine where errors occur
  
    # load
    existence = tryCatch(fromJSON(url), error = function(x){return("Does not exist")})
    
    # save raw output
    if (existence != 'Does not exist'){
      saveRDS(existence, file=outFile)
      existence = "Successfully saved"
    } 

    results = data.table("page_specs" = paste(y, m, s), # add t if using tb
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
source("C:/Users/ccarelli/local/gf/outcome_measurement/hiv/uga/viral_load/dist_facilities_uvl.R")

#--------------------------------------------------


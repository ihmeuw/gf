# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 3/31/2018
# Loop to download data from the Uganda Viral Load Dashboard
# Run this file, then prep_uvl
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
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# ----------------------------------------------
# Files and directories

# whether or not to re-download everything (or just new data)
reload_everything = FALSE

# output directory
dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard')
scratchDir = paste0('/ihme/scratch/users/ccarelli/')
out_dir = paste0(scratchDir, 'webscrape_uvl')

# ----------------------------------------------
# source function for facilities extraction 

# if you need to update the facilities extraction code on the cluster:
# cd /ihme/code/ccarelli/gf/
# git pull

# downloads meta data and saves automatically
src_dir = '/ihme/code/ccarelli/gf/outcome_measurement/hiv/uga/viral_load/extract_prep_uvl/dist_facilities_uvl.R'

# ----------------------------------------------
# Load/prep data

# loop over years - can be altered to run years separately
# do 2015 tomorrow
y = c('15', '16')
m = c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')
s = c('m', 'f', 'x')
a = c('0,1,2,3,4', '5,6,7,8,9', '10,11,12,13,14', 
      '15,16,17,18,19', '20,21,22,23,24', '25,26,27,28,29', 
      '30,31,32,33,34', '35,36,37,38,39', '40,41,42,43,44', 
      '45,46,47,48,49')

# # for all age groups
# a = c('0,1,2,3,4', '5,6,7,8,9', '10,11,12,13,14', 
#       '15,16,17,18,19', '20,21,22,23,24', '25,26,27,28,29', 
#       '30,31,32,33,34', '35,36,37,38,39', '40,41,42,43,44', 
#       '45,46,47,48,49', '50,51,52,53,54', '55,56,57,58,59', 
#       '60,61,62,63,64', '65,66,67,68,69', '70,71,72,73,74', 
#       '75,76,77,78,79', '80,81,82,83,84', '85,86,87,88,89', 
#       '90,91,92,93,94', '95,96,97,98,99')


# calculate the number of downloads
length(y)*length(m)*length(s)*length(a)

#--------------------
# to test the loop
# y = '16'
# m = '01'
# a = '0,1,2,3,4'
# s = c('f', 'm')
#--------------------

#--------------------
# input arguments - if not including tb, drop t
arguments = expand.grid(y=y, m=m, s=s, a=a)

build_url = function(page_specs) {
  
  print(page_specs)
  
  y = page_specs$y
  m = page_specs$m
  s = page_specs$s
  a = page_specs$a 
  
  
  # tb status file
  outFile = paste0(out_dir, '/facilities_suppression_', m,'_','20', y,'_', s,'_', a,'_.rds')
  check = file.exists(outFile)
  
  # only download if it doesn't already exist
  if (check==FALSE | reload_everything==TRUE) {
    
    url = paste0('https://vldash.cphluganda.org/live/?age_ids=%5B',
                 a, '%5D&districts=%5B%5D&emtct=%5B%5D&fro_date=20', 
                 y, m,'&genders=%5B%22',s,'%22%5D&hubs=%5B%5D&indications=%5B%5D&lines=%5B%5D&regimens=%5B%5D&tb_status=%5B%5D&to_date=20',y,m)
    
    
    # to determine where errors occur
    # load
    existence = tryCatch(fromJSON(url), error = function(x) {return("Does not exist")})
    
    # save raw output
    if (existence != 'Does not exist'){
      saveRDS(existence, file=outFile)
      existence = "Successfully saved"
    } 

    results = data.table("page_specs" = paste(y, m, s, a), 
                          "url" = url,
                         "existence" = existence)
    return(results)
  }
}

# run the function
urls = adply(arguments, 1, build_url)

#--------------------

#--------------------------------------------------
# download the meta data on districts and facilities

# download the facilities data 
source(src_dir)
#--------------------------------------------------


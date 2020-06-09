# ----------------------------------------------
# AUTHOR: Emily Linebarger
# PURPOSE: Master file for extracting PUDR performance tracking measures. 
# DATE: Last updated June 2019. 
# INSTRUCTIONS: Working directory should be the root of your personal repository. 
# ----------------------------------------------

#-----------------------------------------
# TO-DO: 


#-----------------------------------------

# ---------------------------------------
# Install packages and set up R  
# ---------------------------------------

# clear memory
rm(list=ls())

# run setup code (load file paths and packages)
user=as.character(Sys.info()[7])
if (Sys.info()[1]=='Windows'){
  repo_root = paste0("C:/Users/", user, "/Documents/gf/") #Change to the root of your repository
  setwd(repo_root)
} else {
  setwd(paste0("/ihme/homes/",user,"/gf/"))
}

source('./outcome_measurement/all/performance_indicators/prep/set_up_r.r', encoding="UTF-8")
source('./resource_tracking/prep/_common/shared_functions.r', encoding="UTF-8")
source('./resource_tracking/prep/_common/load_master_list.r', encoding="UTF-8")

# ---------------------------------------
# Set boolean switches
# ---------------------------------------
# What countries do you want to run? 
countries = c('cod') #Add country codes to this list to prep them. Possible choices are 'cod', 'gtm', 'sen', and 'uga'. 

prep_1a = TRUE #Set to true if you want to extract impact outcome indicators 1A. 
prep_1a_disagg = TRUE #Set to true if you want to extract disaggregated impact outcome indicators 1A.
prep_1b = TRUE #Set to true if you want to extract coverage indicators 1B. 
prep_1b_disagg = TRUE #Set to true if you want to extract disaggregated coverage indicators 1B. 

verbose = FALSE #Set to true if you want to print more detailed error messages. 

#-----------------------------------------------
# 1. Prep individual countries 
#-----------------------------------------------
source("./outcome_measurement/all/performance_indicators/prep/2_read_filelist.R", encoding="UTF-8")

#-----------------------------------------------
# 2. Aggregate and clean data 
#-----------------------------------------------
source("./outcome_measurement/all/performance_indicators/prep/3_aggregate_data.r", encoding="UTF-8")

#-----------------------------------------------
# 3. Clean and validate data 
#-----------------------------------------------
source("./outcome_measurement/all/performance_indicators/prep/4_clean_merge_data.R", encoding="UTF-8")

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
repo_root = "C:/Users/elineb/Documents/gf/" #Change to your repository
setwd(repo_root)
source('./process_evaluation/prep/set_up_r.r')
source('./resource_tracking/prep/_common/shared_functions.r', encoding="UTF-8")
source('./resource_tracking/prep/_common/load_master_list.r', encoding="UTF-8")

# ---------------------------------------
# Set boolean switches
# ---------------------------------------
# What countries do you want to run? 
countries = c('cod', 'gtm', 'sen', 'uga') #Add country codes to this list to prep them. Possible choices are 'cod', 'gtm', 'sen', and 'uga'. 

prep_1a = FALSE #Set to true if you want to extract impact outcome indicators 1A. 
prep_1a_disagg = TRUE #Set to true if you want to extract disaggregated impact outcome indicators 1A.
prep_1b = TRUE #Set to true if you want to extract coverage indicators 1B. 
prep_1b_disagg = TRUE #Set to true if you want to extract disaggregated coverage indicators 1B. 

verbose = FALSE #Set to true if you want to print more detailed error messages. 

#-----------------------------------------------
# 1. Prep individual countries 
#-----------------------------------------------
source("./process_evaluation/prep/2_read_filelist.R", encoding="UTF-8")

#-----------------------------------------------
# 2. Aggregate and clean data 
#-----------------------------------------------
source("./process_evaluation/prep/3_aggregate_data.r", encoding="UTF-8")

#-----------------------------------------------
# 3. Clean and validate data 
#-----------------------------------------------
# source("./process_evaluation/prep/4_clean_data.r", encoding="UTF-8")

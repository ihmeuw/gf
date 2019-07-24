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
source('./process_evaluation/set_up_r.r')
source('./resource_tracking/prep/_common/shared_functions.r', encoding="UTF-8")

# ---------------------------------------
# Set boolean switches
# ---------------------------------------
# What countries do you want to run? 
countries = c('cod', 'gtm', 'uga', 'sen') #Add country codes to this list to prep them. Possible choices are 'cod', 'gtm', 'sen', and 'uga'. 
prep_1b = TRUE #Set to true if you want to extract coverage indicators 1B. 
verbose = FALSE #Set to true if you want to print more detailed error messages. 


#-----------------------------------------------
# 1. Prep individual countries 
#-----------------------------------------------
source("./process_evaluation/2_read_filelist.R", encoding="UTF-8")

#-----------------------------------------------
# 2. Aggregate data 
#-----------------------------------------------

#-----------------------------------------------
# 3. Validate data
#-----------------------------------------------


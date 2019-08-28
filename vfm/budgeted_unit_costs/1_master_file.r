# --------------------------------------------------------------------------
# AUTHOR: Emily Linebarger
# PURPOSE: Master file for extracting unit costs from budgets. 
# DATE: Last updated August 2019. 
# INSTRUCTIONS: Working directory should be the root of your personal repository. 
# --------------------------------------------------------------------------

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
source('./vfm/budgeted_unit_costs/set_up_r.r')
source('./resource_tracking/prep/_common/shared_functions.r', encoding="UTF-8")

# ---------------------------------------
# Set boolean switches
# ---------------------------------------
# What countries do you want to run? 
countries = c('cod', 'gtm', 'sen', 'uga') #Add country codes to this list to prep them. Possible choices are 'cod', 'gtm', 'sen', and 'uga'. 

verbose = FALSE #Set to true if you want to print more detailed error messages. 

#-----------------------------------------------
# 1. Prep individual countries 
#-----------------------------------------------
source("./vfm/budgeted_unit_costs/2_read_filelist.R", encoding="UTF-8")

#-----------------------------------------------
# 2. Aggregate and clean data 
#-----------------------------------------------
source("./vfm/budgeted_unit_costs/3_aggregate_data.r", encoding="UTF-8")

#-----------------------------------------------
# 3. Validate data
#-----------------------------------------------


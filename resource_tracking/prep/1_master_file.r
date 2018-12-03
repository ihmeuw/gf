# ----------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Master file for updating resource tracking database. 
# DATE: Last updated November 2018. 

# ---------------------------------------
## Install packages and set up R  ###### 
# ---------------------------------------

rm(list=ls())
library(lubridate)
library(data.table)
library(readxl)
library(stats)
library(stringr)
library(tools)
library(rlang)
library(zoo)
library(tidyr)

# ---------------------------------------
## Set global variables.   ###### 
# ---------------------------------------

user = "elineb" #Change to your username 
code_dir = paste0("C:/Users/", user, "/Documents/gf/resource_tracking/prep/")
combined_output_dir = "J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping"
countries <- c("cod", "gtm", "uga") #Remove countries from this list that you don't want to update. 
source(paste0(code_dir, "2_shared_mapping_functions.R")) #Emily for some reason this isn't sourcing correctly. Check out what's going on. 

# ----------------------------------------------
## STEP 1: Verify module mapping framework 
# ----------------------------------------------

  source(paste0(code_dir, "3_verify_module_mapping.r")) #Emily would eventually like to have this running continuously to make sure we don't create new errors in the database. 

for(i in 1:length(countries)){
# ----------------------------------------------
## STEP 2: Verify country-level file list 
# ----------------------------------------------
  master_file_dir = paste0("J:/Project/Evaluation/GF/resource_tracking/", country, "/grants/")
  export_dir = paste0("J:/Project/Evaluation/GF/resource_tracking/", country, "/prepped/")
  
  # colnames_desired = sort(c("file_name", "sheet", "start_date", "disease", "loc_id", "data_source", "qtr_num", "period", "grant_period", 
  #                      "function_type", "geography_detail", "grant_name", "lang", "primary_recipient", "secondary_recipient", "status"))
  # 
  # colnames_country = sort(colnames(file_list))
  # stopifnot(colnames_desired == colnames_country)

# ----------------------------------------------
## STEP 3: Prep country-level data 
# ----------------------------------------------
  ifelse(country == "gtm", source(paste0(code_dir, country, "_prep/", "budget_prep/", "master_prep_", country, ".r")),
         source(paste0(code_dir, country, "_prep/", "master_prep_", country, ".r")))
  
}

# ----------------------------------------------
## STEP 4: Aggregate country-level data 
# ----------------------------------------------

  source(paste0(code_dir, "4_aggregate_all_data_sources.r"))

# ----------------------------------------------
## STEP 5: Verify budget numbers
# ----------------------------------------------

  source(paste0(code_dir, "5_verify_budget_numbers.r")) #Emily would eventually like to get this file working. 
 
# ----------------------------------------------
## STEP 6: Upload to Basecamp
# ----------------------------------------------

#Open in Spyder, and run: "C:/Users/user/Documents/gf/resource_tracking/prep/6_basecamp_upload.py"


# ----------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Master file for updating resource tracking database. 
# DATE: Last updated November 2018. 
# ----------------------------------------------

# ---------------------------------------
# Install packages and set up R  
# ---------------------------------------

rm(list=ls())
library(lubridate)
library(data.table)
library(doBy)
library(Hmisc)
library(readxl)
library(stats)
library(stringr)
library(tidyr)
library(tools)
library(rlang)
library(zoo)
library(dplyr)

# ---------------------------------------
# Set global variables and filepaths.  
# ---------------------------------------

#Filepaths
user = "elineb" #Change to your username 
code_dir = paste0("C:/Users/", user, "/Documents/gf/resource_tracking/prep/")
combined_output_dir = "J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping"
country <- "cod" #Change to the country you want to update. 
source(paste0(code_dir, "shared_mapping_functions.R")) 

#Global variables. 
include_stops = FALSE #Set to true if you would like to see error messages in module mapping and budget verification steps. 

# ----------------------------------------------
## STEP 1: Verify module mapping framework 
# ----------------------------------------------
  
  map = read_xlsx('J:/Project/Evaluation/GF/mapping/multi_country/intervention_categories/intervention_and_indicator_list.xlsx', sheet='module_mapping')
  map = data.table(map)
  source(paste0(code_dir, "2_verify_module_mapping.r")) 
  module_map <- prep_map(map)
  
# ----------------------------------------------
# STEP 2: Load country directories and file list
# ----------------------------------------------
  master_file_dir = paste0("J:/Project/Evaluation/GF/resource_tracking/", country, "/grants/")
  export_dir = paste0("J:/Project/Evaluation/GF/resource_tracking/", country, "/prepped/")
  country_code_dir <- paste0(code_dir, "global_fund_prep/", country, "_prep/")
  file_list = fread(paste0(master_file_dir, country, "_budget_filelist.csv"), stringsAsFactors = FALSE)
  file_list$start_date <- as.Date(file_list$start_date, format = "%m/%d/%Y")
  
  desired_cols <- c('file_name', 'sheet', 'function_type', 'start_date', 'disease', 'data_source', 'period', 'qtr_number', 'grant', 'primary_recipient',
                    'secondary_recipient', 'language', 'geography', 'grant_period', 'grant_status', 'file_iteration')
  stopifnot(colnames(file_list) %in% desired_cols)
  
# ----------------------------------------------
# STEP 3: Prep country-level data 
# ----------------------------------------------

  source(paste0(code_dir, "3_prep_country_data.r"))

# ----------------------------------------------
# STEP 4: Aggregate country-level data 
# ----------------------------------------------

  source(paste0(code_dir, "4_aggregate_all_data_sources.r"))

# ----------------------------------------------
# STEP 5: Verify budget numbers
# ----------------------------------------------

  source(paste0(code_dir, "5_verify_budget_numbers.r")) 
 
# ----------------------------------------------
# STEP 6: Upload to Basecamp
# ----------------------------------------------

#Open in Spyder, and run: "C:/Users/user/Documents/gf/resource_tracking/prep/6_basecamp_upload.py"


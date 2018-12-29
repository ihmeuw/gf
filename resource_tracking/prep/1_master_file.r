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

j = ifelse(Sys.info()[1]=='Windows','J:','/home/j')
dir = paste0(j, '/Project/Evaluation/GF/')
code_loc = ifelse(Sys.info()[1]=='Windows','C:/Users/elineb/Documents/gf/','ihme/code/elineb/')

# ---------------------------------------
# Set global variables and filepaths.  
# ---------------------------------------

#Filepaths
user = "elineb" #Change to your username 
code_dir = paste0(code_loc, "resource_tracking/prep/")
combined_output_dir = paste0(j, "resource_tracking/multi_country/mapping")
countries <- c("cod", "gtm", "uga") #Change to the country you want to update. 
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
  
 for (country in countries){
  master_file_dir = paste0("J:/Project/Evaluation/GF/resource_tracking/", country, "/grants/")
  export_dir = paste0("J:/Project/Evaluation/GF/resource_tracking/", country, "/prepped/")
  country_code_dir <- paste0(code_dir, "global_fund_prep/", country, "_prep/")
  file_list = fread(paste0(master_file_dir, country, "_budget_filelist.csv"), stringsAsFactors = FALSE)
  file_list$start_date <- as.Date(file_list$start_date, format = "%m/%d/%Y")
  
  #Validate file list 
  desired_cols <- c('file_name', 'sheet', 'function_type', 'start_date', 'disease', 'data_source', 'period', 'qtr_number', 'grant', 'primary_recipient',
                    'secondary_recipient', 'language', 'geography', 'grant_period', 'grant_status', 'file_iteration')
  stopifnot(colnames(file_list) %in% desired_cols)
  
  stopifnot(sort(unique(file_list$data_source)) == c("fpm", "pudr"))
  stopifnot(sort(unique(file_list$file_iteration)) == c("final", "initial"))
  
  input_fpm <- file_list[data_source == "fpm" & file_iteration == "final", .(grant, grant_period, file_save_date)]
  input_pudr <- file_list[data_source == "pudr" & file_iteration == "final", .(grant, grant_period, file_save_date)]
  
  #Cannot have duplicate 'final' versions of files within grant_number and grant_period. 
  check_fpm = input_fpm[duplicated(input_fpm), ]
  check_pudr = input_pudr[duplicated(input_pudr), ]
  stopifnot(nrow(check_fpm)==0 & nrow(check_pudr)==0)
  
  rm(input_fpm, input_pudr, check_fpm, check_pudr)
  
# ----------------------------------------------
# STEP 3: Prep country-level data 
# ----------------------------------------------

  source(paste0(code_dir, "3_prep_country_data.r"))
  }

# ----------------------------------------------
# STEP 4: Aggregate country-level data 
# ----------------------------------------------

  #source(paste0(code_dir, "4_aggregate_all_data_sources.r"))

# ----------------------------------------------
# STEP 5: Verify budget numbers
# ----------------------------------------------

  #source(paste0(code_dir, "5_verify_budget_numbers.r")) 
 
# ----------------------------------------------
# STEP 6: Upload to Basecamp
# ----------------------------------------------

#Open in Spyder, and run: "C:/Users/user/Documents/gf/resource_tracking/prep/6_basecamp_upload.py"


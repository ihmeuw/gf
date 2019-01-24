# ----------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Master file for updating resource tracking database. 
# DATE: Last updated November 2018. 
# ----------------------------------------------

# ---------------------------------------
# Install packages and set up R  
# ---------------------------------------

# ----------------------------------------------------------------------
# To do list for this code: 
# - add in an option for 'verbose' debugging
# - add in an option to only rework one file (make database append-only)
# - add in variable creation during the append step to flag current grants. 
# ---------------------------------------------------------------------

rm(list=ls())
library(lubridate)
library(data.table)
library(readxl)
library(stats)
library(stringr)
library(tidyr)
library(tools)
library(rlang)
library(zoo)
library(dplyr)

options(scipen=100)

# ---------------------------------------
# Set global variables and filepaths.  
# ---------------------------------------

#Replace global variables to match what code you want to run. 
user = "elineb" #Change to your username 
country <- c("gtm") #Change to the country you want to update. 

#Mark which grants are currently active to save in file - this should be updated every grant period! 
current_gtm_grants <- c('GTM-H-HIVOS', 'GTM-H-INCAP', 'GTM-M-MSPAS', 'GTM-T-MSPAS')
current_gtm_grant_period <- c('2018', '2019-2020', '2018-2020', '2016-2019')

current_cod_grants <- c('COD-C-CORDAID', 'COD-H-MOH', 'COD-T-MOH', 'COD-M-MOH', 'COD-M-SANRU')
current_cod_grant_period <- rep("2018-2020", 5)

current_uga_grants <- c('UGA-C-TASO', 'UGA-H-MoFPED', 'UGA-M-MoFPED', 'UGA-M-TASO', 'UGA-T-MoFPED')
current_uga_grant_period <- rep("2018-2020", 5)

#Filepaths
j = ifelse(Sys.info()[1]=='Windows','J:','/home/j')
dir = paste0(j, '/Project/Evaluation/GF/')
code_loc = ifelse(Sys.info()[1]=='Windows', paste0('C:/Users/', user, '/Documents/gf/'), paste0('ihme/code/', user, '/gf/'))
code_dir = paste0(code_loc, "resource_tracking/prep/")
combined_output_dir = paste0(j, "resource_tracking/multi_country/mapping")
source(paste0(code_dir, "shared_mapping_functions.R")) 

#Boolean logic switches 
include_stops = FALSE #Set to true if you would like scripts to stop when errors are found (specifically, module mapping)
verbose = TRUE #Set to true if you would like warning messages printed (helpful for debugging functions). Urgent messages will always be flagged regardless of this switch. 
limit_filelist <- TRUE #Set to TRUE if you want to only run files that will be saved in final budgets and expenditures. 

# ----------------------------------------------
## STEP 1: Verify module mapping framework 
# ----------------------------------------------
  
  # map = read_xlsx('J:/Project/Evaluation/GF/mapping/multi_country/intervention_categories/intervention_and_indicator_list.xlsx', sheet='module_mapping')
  # map = data.table(map)
  # source(paste0(code_dir, "2_verify_module_mapping.r")) 
  # module_map <- prep_map(map)
  
# ----------------------------------------------
# STEP 2: Load country directories and file list
# ----------------------------------------------
  
  master_file_dir = paste0("J:/Project/Evaluation/GF/resource_tracking/", country, "/grants/")
  export_dir = paste0("J:/Project/Evaluation/GF/resource_tracking/", country, "/prepped/")
  country_code_dir <- paste0(code_dir, "global_fund_prep/", country, "_prep/")
  file_list = fread(paste0(master_file_dir, country, "_budget_filelist.csv"), stringsAsFactors = FALSE)
  file_list$start_date <- as.Date(file_list$start_date, format = "%m/%d/%Y")
  file_list = file_list[, -c('notes')]
  
  #Validate file list 
  desired_cols <- c('file_name', 'sheet', 'function_type', 'start_date', 'disease', 'data_source', 'period', 'qtr_number', 'grant', 'primary_recipient',
                    'secondary_recipient', 'language', 'geography', 'grant_period', 'grant_status', 'file_iteration')
  #stopifnot(colnames(file_list) %in% desired_cols)
  
  stopifnot(sort(unique(file_list$data_source)) == c("fpm", "pudr"))
  stopifnot(sort(unique(file_list$file_iteration)) == c("final", "initial"))
  
  #Turn this variable on to run only a limited section of each country's file list; i.e. only the part that will be kept after GOS data is prioritized in step 4 (aggregate data). 
  if(limit_filelist==TRUE){
    file_list = file_list[file_iteration=='final']
    gos_data <- fread("J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/prepped_gos_data.csv")
    
    gos_data$start_date <- as.Date(gos_data$start_date, "%Y-%m-%d")
    loc <- if(country=='uga'){
      'Uganda'
    } else if (country == 'cod'){
      'Congo (Democratic Republic)'
    } else if (country == 'gtm'){
      'Guatemala'
    }
    gos_data = gos_data[country==loc, ]
    
    #Expand file list by period to see what quarters you're going to get from each file. 
    file_list[, coefficient:=period/90]
    file_list[, num_quarters:=round(qtr_number*coefficient)]
    
    rect_by_qtr <- file_list[rep(1:nrow(file_list), file_list$num_quarters)] # 
    rect_by_qtr[, qtr_count:=seq(0, max(num_quarters)), by=.(file_name)]
    rect_by_qtr[, new_start_date:=start_date + (months(3)*qtr_count)]
    
    #Simplify this data.table so it's easier to compare with GOS. 
    rect_by_qtr = rect_by_qtr[, .(new_start_date, file_name, grant)]
    rect_by_qtr[, year:=year(new_start_date)]
    rect_by_qtr[, quarter:=quarter(new_start_date)]
    
    #See which files will be dropped when GOS data is prioritized in step 4. 
    gos_grant_list <- unique(gos_data[, .(grant_number, start_date)])
    gos_grant_list[, year:=year(start_date)]
    gos_grant_list[, quarter:=quarter(start_date)]
    gos_grant_list[, grant:=grant_number]
    gos_grant_list[, grant_number:=NULL]
    
    files_to_keep <- merge(gos_grant_list, rect_by_qtr, by=c('grant', 'quarter', 'year'), all.y = TRUE)
    
    #If both merge, ok to drop. 
    #If they're only in GOS, ok to drop. 
    #If they're only in budgets, keep these files, these are the files we want to run. 
    
    files_to_keep <- unique(files_to_keep[is.na(start_date), .(file_name)]) #These are the files that didn't match with any GOS data; need to keep and prep this data. 
    
    file_list = file_list[file_name%in%files_to_keep$file_name] #Run it this way so you don't accidentally remove files with quarters you needed to keep. 
    
    #Remove unnecessary variables you created
    file_list[, coefficient:=NULL]
    file_list[, num_quarters:=NULL]
  }
  
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


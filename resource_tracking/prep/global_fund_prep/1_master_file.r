# ----------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Master file for updating resource tracking database. 
# DATE: Last updated November 2018. 
# ----------------------------------------------

rm(list=ls())
# ----------------------------------------------------------------------
# To do list for this code: 
# - add in an option to only rework one file (make database append-only)
# ---------------------------------------------------------------------

user = "elineb" #Change to your username 
country = "uga" #Change to the country you want to update. 
code_loc = ifelse(Sys.info()[1]=='Windows', paste0("C:/Users/", user, "/Documents/gf/"), paste0('/homes/', user, '/gf/'))
source(paste0(code_loc, "resource_tracking/prep/set_up_r.R"))

# ---------------------------------------
#Boolean logic switches 
# ---------------------------------------
prep_files <- TRUE
prep_gos <- FALSE

include_stops = TRUE #Set to true if you would like scripts to stop when errors are found (specifically, module mapping)
# this doesn't appear to have a function in scripts 2-6... is it just planned for later?
verbose = FALSE #Set to true if you would like warning messages printed (helpful for debugging functions). Urgent messages will always be flagged regardless of this switch. 
rerun_filelist <- TRUE  #Set to TRUE if you want to prep all files in the file list again. 
limit_filelist <- TRUE #Set to TRUE if you want to only run files that will be saved in final budgets and expenditures. 

test_current_files = FALSE #Set to true if you would like to run unit tests on current database. Set to false if you would like to 
# run tests on archived database. 

# ----------------------------------------------
# STEP 1: Read in and verify module mapping framework
# ----------------------------------------------

  #Read in the pre- and post-2017 maps, verify them, and rbind them. 
  source(paste0(gf_prep_code, "2_verify_module_mapping.R"))

  post_2017_map = readRDS(paste0(j, "/Project/Evaluation/GF/mapping/multi_country/intervention_categories/post_2017_map.rds"))
  post_2017_map[, module:=as.character(module)]
  post_2017_map[, intervention:=as.character(intervention)]
  post_2017_map[, disease:=as.character(disease)]
  post_2017_map[, loc_name:=as.character(loc_name)]
  post_2017_map[, lang:=as.character(lang)]
  
  post_2017_map = post_2017_map[, .(code, module, intervention, coefficient, disease)]
  
  all_interventions = fread(paste0(j, "/Project/Evaluation/GF/mapping/multi_country/intervention_categories/all_interventions.csv"))
  all_eng = all_interventions[, .(code, module_eng, intervention_eng, disease)]
  setnames(all_eng, old=c('module_eng', 'intervention_eng'), new=c('module', 'intervention'))
  # all_fr = all_interventions[, .(code, module_fr, intervention_fr, disease)]
  # setnames(all_fr, old=c('module_fr', 'intervention_fr'), new=c('module', 'intervention'))
  # all_esp = all_interventions[, .(code, module_esp, intervention_esp, disease)]
  # setnames(all_esp, old=c('module_esp', 'intervention_esp'), new=c('module', 'intervention'))
  # 
  all_langs = list(all_eng)
  all_langs = rbindlist(all_langs)
  all_langs[, coefficient:=1]
  all_langs = strip_chars(all_langs)
  all_langs = all_langs[, -c('orig_module', 'orig_intervention')]
  
  module_map = rbind(post_2017_map, all_langs)
  module_map = unique(module_map)
  
  module_map = prep_map(module_map)
  
# ----------------------------------------------
# STEP 2: Prep a single source of data
# ----------------------------------------------
  
  if (prep_files == TRUE){
    file_list = fread(paste0(master_file_dir, country, "_budget_filelist.csv"), stringsAsFactors = FALSE, encoding="Latin-1")
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
      file_list = prioritize_gos(file_list)
    }
  
  source(paste0(gf_prep_code, "3a_prep_budget_pudr_data.r"))
  }
  
  if (prep_gos == TRUE){
    source(paste0(gf_prep_code, "3b_prep_gos_data.R"))
  }
  
# ----------------------------------------------
# STEP 4: Map prepped data 
# ----------------------------------------------
  
  source(paste0(gf_prep_code, "4_map_data.R"))
  
# ----------------------------------------------
# STEP 5: Aggregate all data sources
# ----------------------------------------------

  source(paste0(code_dir, "aggregate_all_data_sources.R"))

# ----------------------------------------------
# STEP 6: Verify budget numbers
# ----------------------------------------------

  source(paste0(gf_prep_code, "6_verify_financial_numbers.R")) 
 
# ----------------------------------------------
# STEP 7: Upload to Basecamp
# ----------------------------------------------

#Open in Spyder, and run: "C:/Users/user/Documents/gf/resource_tracking/prep/6_basecamp_upload.py"


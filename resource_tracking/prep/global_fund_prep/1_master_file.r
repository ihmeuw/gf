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

user = Sys.info()[['user']] #Change to your username 
country = "cod" #Change to the iso3 code of the country you want to update. 
code_loc = ifelse(Sys.info()[1]=='Windows', 'H:/gf/', paste0('/homes/', user, '/gf/'))
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
limit_filelist <- FALSE #Set to TRUE if you want to only run files that will be saved in final budgets and expenditures. 

# ---------------------------------------
# #Mark which grants are currently active to save in file - this should be updated every grant period! 
# ---------------------------------------

# this info is a good cadidate to save as a little csv somewhere... 
# as a general rule of thumb, try not to embed anything that resembles data/metadata in the code
current_gtm_grants <- c('GTM-H-HIVOS', 'GTM-H-INCAP', 'GTM-M-MSPAS', 'GTM-T-MSPAS')
current_gtm_grant_period <- c('2018', '2019-2020', '2018-2020', '2016-2019')

current_cod_grants <- c('COD-C-CORDAID', 'COD-H-MOH', 'COD-T-MOH', 'COD-M-MOH', 'COD-M-SANRU')
current_cod_grant_period <- rep("2018-2020", 5)

current_uga_grants <- c('UGA-C-TASO', 'UGA-H-MoFPED', 'UGA-M-MoFPED', 'UGA-M-TASO', 'UGA-T-MoFPED')
current_uga_grant_period <- rep("2018-2020", 5)

# ----------------------------------------------
# STEP 1: Read in and verify module mapping framework
# ----------------------------------------------
  
  map = read_xlsx(paste0(j, "/Project/Evaluation/GF/mapping/multi_country/intervention_categories/intervention_and_indicator_list.xlsx"), sheet='module_mapping')
  map = data.table(map)
  source(paste0(gf_prep_code, "2_verify_module_mapping.R"))
  module_map <- prep_map(map)
  
# ----------------------------------------------
# STEP 2: Prep a single source of data
# ----------------------------------------------
  
  if (prep_files == TRUE){
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
  
  #source(paste0(gf_prep_code, "4_map_data.R"))
  
# ----------------------------------------------
# STEP 5: Aggregate all data sources
# ----------------------------------------------

  #source(paste0(code_dir, "aggregate_all_data_sources.r"))

# ----------------------------------------------
# STEP 6: Verify budget numbers
# ----------------------------------------------

 #source(paste0(gf_prep_code, "6_verify_financial_numbers.r")) 
 
# ----------------------------------------------
# STEP 7: Upload to Basecamp
# ----------------------------------------------

#Open in Spyder, and run: "C:/Users/user/Documents/gf/resource_tracking/prep/6_basecamp_upload.py"


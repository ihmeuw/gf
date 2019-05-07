# ----------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Master file for updating resource tracking database. 
# DATE: Last updated March 2019
# 
# The current working directory should be the root of this repository
# ----------------------------------------------

rm(list=ls())
# ----------------------------------------------------------------------
# To do list for this code: 
# - add in an option to only rework one file (make database append-only)
# ---------------------------------------------------------------------

# ----------------------------------------------
# STEP 1: SET UP R
# ----------------------------------------------
setwd("C:/Users/elineb/Documents/gf/") #Change to the root of your repository
source("./resource_tracking/prep/_common/set_up_r.R", encoding="UTF-8")
# ---------------------------------------
# Boolean logic switches 
# ---------------------------------------
prep_files <- TRUE
prep_gos <- FALSE

include_stops = TRUE #Set to true if you would like scripts to stop when errors are found (specifically, module mapping)
verbose = FALSE #Set to true if you would like warning messages printed (helpful for debugging functions). Urgent messages will always be flagged regardless of this switch. 
rerun_filelist <- FALSE #Set to TRUE if you want to prep all files in the file list again. 
limit_filelist <- TRUE #Set to TRUE if you want to only run files that will be saved in final budgets and expenditures. 

test_current_files = TRUE #Set to true if you would like to run unit tests on current database. Set to false if you would like to run tests on archived database. 

# ----------------------------------------------
# STEP 2: GF FILES AND GOS DATA 
# ----------------------------------------------
if (prep_files == TRUE){
  country = "sen" #Change to the country you want to update. Options are "cod", "gtm", "sen", or "uga".  
  master_file_dir = paste0(dir, "_gf_files_gos/", country, "/raw_data/")
  export_dir = paste0(dir, "_gf_files_gos/", country, "/prepped_data/")
}

#Source document prep functions 
doc_prep_functions = list.files(paste0(code_dir, "gf_files_prep_functions"), full.names=TRUE)
for (file in doc_prep_functions){
  source(file)
}

# Load and verify mapping, prep data, and map data. 
source(paste0(code_dir, "2a_gf_files_verify_mapping.R"))
if (prep_files == TRUE){
  source(paste0(code_dir, "2b_gf_files_prep_data.r"))
} else if (prep_gos == TRUE){
  source(paste0(code_dir, "2c_gos_prep_data.R"))
}
source(paste0(code_dir, "2d_gf_files_gos_map_data.R"))
source(paste0(code_dir, "2e_gf_verify_outputs.R"))
source(paste0(code_dir, "2f_gf_visualize_data.rmd"))
  
# ----------------------------------------------
# STEP 3: FGH ACTUALS AND ESTIMATES 
# ----------------------------------------------

#Source document prep functions 
prep_functions = list.files(paste0(code_dir, "fgh_prep_functions"), full.names=TRUE)
for (file in prep_functions){
  source(file)
}

#Prep and map actuals and estimates. *Would be good to add in a mapping verification and calculations verification step! 
source(paste0(code_dir, "3a_fgh_actuals_prep_data.R"))
source(paste0(code_dir, "3b_fgh_estimates_prep_data.R"))
  
# ----------------------------------------------
# STEP 4: GHE (CURRENTLY ONLY SICOIN)
# ----------------------------------------------
#Source document prep functions 
prep_functions = list.files(paste0(code_dir, "ghe_sicoin_prep_functions"), full.names=TRUE)
for (file in prep_functions){
  source(file)
}

# Prep and map SICOIN data.*Would be good to add in a mapping verification and calculations verification step!   
source(paste0(code_dir, "4a_ghe_sicoin_prep_data"))
  
# ----------------------------------------------
# STEP 5: Aggregate all data sources
# ----------------------------------------------

source(paste0(code_dir, "5_aggregate_all_data_sources.R"))

# ----------------------------------------------
# STEP 6: Upload to Basecamp
# ----------------------------------------------

#Open in Spyder, and run: "C:/Users/user/Documents/gf/resource_tracking/prep/6_basecamp_upload.py"


# ----------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Master file for updating resource tracking database. 
# DATE: Last updated July 2019
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
if (Sys.info()[1]=='Windows'){
  setwd("C:/Users/elineb/Documents/gf/") #Change to the root of your repository
} else {
  setwd("/ihme/homes/elineb/gf/")
}
source("./resource_tracking/prep/_common/set_up_r.R", encoding="UTF-8")
source("./resource_tracking/prep/_common/load_master_list.r", encoding="UTF-8")

# ---------------------------------------
# Boolean logic switches 
# ---------------------------------------
#What datasets do you want to run? 
prep_files = TRUE
prep_gos = FALSE
prep_odah = FALSE
prep_ghe = FALSE

#Processing options 
include_stops = TRUE #Set to true if you would like scripts to stop when errors are found (specifically, module mapping) Recommended to always leave as TRUE. 
verbose = FALSE #Set to true if you would like warning messages printed (helpful for debugging functions). Urgent messages will always be flagged regardless of this switch. 
rerun_filelist = FALSE #Set to TRUE if you want to prep all files in the file list again. 
limit_filelist = TRUE #Set to TRUE if you want to only run files that will be saved in final budgets and expenditures. 
test_current_files = TRUE #Set to true if you would like to run unit tests on current database. Set to false if you would like to run tests on archived database. 

# ----------------------------------------------
# STEP 2: PREP GF FILES AND GOS DATA 
# ----------------------------------------------
if (prep_files | prep_gos){
  if (prep_files){
    country = "cod" #Change to the country you want to update. Options are "cod", "gtm", "sen", or "uga".  
    master_file_dir = paste0(dir, "_gf_files_gos/", country, "/raw_data/")
    export_dir = paste0(dir, "_gf_files_gos/", country, "/prepped_data/")
  }
  
  #Source document prep functions 
  doc_prep_functions = list.files(paste0(code_dir, "gf_files_prep_functions"), full.names=TRUE)
  for (file in doc_prep_functions){
    source(file, encoding="UTF-8")
  }
  
  # Load and verify mapping, prep data, and map data. 
  source(paste0(code_dir, "2a_gf_files_verify_mapping.R"))
  if (prep_files){
    source(paste0(code_dir, "2b_gf_files_prep_data.r"))
  } else if (prep_gos){
    source(paste0(code_dir, "2b_gos_prep_data.R"))
  }
  source(paste0(code_dir, "2c_gf_files_gos_map_data.R"))
  # # source(paste0(code_dir, "2e_gf_aggregate_files.R"))
  source(paste0(code_dir, "2f_gf_verify_outputs.R"))
  # 
  # rmarkdown::render(paste0(code_dir, "2g_gf_visualize_data.rmd",
  #                          output_dir=paste0(dir, "/visualizations/verification"),
  #                          output_file="Visual Checks.pdf"))
}

#Run data gap analysis - optional
rmarkdown::render(paste0(code_dir, "reporting_completeness_gf.rmd"),
                  output_dir=paste0(dir, "/visualizations/verification"),
                  output_file="Reporting Completeness.pdf")
#
# # #Run unclassified file analysis - optional
rmarkdown::render(paste0(code_dir, "unclassified_files.rmd"),
                  output_dir=paste0(dir, "/visualizations/verification"),
                  output_file="Unclassified Files.pdf")
# ----------------------------------------------
# STEP 3: PREP FGH ACTUALS AND ESTIMATES 
# ----------------------------------------------
if (prep_odah){
  #Source document prep functions 
  prep_functions = list.files(paste0(code_dir, "odah_prep_functions"), full.names=TRUE)
  for (file in prep_functions){
    source(file, encoding="UTF-8")
  }
  
  #Prep and map actuals and estimates. *Would be good to add in a mapping verification and calculations verification step! 
  source(paste0(code_dir, "3a_fgh_actuals_prep_data.R"))
  # source(paste0(code_dir, "3b_fgh_estimates_prep_data.R"))
  source(paste0(code_dir, "3c_fgh_validate_data.r"))
} 
# ----------------------------------------------
# STEP 4: PREP GHE (CURRENTLY ONLY SICOIN)
# ----------------------------------------------
if (prep_ghe){
  source(paste0(code_dir, "2a_gf_files_verify_mapping.R"))
  #Source document prep functions 
  prep_functions = list.files(paste0(code_dir, "ghe_sicoin_prep_functions"), full.names=TRUE)
  for (file in prep_functions){
    source(file, encoding="UTF-8")
  }
  
  # Prep and map SICOIN data.*Would be good to add in a mapping verification and calculations verification step!   
  source(paste0(code_dir, "4a_ghe_sicoin_prep_data"))
} 

# ----------------------------------------------
# STEP 5: UPLOAD TO BASECAMP 
# ----------------------------------------------

#Open in Spyder, and run: "C:/Users/user/Documents/gf/resource_tracking/prep/6_basecamp_upload.py"


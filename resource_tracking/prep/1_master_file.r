# ----------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Master file for updating resource tracking database. 
# DATE: Last updated February 2020 
# 
# The current working directory should be the root of this repository
# ----------------------------------------------

rm(list=ls())
# ----------------------------------------------------------------------
# To do list for this code: 
# - add in an option to only rework one file (make database append-only)
# - the step run in file 2f is not working
# ---------------------------------------------------------------------

# ----------------------------------------------
# STEP 1: SET UP R
# ----------------------------------------------
user=as.character(Sys.info()[7])
if (Sys.info()[1]=='Windows'){
  if(user == 'abatzel'){ 
    repo_root = "C:/local/gf/"
  } else {
    repo_root = paste0("C:/Users/", user, "/Documents/gf/")} #Change to the root of your repository
  setwd(repo_root)
} else {
  setwd(paste0("/ihme/homes/",user,"/gf/"))
}
source("./resource_tracking/prep/_common/set_up_r.R", encoding="UTF-8")
source("./resource_tracking/prep/_common/load_master_list.r", encoding="UTF-8")

# ---------------------------------------
# Boolean logic switches 
# ---------------------------------------
# What datasets do you want to run? 
prep_files = TRUE
prep_gos = FALSE
prep_odah = FALSE
prep_ghe = FALSE
prep_cost_categories = FALSE
prep_commitments = FALSE

#Processing options 
include_stops = TRUE #Set to TRUE if you would like scripts to stop when errors are found (specifically, module mapping) Recommended to always leave as TRUE. 
verbose = FALSE #Set to true if you would like warning messages printed (helpful for debugging functions). Urgent messages will always be flagged regardless of this switch. 
rerun_filelist = TRUE  #Set to TRUE if you want to prep all files in the file list again. 
limit_filelist = TRUE #Set to TRUE if you want to only run files that will be saved in final budgets and expenditures. 
only_new_files = FALSE # Set to TRUE if, when you re-run file list, you only want to process files that are additional. TRUE is the default. 
include_zero_pudrs = FALSE # Set to TRUE if when you re-run file list, the final data set will include PUDRs with zero expenditure, if FALSE the PUDRs will not be included. FALSE is default

country = "sen" #Change to the country you want to update. Options are "cod", "gtm", "sen", or "uga". 

# ----------------------------------------------
# STEP 2: PREP GF FILES AND GOS DATA 
# ----------------------------------------------
if (prep_files | prep_gos){
  if (prep_files){

    master_file_dir = ifelse(Sys.info()[1]=='Windows', paste0(box, toupper(country), "/raw_data/"), 
                             paste0(dir, "_gf_files_gos/", country, "/raw_data/"))
    export_dir = ifelse(Sys.info()[1]=="Windows", paste0(box, country, "/prepped_data/"),
                        paste0(dir, "_gf_files_gos/", country, "/prepped_data/"))
  }
  
  #Source document prep functions 
  doc_prep_functions = list.files(paste0(code_dir, "gf_files_prep_functions"), full.names=TRUE)
  for (file in doc_prep_functions){
    source(file, encoding="UTF-8")
  }
  
  # Load and verify mapping, prep data, and map data. 
  source(paste0(code_dir, "2a_gf_files_verify_mapping.R"))
  if (prep_files){
    source(paste0(code_dir, "2b_gf_files_prep_data.R"))
  } else if (prep_gos){
    source(paste0(code_dir, "2b_gos_prep_data.R"))
  }
  source(paste0(code_dir, "2c_gf_files_gos_map_data.R")) # Step 2d is sourced in step 2c. 
  source(paste0(code_dir, "2e_gf_aggregate_files.R"))
}

# Need to revisit to implement (AB - 6/11)
  source(paste0(code_dir, "2f_gf_verify_outputs.R")) 
   
  rmarkdown::render(input=paste0(code_dir, "2g_gf_visualize_data.rmd"),
                           output_dir=paste0(dir, "/visualizations/verification/"),
                           output_file="Visual Checks.pdf")
  
  #Run data gap analysis - optional
  rmarkdown::render(paste0(code_dir, "reporting_completeness_gf.rmd"),
                    output_dir=paste0(dir, "/visualizations/verification"),
                    output_file="Reporting Completeness.pdf")
  
  #Run unclassified file analysis - optional
  rmarkdown::render(paste0(code_dir, "unclassified_files.rmd"),
                    output_dir=paste0(dir, "/visualizations/verification"),
                    output_file="Unclassified Files.pdf")
  source(paste0(code_dir, "2f_gf_verify_outputs.R"))

# rmarkdown::render(input=paste0(code_dir, "2g_gf_visualize_data.rmd"),
#                   output_dir=paste0(dir, "/visualizations/verification/"),
#                   output_file="Visual Checks.pdf")
# 
# #Run data gap analysis - optional
# rmarkdown::render(paste0(code_dir, "reporting_completeness_gf.rmd"),
#                   output_dir=paste0(dir, "/visualizations/verification"),
#                   output_file="Reporting Completeness.pdf")
# 
# #Run unclassified file analysis - optional
# rmarkdown::render(paste0(code_dir, "unclassified_files.rmd"),
#                   output_dir=paste0(dir, "/visualizations/verification"),
#                   output_file="Unclassified Files.pdf")

# ----------------------------------------------
# STEP 3: PREP GHE 
# ----------------------------------------------
if (prep_ghe){
  source(paste0(code_dir, "2a_gf_files_verify_mapping.R"))
  #Source document prep functions 
  prep_functions = list.files(paste0(code_dir, "ghe_sicoin_prep_functions"), full.names=TRUE)
  for (file in prep_functions){
    source(file, encoding="UTF-8")
  }
  
  # Prep and map SICOIN data.*Would be good to add in a mapping verification and calculations verification step!   
  source(paste0(code_dir, "3a_ghe_fgh_actuals_prep_data.r"))
  source(paste0(code_dir, "3b_ghe_fgh_estimates_prep_data.r"))
  source(paste0(code_dir, "3c_ghe_sicoin_prep_data.r"))
  # source(paste0(code_dir, "3d_ghe_who_prep_data.r")) # Currently not prepping this data - Emily Linebarger 2/12/2020
  source(paste0(code_dir, "3e_ghe_map_sicoin.r"))
  #source(paste0(code_dir, "3f_ghe_validate_data.r")) # CURRENTLY NOT RUNNING EMILY LINEBARGER 2/12/2020 
  source(paste0(code_dir, "3g_ghe_aggregate_data.r"))
} 

# ---------------------------------------------------------
# STEP 3: PREP OTHER DEVELOPMENT ASSISTANCE FOR HEALTH
# ---------------------------------------------------------
if (prep_odah){
  #Source document prep functions 
  prep_functions = list.files(paste0(code_dir, "odah_prep_functions"), full.names=TRUE)
  for (file in prep_functions){
    source(file, encoding="UTF-8")
  }
  #Prep other development assistance for health data from the financing global health team. 
  source(paste0(code_dir, "4a_other_dah_prep_data.r"))
  # *Would be good to add in a mapping verification and calculations verification step! Emily Linebarger 2/12/2020 
} 

# ----------------------------------------------
# STEP 5: AGGREGATE ACROSS DATA SOURCES
# ----------------------------------------------

# This data is currently not running! Emily Linebarger 2/12/2020 
#source(paste0(code_dir, "5_aggregate_all_data_sources.r"))

# ----------------------------------------------
# STEP 6: VALIDATE DATA AND RELEASE TO PARTNERS - * Should probably be moved to end of pipeline. 
# ----------------------------------------------

# This code is not running 2/12/2020 Emily Linebarger 
#source(paste0(code_dir, "6_validate_and_upload.r")) 

#---------------------------------------------------
# STEP 7: PREP COST CATEGORY PUDR DATA 
#---------------------------------------------------
if (prep_cost_categories) {
  countries = c('cod', 'gtm', 'sen', 'uga') # Options are 'cod', 'gtm', 'sen', 'uga'. 
  
  source(paste0(code_dir, "gf_files_prep_functions/prep_cost_category_pudr.r"))
  source(paste0(code_dir, "7a_cost_categories_prep_data.r"))
  source(paste0(code_dir, "7b_cost_categories_clean_data.r"))
  
}

#---------------------------------------------------
# STEP 8: PREP COMMITMENTS, OBLIGATIONS DATA
#---------------------------------------------------
if (prep_commitments) {
  countries = c('cod', 'gtm', 'sen', 'uga') # Options are 'cod', 'gtm', 'sen', 'uga'. 
  
  source(paste0(code_dir, "2a_gf_files_verify_mapping.R"))
  source(paste0(code_dir, "gf_files_prep_functions/prep_commitments_pudr.r"))
  source(paste0(code_dir, "8a_commitments_obligations_prep.r"))
  source(paste0(code_dir, "8b_commitments_obligations_clean.r"))
  
}

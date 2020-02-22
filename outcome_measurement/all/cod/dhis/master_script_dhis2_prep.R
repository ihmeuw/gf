# Audrey Batzel
# December 12, 2019

# Master script to (hopefully) organize the DHIS2 prep process
# Run on the cluster
library(data.table)
library(lubridate)
#---------------------------------------------

#---------------------------------------------
# Set switches
#---------------------------------------------
# Note: If you run step 1 - the actual download - it should be done on the cluster because it takes forever
# Note: Step X - outlier detection - must also be run on the cluster
rerun_metadata_extraction = FALSE

step1_extract_data = FALSE
step2_merge_metadata = FALSE
step3_remove_outliers = FALSE
step4_addtiional_prep = FALSE

set = 'base'
#---------------------------------------------

#---------------------------------------------
# Files and directories
#---------------------------------------------
# set up: clone the ihme gf repo to your H drive to run on the cluster
user = Sys.info()[['user']]
setwd(paste0('/homes/', user, '/local/gf/')) # set wd to root of the repo
code_dir = ('./outcome_measurement/all/cod/dhis/')
#---------------------------------------------

#---------------------------------------------
# Meta data extraction
#---------------------------------------------
if (rerun_metadata_extraction){
  
}
#---------------------------------------------

#---------------------------------------------
# Step 1 - extract data using the DHIS2 download tool
#---------------------------------------------
if (step1_extract_data) { 
  # Step 1a - download data
    # define main directory
      root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
      dir = paste0(root, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/')
    # source the necessary functions to download the data 
      source(paste0(dir, 'code/dhis_extracting_functions.R')) 
    # check to make sure the package loaded by viewing a help file
      ?extract_all_data
  
    source(paste0(code_dir, 'extract_dhis/extract_data_dhis.R'))
  
    run_extraction_tool(start_year = '2018', end_year = as.character(year(Sys.Date())), 
                        start_month = '01', end_month = as.character(month(Sys.Date())), 
                        set_name = set)
  
  # Step 1b - combine intermediate data files
    source(paste0(code_dir, 'extract_dhis/aggregate_extracted_data_dhis.R'))
}
#---------------------------------------------

#---------------------------------------------
# Step 2 - merge metadata
#---------------------------------------------
if (step2_merge_metadata) { 
  source(paste0(code_dir, 'prep_dhis/merge_dhis_for_new_download.R'))
  source(paste0(code_dir, 'prep_dhis/combine_data_downloads.R'))
}
#---------------------------------------------

#---------------------------------------------
# Step 3 - outlier removal
#---------------------------------------------
# note: SIGL data on stockouts is outlier-screened differently since stockouts are in # of days per month... so we just screen out values >31. 
if (step3_remove_outliers) { 
  # Step 3a - run QR to detect outliers
    #------------------------------------
    # switches
    cleanup_start = TRUE # whether or not to delete all files from parallel runs at the beginning
    cleanup_end = FALSE # "" /end; default to FALSE
    agg_to_DPS = FALSE # whether or not to aggregate the data to DPS level before running QR. 
    #------------------------------------
    source(paste0(code_dir, 'outlier_removal/run_quantreg_parallel.r'))
    
  # Step 3b - run code to create outlier graphs and replace outliers
    source(paste0(code_dir, 'outlier_removal/visualize_qr_outliers.R'))
}
#---------------------------------------------

#---------------------------------------------
# Step 4 - additional prep 
#---------------------------------------------
#***Switch to local computer*** - renaming function doesn't work on the cluster. 
if (set %in% c('base', 'sigl', 'secondary')){
  if (step4_addtiional_prep) {
    source(paste0(code_dir, 'prep_dhis/additional_prep.R'))
  }
}
#---------------------------------------------




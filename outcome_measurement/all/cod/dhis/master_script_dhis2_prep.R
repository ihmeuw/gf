# Audrey Batzel
# December 12, 2019

# Master script to (hopefully) organize the DHIS2 prep process
# Run on the cluster
#---------------------------------------------

#---------------------------------------------
# Files and directories
#---------------------------------------------
# set up: clone the ihme gf repo to your H drive to run on the cluster
user = Sys.info()[['user']]
setwd(paste0('/homes/', user, '/local/')) # set wd to root of the repo
code_dir = ('./gf/outcome_measurement/all/cod/dhis/')
#---------------------------------------------

#---------------------------------------------
# Set switches
#---------------------------------------------
# Note: If you run step 1 - the actual download - it should be done on the cluster because it takes forever
# Note: Step X - outlier detection - must also be run on the cluster
rerun_metadata_extraction = FALSE

step1_extract_data = TRUE
step2_prep_data = TRUE
step3_remove_outliers = TRUE

set = 'base'
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
  source(paste0(code_dir, 'extract_dhis/extract_data_dhis.R'))
  
  run_extraction_tool(start_year = '2018', end_year = '2019', start_month = '01', end_month = '12', 
                      set = '1', set_name = set)

  # Step 1b - combine intermediate data files
  source(paste0(code_dir, 'extract_dhis/aggregate_extracted_data_dhis.R'))
}
#---------------------------------------------

#---------------------------------------------
# Step 2 - prep data
#---------------------------------------------
if (step2_prep_data) { 
  # Step 2a - merge meta data
  source(paste0(code_dir, 'prep_dhis/merge_dhis_for_new_download.rds'))
  # Step 2b - run prep code
  if (set == 'base' | set == 'sigl'){
    source(paste0(code_dir, 'prep_dhis/additional_prep.rds'))
  }
  # Step 2c - run checks on the download
  source(paste0(code_dir, 'prep_dhis/run_checks_on_new_download.rds'))
}
#---------------------------------------------

#---------------------------------------------
# Step 3 - outlier removal
#---------------------------------------------
if (step3_remove_outliers) { 
  # Step 3a - run QR to detect outliers
  
  # Step 3b - run code to create outlier graphs
  
  # Steb 3c remove outliers/replace with fitted values
}
#---------------------------------------------




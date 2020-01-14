# Audrey Batzel
# December 12, 2019

# Master script to (hopefully) organize the DHIS2 prep process
# Run on the cluster
#---------------------------------------------

#---------------------------------------------
# Files and directories
#---------------------------------------------
# clone the ihme gf repo to your H drive
user = Sys.info()[['user']]
setwd(paste0('/homes/', user))
code_dir = ('./local/gf/outcome_measurement/all/cod/dhis/')
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
  
  # Step 2b - run prep code
  
  # Step 2c - run checks on the download
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




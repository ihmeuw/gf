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
step2_prep_data = TRUE
step3_remove_outliers = FALSE

set = 'secondary'

# note: start month and year are inclusive/end month is exclusive
start_month = '01'
start_year = '2017'
end_month = as.character(month(Sys.Date()))
if (nchar(end_month)==1) end_month = paste0('0', end_month)
end_year = as.character(year(Sys.Date()))
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
  source(paste0(code_dir, 'extract_dhis/extract_data_dhis.R'))

  # Step 1b - combine intermediate data files
  source(paste0(code_dir, 'extract_dhis/aggregate_extracted_data_dhis.R'))
}
#---------------------------------------------

#---------------------------------------------
# Step 2 - prep data
#---------------------------------------------
if (step2_prep_data) { 
  # Step 2a - merge meta data
  source(paste0(code_dir, 'prep_dhis/merge_dhis_for_new_download.R'))
}
# Switch to local computer - renaming function doesn't work on the cluster. 
# ***Step 2b - run prep code - RUN THIS PART LOCALLY, not on the cluster***
  if (set %in% c('base', 'sigl', 'secondary')){
    source(paste0(code_dir, 'prep_dhis/additional_prep.R'))
  # Step 2c - run checks on the download
  source(paste0(code_dir, 'prep_dhis/run_checks_on_new_download.R'))
  }
#---------------------------------------------

#---------------------------------------------
# Step 3 - outlier removal
#---------------------------------------------
if (step3_remove_outliers) { 
  # Step 3a - run QR to detect outliers
  source(paste0(code_dir, 'outlier_removel/run_quantreg_parallel.R'))
  source(paste0(code_dir, 'outlier_removel/agg_qr_results.R'))
  # Step 3b - run code to create outlier graphs
  source(paste0(code_dir, 'outlier_removel/visualize_qr_outliers.R'))
  # Steb 3c remove outliers/replace with fitted values
  # source(paste0(code_dir, 'outlier_removel/'))
}
#---------------------------------------------




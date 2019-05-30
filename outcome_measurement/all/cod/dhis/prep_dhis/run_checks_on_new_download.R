# ----------------------------------------------
# Audrey Batzel
# 3/1/19

# Quick tests for new DHIS2 download (compare to previous with overlapping dates to make sure data isn't missing in new download)
setwd('C:/local/gf/')
# ----------------------------------------------

# ----------------------------------------------
# Overview - Files and Directories
# ----------------------------------------------
rm(list=ls())
# data directory
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/')

# input files
new_base_data <- paste0(dir, "pre_prep/merged/base_2018_01_01_2019_01_01.rds")
archive_base_data <- paste0(dir, "prepped/archive/base_services_drc_01_2017_09_2018_prepped.rds")

new_sigl_data <- paste0(dir, "pre_prep/merged/sigl_2018_01_01_2019_01_01.rds")
archive_sigl_data <- paste0(dir, "prepped/archive/sigl_drc_01_2015_07_2018_prepped.rds")
# ----------------------------------------------

# ----------------------------------------------
# Read in the data
# ----------------------------------------------
new_data = readRDS(new_sigl_data)
old_data = readRDS(archive_sigl_data)
# ----------------------------------------------

# ----------------------------------------------
# Function to run checks on two versions of a data set
# ----------------------------------------------
run_checks <- function(new, old, data_element_to_check){
  new$set = "new"
  old$set = "old"
  
  new$element <- as.character(new$element)
  old$element <- as.character(old$element)
  new_elements <- unique(new$element)
  old_elements <- unique(old$element)
  
  # subset data sets to just data elements that are in both for comparisons:
  print(paste0( "These data elements are missing in the new data download compared to the archived version", cat( unique(old$element)[! unique(old$element) %in% new_elements ], sep = "\n")))
  
  new = new[element %in% old_elements, ]
  old = old[element %in% new_elements, ]
  
  # ----------------------------------------------
  # Tests
  # ----------------------------------------------
  # make sure all 26 DPS are present in the new download
  if ( length(unique(new$dps)) != 26 ) {
    stop( paste0("Problem in new download: DPS missing = ", list( unique(old$dps)[!unique(old$dps) %in% unique(new$dps)]) ))
  } else {
    print( "All DPS in new download!")
  }
  # ----------------------------------------------
  # combine new and old data tables:
  dt = rbindlist(list(new, old), use.names = TRUE, fill = TRUE)
  
  # set min and max dates that can be compared
  if (min(new$date) >= min(old$date)){
    min_date = min(new$date) 
  } else {
    min_date = min(old$date)
  }
  if (max(new$date) <= max(old$date)){
    max_date = max(new$date) 
  } else {
    max_date = max(old$date)
  }
  
  dt = dt[date >= min_date, ]
  dt = dt[date <= max_date, ]
  # ----------------------------------------------
  print("Comparing number of facilities reporting at the dps - month level... ")
  # compare facility numbers - make sure at least as many facilities in new download for the same dates/dps as old download
  fac_comp = dt[org_unit_type == "facility", unique(org_unit_id), by = c("dps", "date", "set")]
  fac_comp = fac_comp[, .N, by = c("dps", "date", "set")]
  setnames(fac_comp, "N", "num_fac_reporting")
  
  fac_comp <- dcast.data.table(fac_comp, dps + date ~ set, value.var = 'num_fac_reporting')
  
  fac_comp[, diff:= new - old]
  fac_comp[, new_less_than_old := ifelse(diff < 0, TRUE, FALSE)]  
  
  print( "These are the dps-months that have lower numbers of facilities reporting: ")
  print( fac_comp[new_less_than_old== TRUE, ])
  max_loss <- fac_comp[ diff < 0, max(abs(diff))]
  print( "The maximum loss in number of facilities is: ")
  print(max_loss)
  print( "These are the dps-months with the maximum loss in facilities: ")
  print( fac_comp[diff < 0 & (abs(diff))==max_loss, ] )
  
  # ----------------------------------------------
  print("Comparing number of facilities reporting at the hz - month level... ")
  # compare facility numbers at the health zone - month level
  fac_comp_hz = dt[org_unit_type == "facility", unique(org_unit_id), by = c("date", "dps", "health_zone", "set")]
  fac_comp_hz = fac_comp_hz[, .N, by = c("dps", "date", "health_zone", "set")]
  setnames(fac_comp_hz, "N", "num_fac_reporting")
  
  fac_comp_hz <- dcast.data.table(fac_comp_hz, dps + health_zone + date ~ set, value.var = 'num_fac_reporting')
  
  fac_comp_hz[, diff:= new - old]
  fac_comp_hz[, new_less_than_old := ifelse(diff < 0, TRUE, FALSE)]  
  
  print( "These are the hz-months that have lower numbers of facilities reporting: ")
  print( fac_comp_hz[new_less_than_old== TRUE, ])
  max_loss_hz <- fac_comp_hz[ diff < 0, max(abs(diff))]
  print( "The maximum loss in number of facilities is: ")
  print(max_loss_hz)
  print( "These are the hz-months with the maximum loss in facilities: ")
  print( fac_comp_hz[diff < 0 & abs(diff)==max_loss_hz, ] )
  
  # ----------------------------------------------
  print(paste0("Comparing \'", data_element_to_check, "\' reported at the dps - month level... "))
  # compare cases of malaria at the dps level
  dt$value <- as.numeric(dt$value)
  value_comp = dt[element== data_element_to_check, sum(value), by = c("dps", "date", "set")]
  value_comp <- dcast.data.table(value_comp, dps + date ~ set, value.var = 'V1')
  
  value_comp[, diff:= new - old]
  value_comp[, new_less_than_old := ifelse(diff < 0, TRUE, FALSE)]  
  print( paste0("These are the dps-months that have lower numbers of ", data_element_to_check, ": "))
  print( value_comp[new_less_than_old== TRUE, ])
  
  # ----------------------------------------------
  print(paste0("Comparing \'", data_element_to_check, "\' reported at the hz - month level... "))
  # compare cases of malaria at the hz level
  value_comp_hz = dt[element== data_element_to_check, sum(value), by = c("dps", "health_zone", "date", "set")]
  value_comp_hz <- dcast.data.table(value_comp_hz, dps + health_zone + date ~ set, value.var = 'V1')
  
  value_comp_hz[, diff:= new - old]
  value_comp_hz[, new_less_than_old := ifelse(diff < 0, TRUE, FALSE)]  
  print( paste0("These are the hz-months that have lower numbers of ", data_element_to_check, ": "))
  print( value_comp_hz[new_less_than_old== TRUE, ])
  
}  
# ----------------------------------------------


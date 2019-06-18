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
library(data.table)
# data directory
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/')

# input files
new_base_data <- paste0(dir, "prepped/base_services_prepped.rds") 
base_data_2017 =  paste0(dir, "pre_prep/base/base_01_2016_01_2019.rds") # 2017 data from the "problem" download in January, so need to check it 
archive_base_data <- paste0(dir, "prepped/archive/base_services_drc_01_2017_09_2018_prepped.rds")
prev_base_data = paste0(dir, 'pre_prep/merged/base_2018_01_01_2019_01_01.rds')

new_sigl_data = paste0(dir, "prepped/sigl/sigl_prepped.rds")
prev_sigl_data = paste0(dir, "pre_prep/merged/sigl_2018_01_01_2019_01_01.rds")
archive_sigl_data = paste0(dir, "prepped/archive/sigl_drc_01_2015_07_2018_prepped.rds")
# ----------------------------------------------

# ----------------------------------------------
# Read in the data
# ----------------------------------------------
new = readRDS(new_sigl_data)
old = readRDS(prev_sigl_data)
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
  
  print(paste0( "These data elements are new in the new data download and missing in the archived version", cat( unique(new$element)[! unique(new$element) %in% old_elements ], sep = "\n")))
  
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
  
  dt[ , dps := standardizeDPSNames(dps)]
  dt[ , health_zone := standardizeHZNames(health_zone)]
  # subset to 2017 to compare versions of that data:
  # dt = dt[date >= "2017-01-01", ]
  # dt = dt[date <= "2017-12-01", ]
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
  
  print( "The total loss in number of facilities in the new download is: ")
  print(sum(fac_comp[diff < 0, abs(diff)], na.rm = TRUE))
  print( "The total gain in number of facilities in the new download is: ")
  print(sum(fac_comp[diff > 0, abs(diff)], na.rm = TRUE))
  
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
  
  print( "The total loss in number of facilities in the new download is: ")
  print(sum(fac_comp_hz[diff < 0, abs(diff)], na.rm = TRUE))
  print( "The total gain in number of facilities in the new download is: ")
  print(sum(fac_comp_hz[diff > 0, abs(diff)], na.rm = TRUE))
  
  # ----------------------------------------------
  print(paste0("Comparing \'", data_element_to_check, "\' reported at the dps - month level... "))
  # compare cases of malaria at the dps level
  dt$value <- as.numeric(dt$value)
  value_comp = dt[element== data_element_to_check, sum(value, na.rm = TRUE), by = c("dps", "date", "set")]
  value_comp <- dcast.data.table(value_comp, dps + date ~ set, value.var = 'V1')
  
  value_comp[, diff:= new - old]
  value_comp[, new_less_than_old := ifelse(diff < 0, TRUE, FALSE)]  
  print( paste0("These are the dps-months that have lower numbers of ", data_element_to_check, ": "))
  print( value_comp[new_less_than_old== TRUE, ])
  
  max_loss_hz <- value_comp[ diff < 0, max(abs(diff))]
  print( paste0( "The maximum negative difference in ", data_element_to_check, " is: "))
  print(max_loss_hz)
  
  print(  paste0("The total loss in ", data_element_to_check, " in the new download is: "))
  print(sum(value_comp[diff < 0, abs(diff)], na.rm = TRUE))
  print(  paste0("The total gain in ", data_element_to_check, " in the new download is: "))
  print(sum(value_comp[diff > 0, abs(diff)], na.rm = TRUE))
  
  value_comp[is.na(new), .N]
  value_comp[is.na(old), .N]
  
  # ----------------------------------------------
  print(paste0("Comparing \'", data_element_to_check, "\' reported at the hz - month level... "))
  # compare cases of malaria at the hz level
  value_comp_hz = dt[element== data_element_to_check, sum(value, na.rm = TRUE), by = c("dps", "health_zone", "date", "set")]
  value_comp_hz <- dcast.data.table(value_comp_hz, dps + health_zone + date ~ set, value.var = 'V1')
  
  value_comp_hz[, diff:= new - old]
  value_comp_hz[, new_less_than_old := ifelse(diff < 0, TRUE, FALSE)]  
  print( paste0("These are the hz-months that have lower numbers of ", data_element_to_check, ": "))
  print( value_comp_hz[new_less_than_old== TRUE, ])
  
  max_loss_hz <- value_comp_hz[ diff < 0, max(abs(diff))]
  print( "The maximum negative difference is: ")
  print(max_loss_hz)
  
  print(  paste0("The total loss in ", data_element_to_check, " in the new download is: "))
  print(sum(value_comp_hz[diff < 0, abs(diff)], na.rm = TRUE))
  print(  paste0("The total gain in ", data_element_to_check, " in the new download is: "))
  print(sum(value_comp_hz[diff > 0, abs(diff)], na.rm = TRUE))
  
  value_comp_hz[is.na(new), .N]
  value_comp_hz[is.na(old), .N]
  # ----------------------------------------------
  print(paste0("Comparing \'", data_element_to_check, "\' reported at the facility - month level... "))
  # compare cases of malaria at the hz level
  value_comp_fac = dt[element== data_element_to_check, sum(value, na.rm = TRUE), by = c("org_unit_id", "date", "set")]
  value_comp_fac <- dcast.data.table(value_comp_fac, org_unit_id + date ~ set, value.var = 'V1')
  
  value_comp_fac[, diff:= new - old]
  value_comp_fac[, new_less_than_old := ifelse(diff < 0, TRUE, FALSE)]  
  print( paste0("These are the facility-months that have lower numbers of ", data_element_to_check, ": "))
  print( value_comp_fac[new_less_than_old== TRUE, ])
  
  max_loss_hz <- value_comp_fac[ diff < 0, max(abs(diff))]
  print( "The maximum negative difference is: ")
  print(max_loss_hz)
  
  print(  paste0("The total loss in ", data_element_to_check, " in the new download is: "))
  print(sum(value_comp_fac[diff < 0, abs(diff)], na.rm = TRUE))
  print(  paste0("The total gain in ", data_element_to_check, " in the new download is: "))
  print(sum(value_comp_fac[diff > 0, abs(diff)], na.rm = TRUE))
  
  value_comp_fac[is.na(new), .N]
  value_comp_fac[is.na(old), .N]
}  
# ----------------------------------------------

# ----------------------------------------------
# check national values compared to national values on SNIS dashboard
# ----------------------------------------------
#sum to natl level
natl = new[, .(value = sum(value, na.rm = TRUE)), by = c('date', 'element', 'category')] 

ggplot(natl[ element == 'A 1.4 Paludisme grave', ], aes(x = date, y = value)) + geom_point() + theme_bw()

natl = dt[, .(value = sum(value, na.rm = TRUE)), by = c('date', 'element')] 
natl_wide = dcast.data.table(natl, date ~ element)
# ----------------------------------------------




#-----------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Load and validate Master GF File List 
# DATE: August 2019 
# ----------------------------------------------

load_master_list = function(path) {
  file_list = read_xlsx(path, col_types="text")
  setDT(file_list)
  
  #Fix value types 
  ints = c("period_financial", "qtr_number_financial")
  dates = c("start_date_financial", "start_date_programmatic", "end_date_programmatic", "update_date", "ihme_received_date")
  logicals = c("mod_framework_format") 
  
  file_list = file_list[, lapply(.SD, as.integer), .SDcols=ints, with=F]
  
  
  file_list = file_list[order(loc_name, grant_period, grant_period, data_source, file_name)] #So that you always get consistent ordering, even if the excel beneath is filtered. 
  file_list = file_list[loc_name==country]
  file_list$start_date_financial <- as.Date(file_list$start_date_financial, format = "%Y-%m-%d")
  file_list = file_list[, -c('notes')]
}
# ----------------------------------------------
# AUTHOR: Audrey Batzel and Francisco Rios Casas based on code by Emily Linebarger and Irena Chen
# PURPOSE: Prep commonly-formatted FR budgets using the prep_fr_budgets() function - runs all countries at once
# DATE: Last updated June 2020.

# TO DO: 
# ----------------------------------------------
rm(list=ls())

# ----------------------------------------------
# Initial set up
user=as.character(Sys.info()[7])
if (Sys.info()[1]=='Windows'){
  setwd(paste0("C:/Users/",user,"/Documents/gf/")) #Change to the root of your repository
} else {
  setwd(paste0("/ihme/homes/",user,"/gf/"))
}
source("./resource_tracking/prep/_common/set_up_r.R", encoding="UTF-8")
source("./resource_tracking/prep/_common/load_master_list.r", encoding="UTF-8")
source("./resource_tracking/prep/gf_files_prep_functions/prep_fr_budgets.R", encoding="UTF-8")
# ----------------------------------------------

# ----------------------------------------------
# switches from main file that are necessary here:
verbose = FALSE 
# load files to loop through for FR prep:
file_list = load_master_list(purpose = "financial") #This function is sourced from the _common folder in master script. 
file_list = file_list[data_source == 'funding_request',]
# ----------------------------------------------

# ----------------------------------------------
# loop through FR budget files 
for(i in 1:nrow(file_list)){
  # Set up file path 
  folder = "funding_requests"
  
  if (file_list[i, year(version_date) <= "2019"]){
    folder_cont = "fr_budgets_2017"
  } else if (file_list[i, (year(version_date) >= "2019" & year(version_date) <= '2021')]){
    folder_cont= "fr_budgets_2020"
  } else {
    stop('Something went wrong - version date is missing or incorrect')
  }
  
  grant_period = file_list$grant_period[i]
  
  file_dir = paste0(box, toupper(file_list$loc_name[i]), '/raw_data/', file_list$grant_status[i], "/", 
                    folder, '/', folder_cont, '/')
  
  args = list(file_dir, file_list$file_name[i], file_list$sheet_financial[i], file_list$start_date_financial[i], 
              file_list$period_financial[i], file_list$qtr_number_financial[i], file_list$language_financial[i])
  
  ### RUN THE PREP FUNCTION HERE ###
  tmpData = do.call(prep_fr_budgets, args)
  ###
  
  #Add indexing data
  append_cols = file_list[i, .(loc_name, disease, file_name, data_source, grant_period, grant_status, 
                               file_currency, file_iteration, budget_version, version_date, 
                               function_financial, start_date_financial, language_financial, 
                               period_financial, qtr_number_financial,
                               mod_framework_format, update_date)]
  
  stopifnot(nrow(append_cols)==1)
  
  tmpData = cbind(tmpData, append_cols)
  
  tmpData[, year := year(start_date)]
  tmpData[, file_start_date:=min(start_date), by='file_name']
  
  #Bind data together 
  if(i==1){
    prepped_frs = tmpData
  } else {
    prepped_frs = rbind(prepped_frs, tmpData, use.names=TRUE, fill = TRUE)
  }
  
  print(paste0(i, " ", file_list$data_source[i], " ", file_list$function_financial[i], " ", file_list$file_name[i])) ## if the code breaks, you know which file it broke on
}


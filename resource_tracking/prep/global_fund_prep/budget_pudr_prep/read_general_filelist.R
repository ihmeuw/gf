# ----------------------------------------------
# Emily Linebarger, based on code by Irena Chen
# Function to read in file list and apply prep functions
# ----------------------------------------------

# ----------------------------------------------
# TO-DO: 
#   - Write a general prep function for detailed budgets and summary budgets. 
# - sheet == 'Detailed Budget' and sheet == 'Budget summary GF'. 
# ----------------------------------------------

read_fileList = function(){
  
options(scipen=100)
document_prep <- paste0(code_dir, "global_fund_prep/")
  
# ----------------------------------------------
# Source prep functions
# ----------------------------------------------
source(paste0(document_prep, "prep_general_pudr.R"))

# ----------------------------------------------
# Subset file list (temporary step)
# ----------------------------------------------
#file_list <- readRDS("C:/Users/elineb/Desktop/all_limited_files.rds")

file_list = file_list[data_source == 'pudr']

mod_approach_sheet_names <- c('LFA Expenditure_7B', 'LFA AFR_7B', 'PR Expenditure_7A', 'RFA ALF_7B')
file_list = file_list[!sheet%in%mod_approach_sheet_names]

file_list$country = substring(file_list$grant, 1, 1)
file_list[country=='U', country:='uga']
file_list[country=='G', country:='gtm']
file_list[country=='C', country:='cod']

mod_approach_sheet_names <- c('LFA Expenditure_7B', 'LFA AFR_7B', 'PR Expenditure_7A', 'RFA ALF_7B')

for(i in 1:nrow(file_list)){ 
  master_file_dir = paste0("J:/Project/Evaluation/GF/resource_tracking/", file_list$country[i], "/grants/")
  
  folder = "budgets"
  folder = ifelse (file_list$data_source[i] == "pudr", "pudrs", folder)
  file_dir = paste0(master_file_dir, file_list$grant_status[i], "/", file_list$grant[i], "/", folder, "/")

  if(file_list$sheet[i]%in%mod_approach_sheet_names){
  tmpData <- prep_modular_approach_pudr(file_dir, file_list$file_name[i], as.character(file_list$sheet[i]), 
                                       file_list$start_date[i], file_list$disease[i], file_list$period[i], file_list$grant[i],
                                        file_list$primary_recipient[i], file_list$data_source[i])
  }
  
  tmpData$fileName = file_list$file_name[i]
  tmpData$grant_period = file_list$grant_period[i]
  tmpData$file_iteration = file_list$file_iteration[i]
  tmpData$lang <- file_list$language[i]
  tmpData$recipient <- NULL 
  tmpData$primary_recipient <- file_list$primary_recipient[i]
  tmpData$secondary_recipient <- file_list$secondary_recipient[i]
  tmpData$grant_status <- file_list$grant_status[i]
  tmpData$mod_framework_format = file_list$mod_framework_format[i]
  if(i==1){
    resource_database = tmpData 
  } 
  if(i>1){
    resource_database = rbind(resource_database, tmpData, use.names=TRUE, fill = TRUE)
  }

  print(paste0(i, " ", file_list$data_source[i], " ", file_list$grant[i])) ## if the code breaks, you know which file it broke on
}

return(resource_database)

} 

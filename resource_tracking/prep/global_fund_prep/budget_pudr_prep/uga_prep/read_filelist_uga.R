# ----------------------------------------------
# Emily Linebarger, based on code by Irena Chen
# Master code file for UGA data prep
# ----------------------------------------------

read_fileList = function(){
  
options(scipen=100)
  
# ----------------------------------------------
###### source the functions that we need 
# ----------------------------------------------
# source(paste0(document_prep, "prep_detailed_uga_budget.R"))
source(paste0(country_code_dir, "prep_summary_uga_budget.R"))
# source(paste0(document_prep, "prep_pudr_uga.R"))
source(paste0(gf_prep_code, "budget_pudr_prep/prep_general_detailed_budget.R"))
source(paste0(gf_prep_code, "budget_pudr_prep/prep_modular_approach_pudr.R"))


# ---------------------------------------------
########## Set up variables and load the prep files ########
# ---------------------------------------------
cashText <- " Cash Outflow" ##we'll need this to grab the right columns from the budgets 
loc_name <- 'uga' ##change this when we can get subnational data 
source <- 'gf'

# ---------------------------------------------
########## Run the for loop that preps data ########
# ---------------------------------------------

for(i in 1:nrow(file_list)){ 
  folder = "budgets"
  folder = ifelse (file_list$data_source[i] == "pudr", "pudrs", folder)
  file_dir = paste0(master_file_dir, file_list$grant_status[i], "/", file_list$grant[i], "/", folder, "/")

  if(file_list$function_type[i]=="detailed"){##most detailed level of budgets 
  tmpData <- prep_general_detailed_budget(file_dir, file_list$file_name[i], as.character(file_list$sheet[i]), 
                                       file_list$start_date[i], file_list$qtr_number[i], file_list$grant[i], 
                                        file_list$disease[i], file_list$period[i],file_list$data_source[i])
  } else if (file_list$function_type[i]=="summary"){ ##not much detail, only high level SDAs: 
    tmpData <- prep_summary_uga_budget(file_dir, file_list$file_name[i], as.character(file_list$sheet[i]), 
                                       file_list$start_date[i], file_list$qtr_number[i], 
                                       cashText, file_list$grant[i], 
                                       file_list$disease[i], file_list$period[i], file_list$primary_recipient[i], 
                                       file_list$data_source[i])
  ##LFA data cleaning: 
  } else if (file_list$function_type[i]=="pudr"){ ##has expenditure data 
    tmpData <- prep_modular_approach_pudr(file_dir, file_list$file_name[i], as.character(file_list$sheet[i]), 
                             file_list$start_date[i], file_list$disease[i], file_list$period[i], 
                             file_list$grant[i], file_list$primary_recipient[i],file_list$data_source[i])
  }
  tmpData$fileName = file_list$file_name[i]
  tmpData$grant_period = file_list$grant_period[i]
  tmpData$file_iteration = file_list$file_iteration[i]
  tmpData$lang <- file_list$language[i]
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

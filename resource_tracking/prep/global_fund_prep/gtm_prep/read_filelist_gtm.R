# ----------------------------------------------
# Emily Linebarger, based on code by Irena Chen
# Master code file for gtm data prep
# ----------------------------------------------

read_fileList = function(){
  
document_prep <- paste0(country_code_dir, "document_prep_functions/")

# ----------------------------------------------
# Source prep functions
# --------------------------------------------
source(paste0(document_prep, "prep_fpm_detailed_budget.R"))
source(paste0(document_prep, "prep_fpm_summary_budget.R"))
source(paste0(document_prep, "prep_fpm_other_budget.R"))
source(paste0(document_prep, "prep_fpm_other_detailed_budget.R"))
source(paste0(document_prep, "prep_gtm_pudr.R"))

# ----------------------------------------------
# For loop that preps data and aggregates it
# --------------------------------------------

for(i in 1:length(file_list$file_name)){
  folder = "budgets"
  folder = ifelse (file_list$data_source[i] == "fpm" , folder, "pudrs")
  file_dir = paste0(master_file_dir, file_list$grant_status[i], "/", file_list$grant[i], "/", folder, "/")
  
  if(file_list$function_type[i]=="detailed"){ ## fpm detailed budgets 
    tmpData <- prep_fpm_detailed_budget(file_dir, file_list$file_name[i], as.character(file_list$sheet[i]),
                                        ymd(file_list$start_date[i]), file_list$qtr_number[i], file_list$disease[i], file_list$period[i], 
                                        file_list$language[i], file_list$grant[i], file_list$primary_recipient[i])
    tmpData$disbursement <- 0 
  } else if (file_list$function_type[i]=="summary"){ ## only summary level data - no municipalities 
    tmpData <- prep_fpm_summary_budget(file_dir, file_list$file_name[i], as.character(file_list$sheet[i]),
                                       ymd(file_list$start_date[i]), file_list$qtr_number[i], file_list$disease[i], file_list$period[i], 
                                       file_list$grant[i], file_list$primary_recipient[i], file_list$language[i])
    tmpData$disbursement <- 0
  } else if (file_list$function_type[i]=="detailed_other"){ ## there's an older version of detailed fpm budgets
    tmpData <- prep_other_detailed_budget(file_dir, file_list$file_name[i], as.character(file_list$sheet[i]),
                                        ymd(file_list$start_date[i]), file_list$qtr_number[i], file_list$disease[i], file_list$period[i], 
                                        file_list$language[i], file_list$grant[i])
    tmpData$disbursement <- 0 
  } else if (file_list$function_type[i]=="pudr"){ 
    tmpData <- prep_gtm_pudr(file_dir, file_list$file_name[i], as.character(file_list$sheet[i]),
                                          ymd(file_list$start_date[i]), file_list$qtr_number[i], file_list$disease[i], file_list$period[i], 
                                          file_list$grant[i], file_list$data_source[i], file_list$loc_id[i], file_list$language[i])

  } else if (file_list$function_type[i]=="other"){
    tmpData <- prep_other_budget(file_dir, file_list$file_name[i], as.character(file_list$sheet[i]),
                                          ymd(file_list$start_date[i]), file_list$qtr_number[i], file_list$disease[i], file_list$period[i], 
                                          file_list$language[i], file_list$grant[i])
    tmpData$disbursement <- 0 
  }
  tmpData$data_source <- file_list$data_source[i]
  tmpData$fileName <- file_list$file_name[i]
  tmpData$grant_period <- file_list$grant_period[i]
  tmpData$recipient <- NULL 
  tmpData$primary_recipient <- file_list$primary_recipient[i]
  tmpData$secondary_recipient <- file_list$secondary_recipient[i]
  tmpData$file_iteration = file_list$file_iteration[i]
  tmpData$year <- year(tmpData$start_date[i])
  if(i==1){
    resource_database = tmpData
  }
  if(i>1){
    resource_database = rbind(resource_database, tmpData, use.names=TRUE)
  }

  print(paste0(i, " ", file_list$function_type[i], " ", file_list$grant[i])) ## if the code breaks, you know which file it broke on
}

  return(resource_database)
}






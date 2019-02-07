# ----------------------------------------------
# Emily Linebarger, based on code by Irena Chen
# Master code file for DRC data prep
# ----------------------------------------------

read_fileList = function(){
  
#-----------------------------------------
# Source document prep functions
# ----------------------------------------

document_prep <- paste0(country_code_dir, "document_prep_functions/")

source(paste0(document_prep, "prep_detailed_budget.R"))
source(paste0(document_prep, "prep_summary_budget.R"))
source(paste0(document_prep, "prep_cod_pudr.R"))
source(paste0(document_prep, "prep_old_module_budget.R"))
source(paste0(document_prep, "prep_cod_rejected.R"))
source(paste0(document_prep, "prep_old_detailed_budget.R"))

#-----------------------------------------
# Read in raw files, and rbind them together
# ----------------------------------------
for(i in 1:nrow(file_list)){
  folder = "budgets"
  folder = ifelse (file_list$data_source[i] == "pudr", "pudrs", folder)
  version = ifelse(file_list$file_iteration[i] == "initial", "iterations", "")
  file_dir = paste0(master_file_dir, file_list$grant_status[i], "/", file_list$grant[i], "/", folder, "/")
  if (version != ""){
    file_dir = paste0(file_dir, version, "/")
  }
  
  inFile = paste0(file_dir, file_list$file_name[i])

  if(file_list$function_type[i]=="summary"){
    implementer <- "CAGF"
    tmpData <- prep_summary_budget(file_dir, as.character(file_list$file_name[i]),
                                  file_list$sheet[i], file_list$start_date[i], file_list$qtr_number[i], 
                                  file_list$disease[i], file_list$loc_id[i], file_list$period[i]
                                  , file_list$grant[i], implementer, file_list$source[i], file_list$language[i])
   } else if (file_list$function_type[i]=="detailed"){
    tmpData <- prep_detailed_budget(file_dir, file_list$file_name[i], file_list$sheet[i], file_list$start_date[i], file_list$qtr_number[i],
                                        file_list$disease[i], file_list$period[i],  file_list$language[i], file_list$grant[i], country, file_list$data_source[i])
  } else if(file_list$function_type[i]=="module"){
    tmpData <- prep_old_module_budget(file_dir, as.character(file_list$file_name[i]),
                                   file_list$sheet[i], file_list$start_date[i], file_list$qtr_number[i], 
                                   file_list$disease[i], file_list$loc_id[i], file_list$period[i]
                                   , file_list$grant[i], implementer, file_list$source[i], file_list$language[i])
  } else if(file_list$function_type[i]=="rejected"){
    tmpData <- prep_cod_rejected(paste0(file_dir, file_list$file_name[i]))
    
  }  else if (file_list$function_type[i]=="pudr"){ ##has expenditure data 
    tmpData <- prep_pudr_cod(file_dir, file_list$file_name[i], as.character(file_list$sheet[i]), 
                             file_list$start_date[i], file_list$disease[i], file_list$period[i], 
                             file_list$grant[i], file_list$sr[i],file_list$data_source[i], file_list$language[i], country)
  } else if (file_list$function_type[i]=="old_detailed"){
    tmpData <- prep_old_detailed_budget(file_dir, file_list$file_name[i], file_list$sheet[i], file_list$start_date[i], file_list$qtr_number[i],
                                    file_list$disease[i], file_list$period[i],  file_list$language[i], file_list$grant[i], country, file_list$source[i],
                                    file_list$pr[i])
  }
  tmpData$data_source <- file_list$data_source[i]
  tmpData$grant_period = file_list$grant_period[i]
  tmpData$primary_recipient <- file_list$primary_recipient[i]
  tmpData$secondary_recipient <- file_list$secondary_recipient[i]
  tmpData$recipient <- NULL
  tmpData$file_iteration <- file_list$file_iteration[i]
  tmpData$fileName <- file_list$file_name[i]
  #tmpData$file_save_date <- file_list$file_save_date[i]
  tmpData$grant_status <- file_list$grant_status[i]
  if(i==1){
    resource_database = tmpData
  } 
  if(i>1){
    resource_database = rbind(resource_database, tmpData, use.names=TRUE)
  }
  
  print(paste0(i, " ", file_list$data_source[i], " ", file_list$grant[i])) ## if the code breaks, you know which file it broke on
}

#Fix year format (This should be done back in prep functions when you get the chance) #EKL
setDT(resource_database)
resource_database$year = as.character(resource_database$year)
resource_database[nchar(year) == 2, year:= paste0("20", year)]
resource_database$year = as.numeric(resource_database$year)

resource_database[, start_date:=as.character(start_date)]
resource_database$year_prefix <- substring(resource_database$start_date, 1, 2)
resource_database$year_suffix <- substring(resource_database$start_date, 3, nchar(resource_database$start_date))
resource_database[year_prefix == "00", start_date:=paste0("20", year_suffix)]
resource_database[, start_date:=as.Date(start_date, format="%Y-%m-%d")]
resource_database$year_prefix <- NULL
resource_database$year_suffix <- NULL


return(resource_database)

}


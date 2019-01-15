# ----------------------------------------------
# Emily Linebarger, based on code by Irena Chen
# Master code file for UGA data prep
# ----------------------------------------------

read_fileList = function(){
  
options(scipen=100)
document_prep <- paste0(country_code_dir, "document_prep_functions/")
  
# ----------------------------------------------
###### source the functions that we need 
# ----------------------------------------------
source(paste0(document_prep, "prep_detailed_uga_budget.R"))
source(paste0(document_prep, "prep_summary_uga_budget.R"))
source(paste0(document_prep, "prep_pudr_uga.R"))

# ---------------------------------------------
########## Set up variables and load the prep files ########
# ---------------------------------------------
cashText <- " Cash Outflow" ##we'll need this to grab the right columns from the budgets 
loc_name <- 'uga' ##change this when we can get subnational data 
source <- 'gf'

# ---------------------------------------------
########## Run the for loop that preps data ########
# ---------------------------------------------

for(i in 1:nrow(uga_error_files)){ 
  folder = "budgets"
  #folder = ifelse (file_list$data_source[i] == "pudr", "pudrs", folder)
  #file_dir = paste0(master_file_dir, file_list$grant_status[i], "/", file_list$grant[i], "/", folder, "/")
  folder = ifelse (uga_error_files$data_source[i] == "pudr", "pudrs", folder)
  file_dir = paste0(master_file_dir, uga_error_files$grant_status[i], "/", uga_error_files$grant[i], "/", folder, "/")

  if(file_list$function_type[i]=="detailed"){##most detailed level of budgets 
    tmpData <- prep_detailed_uga_budget(file_dir, file_list$file_name[i], as.character(file_list$sheet[i]), 
                                       file_list$start_date[i], file_list$qtr_number[i],
                                       cashText, file_list$grant[i], 
                                        file_list$disease[i], file_list$period[i],file_list$data_source[i])
  } else if (file_list$function_type[i]=="summary"){ ##not much detail, only high level SDAs: 
    tmpData <- prep_summary_uga_budget(file_dir, file_list$file_name[i], as.character(file_list$sheet[i]), 
                                       file_list$start_date[i], file_list$qtr_number[i], 
                                       cashText, file_list$grant[i], 
                                       file_list$disease[i], file_list$period[i], file_list$primary_recipient[i], 
                                       file_list$data_source[i])
    tmpData$disbursement <- 0 
  ##LFA data cleaning: 
  } else if (file_list$function_type[i]=="pudr"){ ##has expenditure data 
    tmpData <- prep_pudr_uga(file_dir, file_list$file_name[i], as.character(file_list$sheet[i]), 
                             file_list$start_date[i], file_list$disease[i], file_list$period[i], 
                             file_list$grant[i], file_list$primary_recipient[i],file_list$data_source[i])
  }
  tmpData$fileName = file_list$file_name[i]
  tmpData$grant_period = file_list$grant_period[i]
  tmpData$file_iteration = file_list$file_iteration[i]
  tmpData$lang <- file_list$language[i]
  tmpData$recipient <- NULL 
  tmpData$primary_recipient <- file_list$primary_recipient[i]
  tmpData$secondary_recipient <- file_list$secondary_recipient[i]
  tmpData$grant_status <- file_list$grant_status[i]
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

# resource_database$century <- substring(resource_database$start_date, 1, 2)
# unique(resource_database$century)
# resource_database$century <- NULL

#For verifying 
# budget_check <- copy(tmpData)
# budget_check$budget <- gsub("[0-9.]", "", budget_check$budget)
# unique(budget_check$budget)
# 
# check_files <- budget_check[budget != ""]
# stopifnot(nrow(check_files)==0)

#Emily ask David what we want to do with this code. We should have the same process for each country. 
# ---------------------------------------------
########## We'll split the TB/HIV grants as follows: ########
########## If the module explicitly says TB, then assign as TB ########
########## Otherwise, default it to HIV ########
# ---------------------------------------------
## split hiv/tb into hiv or tb (for module/intervention mapping purposes): 
# get_hivtb_split <- function(disease,module){
#   x <- disease
#  if(disease=="hiv/tb"){
#    if(grepl(paste(c("tb", "tuber"), collapse="|"), module)){ 
#     x <- "tb"
#   } else { ##otherwise, map it to HIV
#     x <- "hiv"
#   }
#  }
# return(x)
# }
# 
# cleaned_database$disease <- mapply(get_hivtb_split, cleaned_database$disease, cleaned_database$module)
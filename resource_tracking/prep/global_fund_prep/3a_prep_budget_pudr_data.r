# ----------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Prep country-level budgets and PUDRs
# DATE: Last updated November 2018. 
# ----------------------------------------------

#-----------------------------------------------
# Prep raw data 
#-----------------------------------------------

#-------------------------
#To do: 
# - add a check to make sure we have all files from file list in final data
#--------------------------

#----------------------------------------------------
# 1. Rerun prep functions, or read in prepped files
#----------------------------------------------------
if (rerun_filelist == TRUE & limit_filelist == FALSE){ #Save the prepped files, but only if all are run
  
  pudr_mod_approach_sheets <- c('LFA Expenditure_7B', 'LFA AFR_7B', 'PR Expenditure_7A', 'RFA ALF_7B')
  general_detailed_budget_sheets <- c('Detailed Budget', 'Detailed budget', 'DetailedBudget', 'Recomm_Detailed Budget', '1.Detailed Budget')
  
  for(i in 1:nrow(file_list)){
    folder = "budgets"
    folder = ifelse (file_list$data_source[i] == "pudr", "pudrs", folder)
    version = ifelse(file_list$file_iteration[i] == "initial", "iterations", "")
    file_dir = paste0(master_file_dir, file_list$grant_status[i], "/", file_list$grant[i], "/", folder, "/")
    if (version != ""){
      file_dir = paste0(file_dir, version, "/")
    }
    
    inFile = paste0(file_dir, file_list$file_name[i])
    args = list(file_dir, file_list$file_name[i], file_list$sheet[i], file_list$start_date[i], file_list$period[i])
    
    if(file_list$function_type[i] == 'detailed' & file_list$sheet[i]%in%general_detailed_budget_sheets){
      args[length(args)+1] = file_list$qtr_number[i]
      args[length(args)+1] = file_list$language[i]
      tmpData = do.call(prep_general_detailed_budget, args)
      
    } else if (file_list$function_type[i] == 'pudr' & file_list$sheet[i]%in%pudr_mod_approach_sheets){
      tmpData = do.call(prep_modular_approach_pudr, args)
      
    } else if (file_list$function_type[i] == 'summary' & file_list$loc_name[i] == 'cod'){
      args[length(args)+1] = file_list$qtr_number[i]
      tmpData = do.call(prep_summary_budget_cod, args)
      
    } else {
      print(paste0("File not being processed: ", file_list$file_name[i]))
    }
    
    #Add indexing data
    append_cols = file_list[i, .(data_source, grant_period, primary_recipient, secondary_recipient, file_name, grant_status, disease, grant, 
                                 mod_framework_format, file_iteration, language)]
    for (col in names(append_cols)){
      tmpData[, (col):=append_cols[, get(col)]]
    }  
    tmpData$year <- year(tmpData$start_date)
    
    #Bind data together 
    if(i==1){
      resource_database = tmpData
    } 
    if(i>1){
      resource_database = rbind(resource_database, tmpData, use.names=TRUE, fill = TRUE)
    }
    print(paste0(i, " ", file_list$data_source[i], " ", file_list$grant[i])) ## if the code breaks, you know which file it broke on
  }
  
  saveRDS(resource_database, paste0(j, "/Project/Evaluation/GF/resource_tracking/", country, "/prepped/raw_bound_gf_files.RDS"))
  
  
} else {
  resource_database <- readRDS(paste0(j, "/Project/Evaluation/GF/resource_tracking/", country, "/prepped/raw_bound_gf_files.RDS"))
}

#------------------------------------------------------------------
# 2. Run some checks to make sure this data was prepped correctly. 
#-----------------------------------------------------------------
original_db <- copy(resource_database)
#Make sure all budget data pulled is actually numeric- this is an easy check to see if prep functions are working correctly. 
verify_numeric_budget = resource_database[, .(budget=gsub("[[:digit:]]", "", budget))]
verify_numeric_budget = verify_numeric_budget[, .(budget=gsub("[[:punct:]]", "", budget))]
verify_numeric_budget = verify_numeric_budget[!is.na(budget) & budget != ""]
stopifnot(nrow(verify_numeric_budget)==0)

# Make sure there are no overlapping quarters for the same grant (duplicate files. )
fpm_overlap <- duplicated(resource_database[data_source == "fpm" & file_iteration == "final", .(grant, start_date)])
pudr_overlap <- duplicated(resource_database[data_source == "pudr" & file_iteration == "final", .(grant, start_date)])
stopifnot(nrow(fpm_overlap)==0 & nrow(pudr_overlap)==0)

rm(fpm_overlap, pudr_overlap)

#Make sure all budget and expenditure variables are numeric. 
resource_database$budget <- as.numeric(resource_database$budget)
resource_database$expenditure <- as.numeric(resource_database$expenditure)
resource_database$disbursement <- as.numeric(resource_database$disbursement)

#Add files here that had a sum total for 0 in raw file. 
verified_0_budget <- c('UGD-708-G08-M_PUDR 30Nov2011.xls', 'UGD-708-G08-M_PUDR_30June2012.xls')
#Add PUDRs here that did not report any expenditure. 
verified_0_expenditure <- c('GTM-T-MSPAS_Progress Report_31Dec2017 LFA REVIEW.xlsx', 'UGA-C-TASO_PU_PEJune2017_LFA_30Nov17.xlsx', 'UGA-M-TASO_PU_PEJune2017_LFA_30Nov17.xlsx', 'UGA-S-TASO_PU_PEJune2017_LFA_30Nov17.xlsx')

#Make sure that no files have a total sum of 0; this would indicate an error in the prep code. 
check_0_budgets <- resource_database[, .(budget = sum(budget, na.rm = TRUE)), by=.(file_name)]
check_0_budgets = check_0_budgets[budget == 0 & !file_name%in%verified_0_budget]
check_0_expenditure <- resource_database[data_source == 'pudr', .(expenditure = sum(expenditure, na.rm = TRUE)), by=.(file_name)]
check_0_expenditure <- check_0_expenditure[expenditure == 0 & !file_name%in%verified_0_expenditure]
stopifnot(nrow(check_0_budgets)==0 & nrow(check_0_expenditure)==0)

#Hacky fix - this should be fixed earlier in the prep functions, but remove anything at this point that has NAs for module, intervention, and budget OR expenditure. 
resource_database[module=='all', module:='unspecified']
resource_database[tolower(intervention)=='all', intervention:='unspecified']
resource_database[is.na(module), module:='unspecified'] 
resource_database[is.na(intervention), intervention:='unspecified']
resource_database = resource_database[!(module=='unspecified' & intervention=='unspecified' & budget == 0 & expenditure == 0)]

#check for duplicates, and sum their values if they exist:
dups<-resource_database[duplicated(resource_database) | duplicated(resource_database, fromLast=TRUE)]
print(paste0(nrow(dups), " duplicates found in database; values will be summed"))
byVars = names(resource_database)[!names(resource_database)%in%c('budget', 'expenditure', 'disbursement')]
resource_database= resource_database[, list(budget=sum(na.omit(budget)) ,expenditure=sum(na.omit(expenditure)), disbursement=sum(na.omit(disbursement))), by=byVars]

#Make sure you have all the files here that you started with in your filelist. 
# rt_files <- unique(resource_database$file_name)
# stopifnot(length(unique(file_list$file_name)) == length(rt_files))
# stopifnot(sort(rt_files) == sort(unique(file_list$file_name)))

#Only keep post-2016 files - temporary fix to make sure most recent data is accurate! 
post_mf_files = file_list[mod_framework_format == TRUE]
resource_database = resource_database[file_name%in%post_mf_files$file_name]

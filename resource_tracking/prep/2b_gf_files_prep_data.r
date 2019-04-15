# ----------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Prep country-level budgets and PUDRs
# DATE: Last updated March 2019 
# ----------------------------------------------

#-------------------------
#To do: 
# Make sure that final files in file list don't have overlapping quarters, NOT just duplicate start dates. 
#--------------------------

#----------------------------------------------------
# Read in file list 
#----------------------------------------------------
if (prep_files == TRUE){
  file_list = fread(paste0(master_file_dir, country, "_budget_filelist.csv"), stringsAsFactors = FALSE, encoding="Latin-1")
  file_list$start_date <- as.Date(file_list$start_date, format = "%m/%d/%Y")
  file_list = file_list[, -c('notes')]
  
  #Validate file list 
  desired_cols <- c("file_name", "function_type", "sheet", "disease", "loc_id", "data_source", "period", "qtr_number", "grant", "primary_recipient", 
                    "secondary_recipient", "language", "grant_period", "grant_status", "start_date", "file_iteration", "geography_detail", "loc_name", "mod_framework_format")
  stopifnot(colnames(file_list) %in% desired_cols)
  
  stopifnot(sort(unique(file_list$data_source)) == c("fpm", "pudr"))
  stopifnot(sort(unique(file_list$file_iteration)) == c("final", "initial"))
  
  #Prioritize GOS data where we have it 
  file_list = prioritize_gos(file_list)
  
  #Make sure you don't have the same tart date for the same grant (quick check; it would be better )
  file_list[file_iteration=='final', date_dup:=seq(0, 10, by=1), by=c('grant', 'start_date', 'data_source')]
  
  if ( nrow(file_list[date_dup>0])!=0){
    print(file_list[date_dup > 0, .(file_name, file_iteration, grant, grant_period, start_date)][order(grant, grant_period, start_date)])
    print("There are duplicates in final files - review file list.")
  }

}

#----------------------------------------------------
# 1. Rerun prep functions, or read in prepped files
#----------------------------------------------------
if (rerun_filelist == TRUE){ #Save the prepped files, but only if all are run
  
  pudr_mod_approach_sheets <- c('LFA Expenditure_7B', 'LFA AFR_7B', 'PR Expenditure_7A', 'RFA ALF_7B')
  general_detailed_budget_sheets <- c('Detailed Budget', 'Detailed budget', 'DetailedBudget', 'Recomm_Detailed Budget', '1.Detailed Budget', "Detailed Budget Revise")
  
  budget_cols = c("activity_description", "budget", "cost_category", "intervention", "module", "start_date") #These are the only columns that should be returned from a budget function. 
  pudr_cols = c("budget", "expenditure", "intervention", "module", "period", "start_date") #These are the only columns that should be returned from a pudr function. 
  
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
    
    if(file_list$function_type[i] == 'detailed' & file_list$sheet[i]%in%general_detailed_budget_sheets){ #Prep standardized detailed budgets. 
      args[length(args)+1] = file_list$qtr_number[i]
      args[length(args)+1] = file_list$language[i]
      tmpData = do.call(prep_general_detailed_budget, args)
      
      stopifnot(sort(names(tmpData)) == budget_cols)
      
    } else if (file_list$function_type[i] == 'pudr' & file_list$sheet[i]%in%pudr_mod_approach_sheets){ #Prep standardized 'modular approach' PUDRs. 
      tmpData = do.call(prep_modular_approach_pudr, args)
      
      stopifnot(sort(names(tmpData)) == pudr_cols)
      
    } else if (file_list$function_type[i]=='pudr' & file_list$sheet[i]%in%c('INTEGRACION')){ #Prep more general Guatemala PUDRs. 
      args = list(file_dir, file_list$file_name[i], file_list$sheet[i], file_list$start_date[i], file_list$qtr_number[i], file_list$disease[i], file_list$period[i],
                  file_list$grant[i], file_list$source[i], file_list$loc_name[i], file_list$language[i])
      tmpData = do.call(prep_pudr_gtm, args)
      
      stopifnot(sort(names(tmpData)) == pudr_cols)
      
    }  else if (file_list$function_type[i] == 'summary' & file_list$loc_name[i] == 'cod'){ #Prep summary budgets from DRC. 
      args[length(args)+1] = file_list$qtr_number[i]
      tmpData = do.call(prep_summary_budget_cod, args)
      
      stopifnot(sort(names(tmpData)) == budget_cols)
      
    } else if (file_list$function_type[i] == 'summary' & file_list$loc_name[i]=='gtm') {
      args = list(file_dir, file_list$file_name[i], file_list$sheet[i], file_list$start_date[i], file_list$qtr_number[i], file_list$disease[i], file_list$period[i],
                    file_list$grant[i], file_list$primary_recipient[i], file_list$language[i])
      tmpData = do.call(prep_summary_budget_gtm, args)
      
      stopifnot(sort(names(tmpData)) == budget_cols)
    } else {
      print(paste0("File not being processed: ", file_list$file_name[i]))
      print(paste0("Check logic conditions. This file has the function_type: ", file_list$function_type[i],
            " and the sheet name: ", file_list$sheet[i]))
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
    print(paste0(i, " ", file_list$data_source[i], " ", file_list$function_type[i], " ", file_list$grant[i])) ## if the code breaks, you know which file it broke on
  }
  
  saveRDS(resource_database, paste0(export_dir, "raw_bound_gf_files.RDS"))
  
  
} else {
  resource_database <- readRDS(paste0(dir, "_gf_files_gos/", country, "/prepped_data/raw_bound_gf_files.RDS"))
  resource_database = resource_database[file_name%in%file_list$file_name]
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
verified_0_expenditure <- c('GTM-T-MSPAS_Progress Report_31Dec2017 LFA REVIEW.xlsx', 'UGA-C-TASO_PU_PEJune2017_LFA_30Nov17.xlsx', 'UGA-M-TASO_PU_PEJune2017_LFA_30Nov17.xlsx', 'UGA-S-TASO_PU_PEJune2017_LFA_30Nov17.xlsx', 
                            "GTM-T-MSPAS_Progress Report jul _31Dec2018_v2  rev LFA.xlsx", "GTM-H-HIVOS_Progress Report_31Dec2018_v1.xlsx")

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
rt_files <- unique(resource_database$file_name)
stopifnot(length(unique(file_list$file_name)) == length(rt_files))
stopifnot(sort(rt_files) == sort(unique(file_list$file_name)))

#Add in a variable for the disease of the file before you start mapping process. 
resource_database[, disease_grant:=strsplit(grant, "-")]
resource_database[, disease_grant:=sapply(disease_grant, "[", 2 )]
unique(resource_database$disease_grant) #Visual check that these all make sense. 

resource_database[disease_grant=='C', disease_grant:='hiv/tb']
resource_database[disease_grant=='H', disease_grant:='hiv']
resource_database[disease_grant=='T', disease_grant:='tb']
resource_database[disease_grant=='S' | disease_grant=='R', disease_grant:='rssh']
resource_database[disease_grant=='M', disease_grant:='malaria']


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
# - add a check to make sure total sum for a file isn't 'na' after we convert to numeric
#--------------------------

if (rerun_filelist == TRUE & limit_filelist == FALSE){ #Save the prepped files, but only if all are run
  # Read in file list 
  source(paste0(country_code_dir, "read_filelist_", country, ".R"))
  resource_database <- read_fileList()
  
  saveRDS(resource_database, paste0(j, "/Project/Evaluation/GF/resource_tracking/", country, "/prepped/raw_bound_gf_files.RDS"))
} else {
  resource_database <- readRDS(paste0(j, "/Project/Evaluation/GF/resource_tracking/", country, "/prepped/raw_bound_gf_files.RDS"))
}

original_db <- copy(resource_database)
#Make sure all budget data pulled is actually numeric- this is an easy check to see if prep functions are working correctly. 
verify_numeric_budget = resource_database[, .(budget=gsub("[[:digit:]]", "", budget))]
verify_numeric_budget = verify_numeric_budget[, .(budget=gsub("[[:punct:]]", "", budget))]
verify_numeric_budget = verify_numeric_budget[!is.na(budget) & budget != ""]
stopifnot(nrow(verify_numeric_budget)==0)

# Make sure there are no overlapping quarters for the same grant (duplicate files. )
fpm_overlap <- duplicated(resource_database[data_source == "fpm" & file_iteration == "final", .(grant_number, start_date)])
pudr_overlap <- duplicated(resource_database[data_source == "pudr" & file_iteration == "final", .(grant_number, start_date)])
stopifnot(nrow(fpm_overlap)==0 & nrow(pudr_overlap)==0)

rm(fpm_overlap, pudr_overlap)

#Make sure all budget and expenditure variables are numeric. 
resource_database$budget <- as.numeric(resource_database$budget)
resource_database$expenditure <- as.numeric(resource_database$expenditure)
resource_database$disbursement <- as.numeric(resource_database$disbursement)

#Add files here that had a sum total for 0 in raw file. 
verified_0_budget <- c('UGD-708-G08-M_PUDR 30Nov2011.xls', 'UGD-708-G08-M_PUDR_30June2012.xls')
#Add PUDRs here that did not report any expenditure. 
verified_0_expenditure <- c('GTM-T-MSPAS_Progress Report_31Dec2017 LFA REVIEW.xlsx')

#Make sure that no files have a total sum of 0; this would indicate an error in the prep code. 
check_0_budgets <- resource_database[, .(budget = sum(budget, na.rm = TRUE)), by=.(fileName)]
check_0_budgets = check_0_budgets[budget == 0 & !fileName%in%verified_0_budget]
check_0_expenditure <- resource_database[data_source == 'pudr', .(expenditure = sum(expenditure, na.rm = TRUE)), by=.(fileName)]
check_0_expenditure <- check_0_expenditure[expenditure == 0 & !fileName%in%verified_0_expenditure]
stopifnot(nrow(check_0_budgets)==0 & nrow(check_0_expenditure)==0)

#check for duplicates, and sum their values if they exist:
dups<-resource_database[duplicated(resource_database) | duplicated(resource_database, fromLast=TRUE)]
print(paste0(nrow(dups), " duplicates found in database; values will be summed"))
byVars = names(resource_database)[!names(resource_database)%in%c('budget', 'expenditure')]
resource_database= resource_database[, list(budget=sum(na.omit(budget)) ,expenditure=sum(na.omit(expenditure))), by=byVars]

#Hacky fix - this should be fixed earlier in the prep functions, but remove anything at this point that has NAs for module, intervention, and budget OR expenditure. 
resource_database = resource_database[!(is.na(module) & is.na(intervention) & (budget == 0 | expenditure == 0))]

#Make sure you have all the files here that you started with in your filelist. 
# rt_files <- unique(resource_database$fileName)
# stopifnot(length(unique(file_list$file_name)) == length(rt_files))
# stopifnot(sort(rt_files) == sort(unique(file_list$file_name)))

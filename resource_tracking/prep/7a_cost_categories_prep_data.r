# ----------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Preps cost category data table from Global Fund PUDRs.
# DATE: Last updated November 2019. 
# 
# The current working directory should be the root of this repository, 
# and this file should be sourced from '1_master_file.r' in this folder.
# ----------------------------------------------

#----------------------------------------------------
# Read in file list 
#----------------------------------------------------

file_list = load_master_list(purpose = "financial") #This function is sourced from the _common folder in master script. 
file_list = file_list[loc_name%in%countries] #List of countries to run is set in master file

# Subset down to just PUDRs where we know we'll have cost category data 
pudr_mod_approach_sheet_financials <- c('LFA Expenditure_7B', 'LFA AFR_7B', 'PR Expenditure_7A', 'RFA ALF_7B', 'ALF RFR_7')
file_list = file_list[function_financial=="pudr" & sheet_financial%in%pudr_mod_approach_sheet_financials]

#Make sure you don't have the same start date for the same grant (quick check; it would be better )
file_list[file_iteration=='final', date_dup:=sequence(.N), by=c('grant', 'sheet_financial', 'start_date_financial', 'data_source', 'pudr_semester_financial')] #EMILY NEED TO RETHINK THIS. 
file_list[, date_dup:=date_dup-1]#This indexes at one, so you need to decrement it

if ( nrow(file_list[date_dup>0])!=0){
  print(file_list[date_dup > 0, .(file_name, file_iteration, grant, grant_period, start_date_financial)][order(grant, grant_period, start_date_financial)])
  print("There are duplicates in final files - review file list.")
}

file_list[data_source=="pudr" & file_iteration=="final", pudr_dup:=sequence(.N), by=c('grant', 'grant_period', 'pudr_semester_financial')]
file_list[, pudr_dup:=pudr_dup-1] #This variable indexes at 1.
if (nrow(file_list[pudr_dup>0 & !is.na(pudr_dup)])>0){
  print(file_list[pudr_dup>0 & !is.na(pudr_dup)])
  stop("There are duplicates in PUDRs between semesters - review file list.")
}
#Fix variable formats
file_list[, period_financial:=as.numeric(period_financial)]
file_list[, qtr_number_financial:=as.numeric(qtr_number_financial)]

# Just process final PUDRs for this list. 
file_list = file_list[file_iteration=="final"]

# For now, just process 2018-2020 files to get this code running. EL 11/20/2019 
file_list = file_list[grant_period=="2018-2020"]

#----------------------------------------------------
# 1. Rerun prep functions, or read in prepped files
#----------------------------------------------------
if (rerun_filelist) {
  all_cost_categories = data.table() 
  for (country in countries){
    master_file_dir = ifelse(Sys.info()[1]=='Windows', paste0(box, toupper(country), "/raw_data/"), 
                             paste0(dir, "_gf_files_gos/", country, "/raw_data/"))
    export_dir = ifelse(Sys.info()[1]=="Windows", paste0(box, country, "/prepped_data/"),
                        paste0(dir, "_gf_files_gos/", country, "/prepped_data/"))
    
    pudr_mod_approach_sheet_financials <- c('LFA Expenditure_7B', 'LFA AFR_7B', 'PR Expenditure_7A', 'RFA ALF_7B', 'ALF RFR_7')
    pudr_cols = c("budget", "expenditure", "cumulative_budget", "cumulative_expenditure", "cost_category", "quarter", "start_date", "year") #These are the only columns that should be returned from a pudr function. 
    
    file_list_subset = file_list[loc_name==country]
    for(i in 1:nrow(file_list_subset)){
      # Set up file path 
      folder = "budgets"
      folder = ifelse (file_list_subset$data_source[i] == "pudr", "pudrs", folder)
      if (file_list_subset$file_iteration[i]=="initial"){
        version = "iterations"
      } else if (file_list_subset$file_iteration[i]=="revision"){
        version= "revisions"
      } else {
        version = ""
      }
      grant_period = file_list_subset$grant_period[i]
      
      file_dir = paste0(master_file_dir, file_list_subset$grant_status[i], "/", file_list_subset$grant[i], "/", grant_period, "/", folder, "/")
      if (version != ""){
        file_dir = paste0(file_dir, version, "/")
      }
      
      # EXTRACT THE DATA 
      args = list(file_dir, file_list_subset$file_name[i], file_list_subset$sheet_financial[i], file_list_subset$start_date_financial[i], 
                  file_list_subset$period_financial[i], file_list_subset$qtr_number_financial[i])
      tmpData = do.call(prep_cost_category, args)
      stopifnot(pudr_cols%in%names(tmpData))
  
      #Add indexing data
      append_cols = file_list_subset[i, .(data_source, grant_period, primary_recipient, file_name, grant_status, disease, grant, 
                                   mod_framework_format, file_iteration, language_financial, file_currency, pudr_semester_financial, period_financial, update_date)]
      for (col in names(append_cols)){
        tmpData[, (col):=append_cols[, get(col)]]
      }  
      tmpData$year <- year(tmpData$start_date)
      tmpData[, file_start_date:=min(start_date), by='file_name']
      tmpData$loc_name <- country
      
      #Bind data together 
      if(i==1){
        resource_database = tmpData
      } else {
        resource_database = rbind(resource_database, tmpData, use.names=TRUE, fill = TRUE)
      }
      print(paste0(i, " ", file_list_subset$data_source[i], " ", file_list_subset$function_financial[i], " ", file_list_subset$grant[i])) ## if the code breaks, you know which file it broke on
    }
    saveRDS(resource_database, paste0(export_dir, "raw_bound_cost_categories.RDS"))
    saveRDS(resource_database, paste0(export_dir, "archive/raw_bound_cost_categories_", Sys.Date(), ".RDS"))
    
    all_cost_categories = rbind(all_cost_categories, resource_database, fill=T)
  } 
  
  #If you don't have lfa_exp_adjustment in any of the files for this country, add it as NA so checks later will work. 
  if (!'lfa_exp_adjustment'%in%names(all_cost_categories)){
    all_cost_categories[, lfa_exp_adjustment:=NA]
  }
} else {
  cod = readRDS(paste0(box, "COD/prepped_data/raw_bound_cost_categories.RDS")) 
  gtm = readRDS(paste0(box, "GTM/prepped_data/raw_bound_cost_categories.RDS")) 
  sen = readRDS(paste0(box, "SEN/prepped_data/raw_bound_cost_categories.RDS")) 
  uga = readRDS(paste0(box, "UGA/prepped_data/raw_bound_cost_categories.RDS")) 
  
  all_cost_categories = rbindlist(list(cod, gtm, sen, uga))
}

# If you didn't re-run all countries, bind on their raw data.
if (rerun_filelist) { 
  all_countries = c('cod', 'gtm', 'sen', 'uga') 
  not_run = all_countries[!all_countries%in%countries]
  if (length(not_run)>0){
    for (country in not_run){
      add_data = readRDS(paste0(box, toupper(country), "/prepped_data/raw_bound_cost_categories.RDS"))
      all_cost_categories = rbind(all_cost_categories, add_data)
    }
  }
} 
#------------------------------------------------------------------
# 2. Run some checks to make sure this data was prepped correctly. 
#-----------------------------------------------------------------
original_db <- copy(all_cost_categories)
#Make sure all budget data pulled is actually numeric- this is an easy check to see if prep functions are working correctly. 
verify_numeric_budget = all_cost_categories[, .(budget=gsub("[[:digit:]]", "", budget))]
verify_numeric_budget = verify_numeric_budget[, .(budget=gsub("[[:punct:]]", "", budget))]
verify_numeric_budget = verify_numeric_budget[!is.na(budget) & budget != ""]
stopifnot(nrow(verify_numeric_budget)==0)

# Make sure there are no overlapping quarters for the same grant (duplicate files. )
budget_overlap <- duplicated(all_cost_categories[data_source == "budget" & file_iteration == "final", .(grant, start_date)])
pudr_overlap <- duplicated(all_cost_categories[data_source == "pudr" & file_iteration == "final", .(grant, start_date)])
stopifnot(nrow(budget_overlap)==0 & nrow(pudr_overlap)==0)

rm(budget_overlap, pudr_overlap)

#Make sure all budget and expenditure variables are numeric. 
all_cost_categories$budget <- as.numeric(all_cost_categories$budget)
all_cost_categories$expenditure <- as.numeric(all_cost_categories$expenditure)
all_cost_categories$disbursement <- as.numeric(all_cost_categories$disbursement)

#Add files here that had a sum total for 0 in raw file. 
verified_0_budget <- c('UGD-708-G08-M_PUDR 30Nov2011.xls', 'UGD-708-G08-M_PUDR_30June2012.xls', "Core_SANRU_PU_P3141116.xlsm",
                       "PSI PU NFM S1 2016 09102016.xlsm", "Core_PUDR_P30_HivosGT_231116_ LFA Signed.xlsx", 
                       "Core_PUDR_MALARIA_P12_03-03-17_Revisado ALF.xlsx")
#Add PUDRs here that did not report any expenditure.
verified_0_expenditure <- c("UGA-C-TASO_PU_PEJune2017_LFA_30Nov17.xlsx", "UGA-M-TASO_PU_PEJune2017_LFA_30Nov17.xlsx", 
                            "UGA-S-TASO_PU_PEJune2017_LFA_30Nov17.xlsx", "GTM-T-MSPAS_Progress Report_31Dec2017 LFA REVIEW.xlsx", 
                            "GTM-T-MSPAS_Progress Report jul _31Dec2018_v2  rev LFA.xlsx", "GTM-H-HIVOS_Progress Report_31Dec2018_v1.xlsx", 
                            "GTM-T-MSPAS_Progress Report_LFA18Mar19.xlsx", "Core_SANRU_PU_P3141116.xlsm", "PSI PU NFM S1 2016 09102016.xlsm", 
                            "Core_PUDR_P30_HivosGT_231116_ LFA Signed.xlsx", "Core_PUDR_MALARIA_P12_03-03-17_Revisado ALF.xlsx",  
                            "GTM-T-MSPAS_Progress Report_31Dec2017 LFA REVIEW.XLSX", "GTM-M-MSPAS_Progress Report_30Jun2019_REV LFA.XLSX") #These files have 0 for all expenditure.

#Make sure that no files have a total sum of 0; this would indicate an error in the prep code. 
check_0_budgets <- all_cost_categories[, .(budget = sum(budget, na.rm = TRUE)), by=.(file_name)]
check_0_budgets = check_0_budgets[budget == 0 & !file_name%in%verified_0_budget]
check_0_expenditure <- all_cost_categories[data_source == 'pudr', .(expenditure = sum(expenditure, na.rm = TRUE)), by=.(file_name)]
check_0_expenditure <- check_0_expenditure[expenditure == 0 & !file_name%in%verified_0_expenditure]
stopifnot(nrow(check_0_budgets)==0 & nrow(check_0_expenditure)==0)

#check for duplicates, and sum their values if they exist:
dups<-all_cost_categories[duplicated(all_cost_categories) | duplicated(all_cost_categories, fromLast=TRUE)]
print(paste0(nrow(dups), " duplicates found in database; values will be summed"))
byVars = names(all_cost_categories)[!names(all_cost_categories)%in%c('budget', 'expenditure', 'disbursement')]
all_cost_categories= all_cost_categories[, list(budget=sum(na.omit(budget)) ,expenditure=sum(na.omit(expenditure)), disbursement=sum(na.omit(disbursement))), by=byVars]

#Make sure you have all the files here that you started with in your filelist. 
rt_files <- unique(all_cost_categories$file_name)
warning1 = (length(unique(file_list$file_name)) == length(rt_files))
if (!warning1){
  warning("The length of the original file list is not the same as the number of processed files.")
}
warning2 = sort(rt_files) == sort(unique(file_list$file_name))
if (!warning2){
  warning("The files in the processed data are not the same as the files in the file list.")
}

print("Step B: Prep GF files completed.")
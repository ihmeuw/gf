# ----------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Preps commitments, obligations data from Global Fund PUDRs.
# DATE: Last updated January 2020. 
# 
# The current working directory should be the root of this repository, 
# and this file should be sourced from '1_master_file.r' in this folder.
# ----------------------------------------------

#----------------------------------------------------
# Read in file list 
#----------------------------------------------------

file_list = load_master_list(purpose = "financial") #This function is sourced from the _common folder in master script. 
file_list = file_list[loc_name%in%countries] #List of countries to run is set in master file

# Subset down to just PUDRs where we know we'll have commitments/obligations data 
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
  all_commitments = data.table() 
  for (country in countries){
    master_file_dir = ifelse(Sys.info()[1]=='Windows', paste0(box, toupper(country), "/raw_data/"), 
                             paste0(dir, "_gf_files_gos/", country, "/raw_data/"))
    export_dir = ifelse(Sys.info()[1]=="Windows", paste0(box, country, "/prepped_data/"),
                        paste0(dir, "_gf_files_gos/", country, "/prepped_data/"))
  
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
      args = list(file_dir, file_list_subset$file_name[i])
      tmpData = do.call(prep_commitments_pudr, args)
  
      #Add indexing data
      append_cols = file_list_subset[i, .(data_source, grant_period, primary_recipient, file_name, grant_status, disease, grant, 
                                   mod_framework_format, file_iteration, language_financial, file_currency, pudr_semester_financial, period_financial, update_date)]
      for (col in names(append_cols)){
        tmpData[, (col):=append_cols[, get(col)]]
      }  
      # tmpData$year <- year(tmpData$start_date)
      # tmpData[, file_start_date:=min(start_date), by='file_name']
      tmpData$loc_name <- country
      
      #Bind data together 
      if(i==1){
        resource_database = tmpData
      } else {
        resource_database = rbind(resource_database, tmpData, use.names=TRUE, fill = TRUE)
      }
      print(paste0(i, " ", file_list_subset$data_source[i], " ", file_list_subset$function_financial[i], " ", file_list_subset$grant[i])) ## if the code breaks, you know which file it broke on
    }
    saveRDS(resource_database, paste0(export_dir, "raw_bound_commitments.RDS"))
    saveRDS(resource_database, paste0(export_dir, "archive/raw_bound_commitments_", Sys.Date(), ".RDS"))
    
    all_commitments = rbind(all_commitments, resource_database, fill=T)
  } 
  
  #If you don't have lfa_exp_adjustment in any of the files for this country, add it as NA so checks later will work. 
} else {
  cod = readRDS(paste0(box, "COD/prepped_data/raw_bound_commitments.RDS")) 
  gtm = readRDS(paste0(box, "GTM/prepped_data/raw_bound_commitments.RDS")) 
  sen = readRDS(paste0(box, "SEN/prepped_data/raw_bound_commitments.RDS")) 
  uga = readRDS(paste0(box, "UGA/prepped_data/raw_bound_commitments.RDS")) 
  
  all_commitments = rbindlist(list(cod, gtm, sen, uga))
}

# If you didn't re-run all countries, bind on their raw data.
if (rerun_filelist) { 
  all_countries = c('cod', 'gtm', 'sen', 'uga') 
  not_run = all_countries[!all_countries%in%countries]
  if (length(not_run)>0){
    for (country in not_run){
      add_data = readRDS(paste0(box, toupper(country), "/prepped_data/raw_bound_commitments.RDS"))
      all_commitments = rbind(all_commitments, add_data)
    }
  }
} 

print("Step A: Prep commitments completed.")
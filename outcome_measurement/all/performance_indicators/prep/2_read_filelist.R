# ----------------------------------------------
# AUTHOR: Emily Linebarger
# PURPOSE: Read in file_lists and extract indicator data. 
# DATE: Last updated June 2019. 
# ----------------------------------------------

master_file_list = load_master_list(purpose="performance indicators")

#Only want to keep final files (EL 9/12/2019 - what about PUDR revisions?)
master_file_list = master_file_list[file_iteration=="final"]

#Flag files where sheets are NA - if they are duplicated with non-NA files, drop these. 
na_sheets = master_file_list[is.na(sheet_impact_outcome_1a) | sheet_impact_outcome_1a=="NA" |
                               is.na(sheet_impact_outcome_1a_disagg) | sheet_impact_outcome_1a_disagg=="NA" |
                               is.na(sheet_coverage_1b) | sheet_coverage_1b=="NA" | 
                               is.na(sheet_coverage_1b_disagg) | sheet_coverage_1b_disagg=="NA", file_name]

#Make sure you don't have the same start date for the same grant (quick check; it would be better )
master_file_list[, date_dup:=sequence(.N), by=c('grant', 'grant_period', 'start_date_programmatic', 'pudr_semester_programmatic')] #EMILY NEED TO RETHINK THIS. 
master_file_list[, date_dup:=date_dup-1]#This indexes at one, so you need to decrement it

#Drop duplicates that are NA 
dup_grants = master_file_list[date_dup==1, .(grant, grant_period, pudr_semester_programmatic)]
dup_grants[, concat:=paste0(grant, grant_period, pudr_semester_programmatic)]
dup_files = master_file_list[paste0(grant, grant_period, pudr_semester_programmatic)%in%dup_grants$concat, file_name]

master_file_list = master_file_list[!(file_name%in%na_sheets & file_name%in%dup_files)]

#Re-do duplicates check. 
master_file_list$date_dup = NULL
master_file_list[, date_dup:=sequence(.N), by=c('grant', 'grant_period', 'start_date_programmatic', 'pudr_semester_programmatic')] #EMILY NEED TO RETHINK THIS. 
master_file_list[, date_dup:=date_dup-1]#This indexes at one, so you need to decrement it

if ( nrow(master_file_list[date_dup>0])!=0){
  print(master_file_list[date_dup > 0, .(file_name, file_iteration, grant, grant_period, start_date_financial)][order(grant, grant_period, start_date_financial)])
  print("There are duplicates in final files - review file list.")
}

#Only extract indicators for current grant periods! EL 9/20/2019 
current_periods = c('2018-2020', '2016-2019', '2018-2018')
master_file_list = master_file_list[grant_period%in%current_periods]

#-------------------------------------------
#Prep Coverage 1A sheets 
#-------------------------------------------
if (prep_1a){
  print("Prepping 1A sheets...")
  for (country in countries){
    print(paste0("Processing country: ", country))
    file_list = master_file_list[loc_name==country & file_iteration=="final" & !is.na(sheet_impact_outcome_1a)] 
    
    #Just force-change all of the sheet names for now - will want to make sure this is right EKL 6/25/19 
    #country_dir = ? 
    master_file_dir = paste0(box, toupper(country), "/raw_data/")
    
    # Validate file list 
    if (nrow(file_list)>0){ #Do you have files for this country to process? 
      for (i in 1:nrow(file_list)){
        # Set up file path 
        folder = "budgets"
        folder = ifelse (file_list$data_source[i] == "pudr", "pudrs", folder)
        if (file_list$file_iteration[i]=="initial"){
          version = "iterations"
        } else if (file_list$file_iteration[i]=="revision"){
          version= "revisions"
        } else {
          version = ""
        }
        grant_period = file_list$grant_period[i]
        
        file_dir = paste0(master_file_dir, file_list$grant_status[i], "/", file_list$grant[i], "/", grant_period, "/", folder, "/")
        if (version != ""){
          file_dir = paste0(file_dir, version, "/")
        }
        
        args = list(file_dir, file_list$file_name[i], file_list$sheet_impact_outcome_1a[i], file_list$language_programmatic[i])
        tmpData = do.call(prep_impact_outcome_1A, args) 
        
        #Append important columns from the file list 
        append_cols = file_list[i, .(loc_name, grant, grant_period, grant_status, file_name, disease, primary_recipient, start_date_programmatic, end_date_programmatic)]
        for (col in names(append_cols)){
          tmpData[, (col):=append_cols[, get(col)]]
        }  
        tmpData$pudr_sheet = "impact_outcome_indicators_main"
        
        #Bind the files together 
        if (i == 1){
          impact_outcome1A = tmpData
        } else {
          impact_outcome1A = rbind(tmpData, impact_outcome1A, fill=T, use.names=T) #You won't always have the same column names, and that's ok. 
        }
        print(paste0(i, " ", file_list$grant[i], " ", file_list$grant_period[i])) ## if the code breaks, you know which file it broke on
        
      }
      
      #Save the country-level file 
      saveRDS(impact_outcome1A, paste0("J:/Project/Evaluation/GF/process_evaluation/pudr_indicator_extraction/prepped_data/", country, "_1A.rds"))
      write.csv(impact_outcome1A, paste0("J:/Project/Evaluation/GF/process_evaluation/pudr_indicator_extraction/prepped_data/", country, "_1A.csv"), row.names=F)
      
      #Archive a copy 
      saveRDS(impact_outcome1A, paste0("J:/Project/Evaluation/GF/process_evaluation/pudr_indicator_extraction/prepped_data/archive/", country, "_1A_", Sys.Date(), ".rds"))
      
    } else {
      print(paste0("No applicable Coverage 1A files for ", country))
    }
    
  }
}

#-------------------------------------------
#Prep Coverage 1A disaggregated sheets 
#-------------------------------------------
if (prep_1a_disagg){
  print("Prepping 1A disaggregated sheets...")
  for (country in countries){
    print(paste0("Processing country: ", country))
    file_list = master_file_list[loc_name==country & file_iteration=="final" & !is.na(sheet_impact_outcome_1a_disagg)] 
    
    #Just force-change all of the sheet names for now - will want to make sure this is right EKL 6/25/19 
    #country_dir = ? 
    master_file_dir = paste0(box, toupper(country), "/raw_data/")
    
    # Validate file list 
    if (nrow(file_list)>0){ #Do you have files for this country to process? 
      for (i in 1:nrow(file_list)){
        # Set up file path 
        folder = "budgets"
        folder = ifelse (file_list$data_source[i] == "pudr", "pudrs", folder)
        if (file_list$file_iteration[i]=="initial"){
          version = "iterations"
        } else if (file_list$file_iteration[i]=="revision"){
          version= "revisions"
        } else {
          version = ""
        }
        grant_period = file_list$grant_period[i]
        
        file_dir = paste0(master_file_dir, file_list$grant_status[i], "/", file_list$grant[i], "/", grant_period, "/", folder, "/")
        if (version != ""){
          file_dir = paste0(file_dir, version, "/")
        }
        
        args = list(file_dir, file_list$file_name[i], file_list$sheet_impact_outcome_1a_disagg[i], file_list$language_programmatic[i])
        tmpData = do.call(prep_impact_outcome_1A_disagg, args) 
        
        #Append important columns from the file list 
        append_cols = file_list[i, .(loc_name, grant, grant_period, grant_status, file_name, disease, primary_recipient, start_date_programmatic, end_date_programmatic)]
        for (col in names(append_cols)){
          tmpData[, (col):=append_cols[, get(col)]]
        }  
        tmpData$pudr_sheet = "impact_outcome_indicators_disagg"
        
        #Bind the files together 
        if (i == 1){
          impact_outcome1A = tmpData
        } else {
          impact_outcome1A = rbind(tmpData, impact_outcome1A, fill=T, use.names=T) #You won't always have the same column names, and that's ok. 
        }
        print(paste0(i, " ", file_list$grant[i], " ", file_list$grant_period[i])) ## if the code breaks, you know which file it broke on
        
      }
      
      #Save the country-level file 
      saveRDS(impact_outcome1A, paste0("J:/Project/Evaluation/GF/process_evaluation/pudr_indicator_extraction/prepped_data/", country, "_1A_disagg.rds"))
      write.csv(impact_outcome1A, paste0("J:/Project/Evaluation/GF/process_evaluation/pudr_indicator_extraction/prepped_data/", country, "_1A_disagg.csv"), row.names=F)
      
      #Archive a copy 
      saveRDS(impact_outcome1A, paste0("J:/Project/Evaluation/GF/process_evaluation/pudr_indicator_extraction/prepped_data/archive/", country, "_1A_disagg_", Sys.Date(), ".rds"))
      
    } else {
      print(paste0("No applicable Coverage 1A disaggregated files for ", country))
    }
    
  }
}

#-------------------------------------------
#Prep Coverage 1B sheets 
#-------------------------------------------
if (prep_1b){
  print("Prepping 1B sheets...")
  for (country in countries){
    print(paste0("Processing country: ", country))
    file_list = master_file_list[loc_name==country & file_iteration=="final" & !is.na(sheet_coverage_1b)] 
    
    #Just force-change all of the sheet names for now - will want to make sure this is right EKL 6/25/19 
    #country_dir = ? 
    master_file_dir = paste0(box, toupper(country), "/raw_data/")
    
    # Validate file list 
    if (nrow(file_list)>0){ #Do you have files for this country to process? 
    for (i in 1:nrow(file_list)){
      # Set up file path 
      folder = "budgets"
      folder = ifelse (file_list$data_source[i] == "pudr", "pudrs", folder)
      if (file_list$file_iteration[i]=="initial"){
        version = "iterations"
      } else if (file_list$file_iteration[i]=="revision"){
        version= "revisions"
      } else {
        version = ""
      }
      grant_period = file_list$grant_period[i]
      
      file_dir = paste0(master_file_dir, file_list$grant_status[i], "/", file_list$grant[i], "/", grant_period, "/", folder, "/")
      if (version != ""){
        file_dir = paste0(file_dir, version, "/")
      }
      
      args = list(file_dir, file_list$file_name[i], file_list$sheet_coverage_1b[i], file_list$language_programmatic[i])
      tmpData = do.call(prep_coverage_1B, args) 
      
      #Append important columns from the file list 
      append_cols = file_list[i, .(loc_name, grant, grant_period, grant_status, file_name, disease, primary_recipient, start_date_programmatic, end_date_programmatic)]
      for (col in names(append_cols)){
        tmpData[, (col):=append_cols[, get(col)]]
      }  
      tmpData$pudr_sheet = "coverage_indicators_main"
      
      #Bind the files together 
      if (i == 1){
        coverage1B = tmpData
      } else {
        coverage1B = rbind(tmpData, coverage1B, fill=T, use.names=T) #You won't always have the same column names, and that's ok. 
      }
      print(paste0(i, " ", file_list$grant[i], " ", file_list$grant_period[i])) ## if the code breaks, you know which file it broke on

    }
    
    #Save the country-level file 
    saveRDS(coverage1B, paste0("J:/Project/Evaluation/GF/process_evaluation/pudr_indicator_extraction/prepped_data/", country, "_1B.rds"))
    write.csv(coverage1B, paste0("J:/Project/Evaluation/GF/process_evaluation/pudr_indicator_extraction/prepped_data/", country, "_1B.csv"), row.names=F)
    
    #Archive a copy 
    saveRDS(coverage1B, paste0("J:/Project/Evaluation/GF/process_evaluation/pudr_indicator_extraction/prepped_data/archive/", country, "_1B_", Sys.Date(), ".rds"))
    
    } else {
      print(paste0("No applicable Coverage 1B files for ", country))
    }
    
  }
}

#-------------------------------------------
#Prep Coverage 1B disaggregated sheets 
#-------------------------------------------
if (prep_1b_disagg){
  print("Prepping 1B disaggregated sheets...")
  for (country in countries){
    print(paste0("Processing country: ", country))
    file_list = master_file_list[loc_name==country & file_iteration=="final" & !is.na(sheet_coverage_1b_disagg)] 
    
    #Just force-change all of the sheet names for now - will want to make sure this is right EKL 6/25/19 
    #country_dir = ? 
    master_file_dir = paste0(box, toupper(country), "/raw_data/")
    
    # Validate file list 
    if (nrow(file_list)>0){ #Do you have files for this country to process? 
      for (i in 1:nrow(file_list)){
        # Set up file path 
        folder = "budgets"
        folder = ifelse (file_list$data_source[i] == "pudr", "pudrs", folder)
        if (file_list$file_iteration[i]=="initial"){
          version = "iterations"
        } else if (file_list$file_iteration[i]=="revision"){
          version= "revisions"
        } else {
          version = ""
        }
        grant_period = file_list$grant_period[i]
        
        file_dir = paste0(master_file_dir, file_list$grant_status[i], "/", file_list$grant[i], "/", grant_period, "/", folder, "/")
        if (version != ""){
          file_dir = paste0(file_dir, version, "/")
        }
        
        args = list(file_dir, file_list$file_name[i], file_list$sheet_coverage_1b_disagg[i], file_list$language_programmatic[i])
        tmpData = do.call(prep_coverage_1B_disagg, args) 
        
        #Append important columns from the file list 
        append_cols = file_list[i, .(loc_name, grant, grant_period, grant_status, file_name, disease, primary_recipient, start_date_programmatic, end_date_programmatic)]
        for (col in names(append_cols)){
          tmpData[, (col):=append_cols[, get(col)]]
        }  
        tmpData$pudr_sheet = "coverage_indicators_disagg"
        
        #Bind the files together 
        if (i == 1){
          coverage1B = tmpData
        } else {
          coverage1B = rbind(tmpData, coverage1B, fill=T, use.names=T) #You won't always have the same column names, and that's ok. 
        }
        print(paste0(i, " ", file_list$grant[i], " ", file_list$grant_period[i])) ## if the code breaks, you know which file it broke on
        
      }
      
      #Save the country-level file 
      saveRDS(coverage1B, paste0("J:/Project/Evaluation/GF/process_evaluation/pudr_indicator_extraction/prepped_data/", country, "_1B_disagg.rds"))
      write.csv(coverage1B, paste0("J:/Project/Evaluation/GF/process_evaluation/pudr_indicator_extraction/prepped_data/", country, "_1B_disagg.csv"), row.names=F)
      
      #Archive a copy 
      saveRDS(coverage1B, paste0("J:/Project/Evaluation/GF/process_evaluation/pudr_indicator_extraction/prepped_data/archive/", country, "_1B_disagg_", Sys.Date(), ".rds"))
      
    } else {
      print(paste0("No applicable Coverage 1B disaggregated files for ", country))
    }
    
  }
}
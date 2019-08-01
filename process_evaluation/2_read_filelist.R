# ----------------------------------------------
# AUTHOR: Emily Linebarger
# PURPOSE: Read in file_lists and extract indicator data. 
# DATE: Last updated June 2019. 
# ----------------------------------------------

master_file_list = data.table(read.xlsx("J:/Project/Evaluation/GF/resource_tracking/_gf_files_gos/master_file_list.xlsx", detectDates=T))
master_file_list[, start_date_programmatic:=as.Date(start_date_programmatic, format = "%m/%d/%Y")]
master_file_list = master_file_list[grant_status == "active"]

# THIS SHOULD BE DELETED WHEN YOU MAKE A MORE GENERAL FUNCTION! EL 7/24/2019
master_file_list = master_file_list[sheet_coverage_1b%in%c("Indicateurs Couverture_1B", "Coverage Indicators_1B")]
# ONLY WANT TO PREP CURRENT GRANTS FOR NOW, BUT SHOULD CHANGE THIS LATER! el 8/1/2019
master_file_list = master_file_list[grant_period%in%c('2018-2020', '2018', '2016-2019')] #This might not be catching everything for Guatemala. 



#-------------------------------------------
#Prep Coverage 1A sheets 
#-------------------------------------------
if (prep_1a){
  for (country in countries){
    print(paste0("Processing country: ", country))
    file_list = master_file_list[loc_name==country & file_iteration=="final" & !is.na(sheet_impact_outcome_1a)] 
    
    #Just force-change all of the sheet names for now - will want to make sure this is right EKL 6/25/19 
    #country_dir = ? 
    master_file_dir = paste0("J:/Project/Evaluation/GF/resource_tracking/_gf_files_gos/", country, "/raw_data/")
    
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
          impact_outcome1A = rbind(tmpData, impact_outcome1A, fill=T) #You won't always have the same column names, and that's ok. 
        }
        print(paste0(i, " ", file_list$grant[i], " ", file_list$grant_period[i])) ## if the code breaks, you know which file it broke on
        
      }
      
      #Save the country-level file 
      saveRDS(impact_outcome1A, paste0("J:/Project/Evaluation/GF/process_evaluation/pudr_indicator_extraction/prepped_data/", country, "_1A.rds"))
      write.csv(impact_outcome1A, paste0("J:/Project/Evaluation/GF/process_evaluation/pudr_indicator_extraction/prepped_data/", country, "_1A.csv"), row.names=F)
      
    } else {
      print(paste0("No applicable Coverage 1A files for ", country))
    }
    
  }
}

#-------------------------------------------
#Prep Coverage 1B sheets 
#-------------------------------------------
if (prep_1b){
  for (country in countries){
    print(paste0("Processing country: ", country))
    file_list = master_file_list[loc_name==country & file_iteration=="final" & !is.na(sheet_coverage_1b)] 
    
    #Just force-change all of the sheet names for now - will want to make sure this is right EKL 6/25/19 
    #country_dir = ? 
    master_file_dir = paste0("J:/Project/Evaluation/GF/resource_tracking/_gf_files_gos/", country, "/raw_data/")
    
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
        coverage1B = rbind(tmpData, coverage1B, fill=T) #You won't always have the same column names, and that's ok. 
      }
      print(paste0(i, " ", file_list$grant[i], " ", file_list$grant_period[i])) ## if the code breaks, you know which file it broke on

    }
    
    #Save the country-level file 
    saveRDS(coverage1B, paste0("J:/Project/Evaluation/GF/process_evaluation/pudr_indicator_extraction/prepped_data/", country, "_1B.rds"))
    write.csv(coverage1B, paste0("J:/Project/Evaluation/GF/process_evaluation/pudr_indicator_extraction/prepped_data/", country, "_1B.csv"), row.names=F)
    
    } else {
      print(paste0("No applicable Coverage 1B files for ", country))
    }
    
  }
}

#-------------------------------------------
#Prep Coverage 1B disaggregated sheets 
#-------------------------------------------
if (prep_1b_disagg){
  for (country in countries){
    print(paste0("Processing country: ", country))
    file_list = master_file_list[loc_name==country & file_iteration=="final" & !is.na(sheet_coverage_1b_disagg)] 
    
    #Just force-change all of the sheet names for now - will want to make sure this is right EKL 6/25/19 
    #country_dir = ? 
    master_file_dir = paste0("J:/Project/Evaluation/GF/resource_tracking/_gf_files_gos/", country, "/raw_data/")
    
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
          coverage1B = rbind(tmpData, coverage1B, fill=T) #You won't always have the same column names, and that's ok. 
        }
        print(paste0(i, " ", file_list$grant[i], " ", file_list$grant_period[i])) ## if the code breaks, you know which file it broke on
        
      }
      
      #Save the country-level file 
      saveRDS(coverage1B, paste0("J:/Project/Evaluation/GF/process_evaluation/pudr_indicator_extraction/prepped_data/", country, "_1B_disagg.rds"))
      write.csv(coverage1B, paste0("J:/Project/Evaluation/GF/process_evaluation/pudr_indicator_extraction/prepped_data/", country, "_1B_disagg.csv"), row.names=F)
      
    } else {
      print(paste0("No applicable Coverage 1B disaggregated files for ", country))
    }
    
  }
}
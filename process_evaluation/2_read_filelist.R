# ----------------------------------------------
# AUTHOR: Emily Linebarger
# PURPOSE: Read in file_lists and extract indicator data. 
# DATE: Last updated June 2019. 
# ----------------------------------------------

master_file_list = data.table(read.xlsx("J:/Project/Evaluation/GF/resource_tracking/_gf_files_gos/master_file_list.xlsx", detectDates=T))
master_file_list[, start_date_programmatic:=as.Date(start_date_programmatic, format = "%m/%d/%Y")]
master_file_list = master_file_list[!is.na(function_coverage_1b) & grant_status == "active"]

# THIS SHOULD BE DELETED WHEN YOU MAKE A MORE GENERAL FUNCTION! EL 7/24/2019
master_file_list = master_file_list[sheet_coverage_1b%in%c("Indicateurs Couverture_1B", "Coverage Indicators_1B")]
#-------------------------------------------
#Prep Coverage 1B sheets 
#-------------------------------------------
if (prep_1b){
  for (country in countries){
    file_list = master_file_list[loc_name==country & file_iteration=="final"] 
    
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
      
      #Bind the files together 
      if (i == 1){
        coverage1B = tmpData
      } else {
        coverage1B = rbind(tmpData, coverage1B)
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
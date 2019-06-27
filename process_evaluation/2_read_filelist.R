# ----------------------------------------------
# AUTHOR: Emily Linebarger
# PURPOSE: Read in file_lists and extract indicator data. 
# DATE: Last updated June 2019. 
# ----------------------------------------------

master_file_list = data.table(read.xlsx("J:/Project/Evaluation/GF/resource_tracking/_gf_files_gos/master_file_list.xlsx"))
master_file_list[, start_date:=as.Date(start_date, format = "%m/%d/%Y")]
master_file_list = master_file_list[!is.na(function_coverage_1b) & grant_status == "active"]
#-------------------------------------------
#Prep Coverage 1B sheets 
#-------------------------------------------
if (prep_1b){
  for (country in countries){
    file_list = master_file_list[loc_name==country & file_iteration=="final" & year(start_date)>=2018] #Just prep 2018 files for now!
    
    #Just force-change all of the sheet names for now - will want to make sure this is right EKL 6/25/19 
    #country_dir = ? 
    master_file_dir = paste0("J:/Project/Evaluation/GF/resource_tracking/_gf_files_gos/", country, "/raw_data/")
    
    # Validate file list 
    if (nrow(file_list)>0){ #Do you have files for this country to process? 
    for (i in 1:nrow(file_list)){
      #Prep each file 
      folder = "pudrs"
      version = ifelse(file_list$file_iteration[i] == "initial", "iterations", "")
      file_dir = paste0(master_file_dir, file_list$grant_status[i], "/", file_list$grant[i], "/", folder, "/")
      if (version != ""){
        file_dir = paste0(file_dir, version, "/")
      }
      
      args = list(file_dir, file_list$file_name[i], file_list$sheet_coverage_1b[i], file_list$language_coverage_1b[i])
      tmpData = do.call(prep_coverage_1B, args) 
      
      #Append important columns from the file list 
      append_cols = file_list[i, .(loc_name, grant, grant_period, grant_status, file_name, disease, primary_recipient, start_date, period)]
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
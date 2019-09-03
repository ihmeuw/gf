# ----------------------------------------------
# AUTHOR: Emily Linebarger
# PURPOSE: Read in file_lists and extract unit costs. 
# DATE: Last updated August 2019. 
# ----------------------------------------------

master_file_list = data.table(read_excel("J:/Project/Evaluation/GF/resource_tracking/_gf_files_gos/master_file_list.xlsx"))
master_file_list[, start_date_programmatic:=as.Date(start_date_programmatic, format = "%m/%d/%Y")]

#Only keep the final detailed budgets 
master_file_list = master_file_list[data_source=="fpm" & function_financial=="detailed" & !is.na(function_financial) & file_iteration=='final']

#Only keep active grants for 2018 grant cycle for the moment. EL 8/28/19 
master_file_list = master_file_list[grant_status=="active" & grant_period%in%c('2016-2019', '2018-2020', '2018', '2019-2021')]

#-------------------------------------------
#Pull unit costs from detailed budgets. 
#-------------------------------------------
print("Pulling unit cost data...")

# Validate file list 
for (i in 1:nrow(master_file_list)){
  # Set up file path 
  master_file_dir = paste0("J:/Project/Evaluation/GF/resource_tracking/_gf_files_gos/", file_list$loc_name[i], "/raw_data/")
  
  folder = "budgets"
  folder = ifelse (master_file_list$data_source[i] == "pudr", "pudrs", folder)
  if (master_file_list$file_iteration[i]=="initial"){
    version = "iterations"
  } else if (master_file_list$file_iteration[i]=="revision"){
    version= "revisions"
  } else {
    version = ""
  }
  grant_period = master_file_list$grant_period[i]
  
  file_dir = paste0(master_file_dir, master_file_list$grant_status[i], "/", master_file_list$grant[i], "/", grant_period, "/", folder, "/")
  if (version != ""){
    file_dir = paste0(file_dir, version, "/")
  }
  
  args = list(file_dir, master_file_list$file_name[i], master_file_list$sheet_impact_outcome_1a[i], master_file_list$language_programmatic[i])
  tmpData = do.call(prep_budget_unit_costs, args) 
  
  #Append important columns from the file list 
  append_cols = master_file_list[i, .(loc_name, grant, grant_period, grant_status, file_name, disease, 
                                      primary_recipient, start_date_financial, end_date_financial)]
  for (col in names(append_cols)){
    tmpData[, (col):=append_cols[, get(col)]]
  }  
  
  #Bind the files together 
  if (i == 1){
    unit_costs = tmpData
  } else {
    unit_costs = rbind(tmpData, unit_costs, fill=T, use.names=T) #You won't always have the same column names, and that's ok. 
  }
  print(paste0(i, " ", master_file_list$loc_name[i], " ", master_file_list$grant[i], " ", master_file_list$grant_period[i])) ## if the code breaks, you know which file it broke on
  
}

#Save the country-level file 
saveRDS(unit_costs, paste0(dir, "gf_unit_costs_", country, ".rds"))
write.csv(unit_costs, paste0(dir, "gf_unit_costs_", country, ".csv"), row.names=F)

#Save an archived version
saveRDS(unit_costs, paste0(dir, "archive/gf_unit_costs_", country, Sys.Date(), ".rds"))
write.csv(unit_costs, paste0(dir, "archive/gf_unit_costs_", country, Sys.Date(), ".csv"), row.names=F)

print("Step 2 complete: Unit costs data saved.")
  



#------------------------------------------------
# AUTHOR: Irena Chen, modified by Emily Linebarger
# DATE: Written in Nov. 2017, and modified May 2019
# PURPOSE: Preps SICOIN files. 
# ----------------------------------------------

#------------------------------------------------
# TO DO: Can we check if the files in our file list include everything in the raw data folder? 
file_list = fread(paste0(sicoin_raw, "sicoin_filelist.csv"))
#Subset to one function at a time so we can see what they're doing. 
file_list = file_list[function_type=='detailed']

## loop over all of the files 
if (rerun_filelist==TRUE){
  for(i in 1:length(file_list$file_name)){
    
    if(file_list$function_type[i]=="detailed"){
      tmpData = prep_detailed_sicoin(file_list$file_name[i])
    } else {
      print("Function hasn't been verified yet.")
    }
    
    #Still need to modify these functions! 
    #   else if (file_list$function_type[i]=="summary"){
    #   tmpData = prep_summary_sicoin(as.character(paste0(sicoinDir,file_list$file_path[i],file_list$file_name[i])), ymd(file_list$start_date[i]), file_list$disease[i], file_list$period[i], file_list$source[i])
    # } else if (file_list$function_type[i]=="blank"){
    #   tmpData = prep_blank_sicoin(country, adm1, ymd(file_list$start_date[i]), file_list$disease[i], file_list$period[i], file_list$source[i])
    # } else if(file_list$function_type[i]=="donacions"){
    #   tmpData = prep_donacions_sicoin(as.character(paste0(sicoinDir,file_list$file_path[i],file_list$file_name[i])),
    #                                    ymd(file_list$start_date[i]), file_list$disease[i], file_list$period[i], file_list$source[i],
    #                                    country,  adm1)
    # } else if (file_list$function_type[i]=="report"){
    #   tmpData = prep_report_sicoin(as.character(paste0(sicoinDir,file_list$file_path[i],file_list$file_name[i])), ymd(file_list$start_date[i]), file_list$disease[i], file_list$period[i], file_list$source[i])
    # }
    
    #Add extra columns 
    append_cols = file_list[i, .(file_name, start_date, disease, is_program_file, program)]
    for (col in names(append_cols)){
      tmpData[, (col):=append_cols[, get(col)]]
    }  
    
    # Bind together with previously run files. 
    if(i==1){
      resource_database = tmpData
    } else {
      resource_database = rbind(resource_database, tmpData, use.names=TRUE)
    }
  
    print(paste0(i, " ", file_list$function_type[i], " ", file_list$file_name[i])) ## if the code breaks, you know which file it broke on
  }
  resource_database$data_source = "sicoin"
  resource_database$lang = "esp"
  saveRDS(resource_database, paste0(sicoin_prepped, "raw_bound_files.rds"))
} else {
  resource_database = readRDS(paste0(sicoin_prepped, "raw_bound_files.rds"))
}
# ---------------------------------------------
# Clean data and run quality checks 
# ---------------------------------------------

#Are there cases of overlapping or missing dates? 
date_dups = unique(resource_database[, .(department, municipality, file_name, start_date, disease, program)])
date_dups[, count:=1]
date_dups[, dup:=sum(count), by=c('department', 'municipality', 'start_date', 'disease', 'program')]
if (nrow(date_dups[dup>1])!=0){
  date_dups = date_dups[order(department, municipality, start_date)]
  View(date_dups[dup>1])
  print("Duplicate data - the same month is being covered for the same geographic area in two different files. Review.")
}









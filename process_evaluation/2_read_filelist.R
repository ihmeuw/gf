# ----------------------------------------------
# AUTHOR: Emily Linebarger
# PURPOSE: Read in filelists and extract indicator data. 
# DATE: Last updated June 2019. 
# ----------------------------------------------

#-------------------------------------------
#Prep Coverage 1B sheets 
#-------------------------------------------
if (prep_1b){
  for (country in countries){
    filelist = read.xlsx(paste0(dir, "filelist.xlsx")) 
    #country_dir = ? 
    
    # Validate file list 
    for (i in 1:nrow(filelist)){
      #Prep each file 
      args = list(file_list$dir[i], file_list$file_name[i], file_list$sheet_name[i], file_list$language[i])
      tmpData = do.call(prep_coverage_1b, args) 
      
      #Bind the files together 
      if (i == 1){
        coverage1B = tmpData
      } else {
        coverage1B = rbind(tmpData, coverage1B)
      }
    }
    
    #Save the country-level file 
    saveRDS(coverage1B, paste0(country_dir, country, "_1B.rds"))
  }
}
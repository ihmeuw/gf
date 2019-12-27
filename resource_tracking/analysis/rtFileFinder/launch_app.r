# Script to deploy R shiny app 
# Run line-by-line! 

library(data.table) 
library(rsconnect) 
library(shiny) 
library(readxl)

filePath = "C:/Users/elineb/Documents/gf/resource_tracking/analysis/rtFileFinder"

file_list = read_xlsx("C:/Users/elineb/Box Sync/Global Fund Files/master_file_list.xlsx")
write.csv(file_list, paste0(filePath, "/file_list.csv"))

rsconnect::deployApp(paste0(filePath))

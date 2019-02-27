# ----------------------------------------------
# Irena Chen
#
# 1/5/2018

### This code is to prepare the condom distribution data from Guatemala

# ----------------------------------------------
###### Function to prep the bed net data ###### 
# ----------------------------------------------

prep_condom_data <- function(inFile, sheet, start_date, period){
  cd_data <- data.table(read_excel(inFile, sheet = sheet))
  
  cd_data <- cd_data[-c(1:3),]
  cd_data <- cd_data[, c(1:4), with=FALSE]
  
  setnames(cd_data, 
           c("code", "department", "condom_consumption", "condom_stockage"))
  cd_data <- na.omit(cd_data, cols=c(1:2), invert=FALSE)
  ##add variables that might be useful:
  cd_data$start_date <- start_date
  cd_data$period <- period
  return(cd_data)
}

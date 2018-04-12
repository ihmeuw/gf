
# ----------------------------------------------
# Irena Chen
#
# 11/8/2017
# Template for prepping GF COD Category budget data
# Inputs:
# inFile - name of the file to be prepped
# Outputs:
# budget_dataset - prepped data.table object
# ----------------------------------------------

library(lubridate)
library(data.table)
library(readxl)
library(stats)
library(stringr)
library(rlang)
library(zoo)
# ----------------------------------------------

prep_old_module_budget = function(dir, inFile, sheet_name, start_date, 
                               qtr_num, disease, loc_name, period, grant, recipient, source){
  
  dates <- rep(start_date, qtr_num) # 
  for (i in 1:length(dates)){
    if (i==1){
      dates[i] <- start_date
    } else {
      dates[i] <- dates[i-1]%m+% months(3)
    }
  }
  
  if(!is.na(sheet_name)){
    gf_data <- data.table(read_excel(paste0(dir, inFile), sheet=as.character(sheet_name), col_names = FALSE))
  } else {
    gf_data <- data.table(read_excel(paste0(dir, inFile)))
  }
  
  colnames(gf_data)[2] <- "module"
  colnames(gf_data)[4] <- "sda_activity"
  ##only keep data that has a value in the "category" column 
  gf_data <- na.omit(gf_data, cols=2, invert=FALSE)
  
  ## grab the SDA data
  gf_data <- gf_data[c((grep("macro",tolower(gf_data$module))):(grep("implementing", tolower(gf_data$sda_activity)))),]
  
  ##drop the last row (we don't need them)
  gf_data <- head(gf_data,-1)
  
  ## rename the columns 
  colnames(gf_data) <- as.character(gf_data[1,])
  gf_data <- gf_data[-1,]
  toMatch <- c("Ann","Year", "RCC", "%", "Phase", "Implem", "Total")
  drop.cols <- grep(paste(toMatch, collapse="|"), ignore.case=TRUE, colnames(gf_data))
  gf_data <- gf_data[, (drop.cols) := NULL]
  
  
  ## also drop columns containing only NA's
  gf_data<- Filter(function(x) !all(is.na(x)), gf_data)
  
  ## invert the dataset so that budget expenses and quarters are grouped by category
  ##library(reshape)
 setnames(gf_data, c("module", "intervention", "sda_activity", dates))
  budget_dataset<- melt(gf_data,id=c("module", "intervention", "sda_activity"), variable.name = "start_date", value.name="budget")
  
  if(length(dates) != length(unique(budget_dataset$start_date))){
    stop('Error: quarters were dropped!')
  }
  
  
  ##rename the category column 
  budget_dataset$disease <- disease
  budget_dataset$loc_name <- loc_name
  budget_dataset$period <- period
  budget_dataset$grant_number <- grant
  budget_dataset$recipient <- recipient
  budget_dataset$qtr <- NULL
  budget_dataset$data_source <- source
  budget_dataset$start_date <- as.Date(budget_dataset$start_date,"%Y-%m-%d")
  return(budget_dataset)
  
}
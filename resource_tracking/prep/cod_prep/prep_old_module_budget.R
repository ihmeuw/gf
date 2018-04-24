
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
                               qtr_num, disease, loc_name, period, grant, recipient, source, lang){

  
  if(!is.na(sheet_name)){
    gf_data <- data.table(read_excel(paste0(dir, inFile), sheet=as.character(sheet_name), col_names = FALSE))
  } else {
    gf_data <- data.table(read_excel(paste0(dir, inFile)))
  }
  
  colnames(gf_data)[2] <- "module"
  colnames(gf_data)[3] <- "intervention"
  colnames(gf_data)[4] <- "sda_activity"
  colnames(gf_data)[5] <- "budget"

  ## grab the SDA data
  gf_data <- gf_data[c((grep("macro",tolower(gf_data$module))):(grep("implementing", tolower(gf_data$sda_activity)))),]
  
  ##drop the last row (we don't need them)
  gf_data <- gf_data[-c(1:2),]
  gf_data <- head(gf_data,-1)

  
  budget_dataset <-  gf_data[, c("module", "intervention", "sda_activity", "budget"), with=FALSE]
  
  
  budget_dataset <- na.omit(budget_dataset, cols=2, invert=FALSE)
  

  ##rename the category column 
  budget_dataset$disease <- disease
  budget_dataset$loc_name <- loc_name
  budget_dataset$period <- period
  budget_dataset$grant_number <- grant
  budget_dataset$start_date <- start_date
  budget_dataset$recipient <- recipient
  budget_dataset$qtr <- NULL
  budget_dataset$data_source <- source
  budget_dataset$lang <- lang
  budget_dataset$cost_category <- "all"
  budget_dataset$start_date <- as.Date(budget_dataset$start_date,"%Y-%m-%d")
  return(budget_dataset)
  
}
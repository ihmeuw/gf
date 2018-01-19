# ----------------------------------------------
# Irena Chen
#
# 10/31/2017
# Template for prepping UGA PUDR data:  
# Inputs:
# inFile - name of the file to be prepped
# year - which year the dataset corresponds to
#
# Outputs:
# budget_dataset - prepped data.table object

# ----------------------------------------------
##download necessary packages: 
library(lubridate)
library(data.table)
library(readxl)
library(stats)
library(stringr)
library(rlang)
library(zoo)
# ----------------------------------------------

# start function
prep_pudr_uga = function(dir, inFile, sheet_name, start_date, disease, period, grant, recipient, source) {
  
  # --------------------
  # Test the inputs
  if (class(inFile)!='character') stop('Error: inFile argument must be a string!')
  if (class(year)=='character') stop('Error: year argument must be a number!')
  # ----------------------------------------------
  # Files and directories
  
  # Load/prep data
  gf_data <-data.table(read_excel(paste0(dir,inFile), sheet=sheet_name))
  
  if(grant%in%c("UGD-708-GO8-M")){
    gf_data <- gf_data[, -c(1:3)]
    colnames(gf_data)[1] <- "cost_category"
    colnames(gf_data)[2] <- "budget"
    colnames(gf_data)[3] <- "expenditure"
    ghe_data <- ghe_data[c(grep("module", tolower(ghe_data$description)):grep(0, tolower(ghe_data$cost_category))),]
  
    } else {
    ##clean the data depending on if in spanish or english
    colnames(gf_data)[1] <- "description"
    colnames(gf_data)[2] <- "cost_category"
    colnames(gf_data)[3] <- "budget"
    colnames(gf_data)[5] <- "expenditure"
    gf_data <- gf_data[c(grep("module", tolower(gf_data$description)):grep(0, tolower(gf_data$cost_category))),]
  }
  ## drop 1st row: 
  gf_data <- gf_data[-1, ,drop = FALSE]
  budget_dataset <- gf_data[, c("cost_category", "budget", "expenditure"),with=FALSE]
  budget_dataset<- budget_dataset[!is.na(budget_dataset$cost_category),]
  budget_dataset$start_date <- start_date
  budget_dataset$source <- source
  budget_dataset$period <- period
  budget_dataset$disease <- disease
  budget_dataset$recipient <- recipient
  budget_dataset$grant_number <- grant

  # return prepped data
  return(budget_dataset)
}



# ----------------------------------------------
# Irena Chen
#
# 11/8/2017
# Template for prepping GF UGA budgets where the data is in "summary" form 
# Inputs:
# inFile - name of the file to be prepped
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
##function to clean the data: 

prep_summary_uga_budget = function(dir, inFile, sheet_name, start_date, qtr_num, cashText, grant, disease, period, recipient, source){

  if(!is.na(sheet_name)){
    gf_data <- data.table(read_excel(paste0(dir, inFile), sheet=as.character(sheet_name), col_names = FALSE))
  } else {
    gf_data <- data.table(read_excel(paste0(dir, inFile)))
  }
  
  ##we don't need the first three columns
  gf_data <- gf_data[, -c(1:3)]
  colnames(gf_data)[1] <- "cost_category"
  ##only keep data that has a value in the "category" column 
  gf_data <- na.omit(gf_data, cols=1, invert=FALSE)
  
  ## this type of budget data should always have 13 cost categories
  gf_data <- gf_data[c((grep("service deliv",tolower(gf_data$cost_category))):(grep("implementing", tolower(gf_data$cost_category)))),]
  
  ## drop the last row of the data 
  gf_data <- head(gf_data,-1)
  
  ## drop the first row now that we renamed the columns 
  toMatch <- c("%", "Phase", "Total")
  drop.cols <- grep(paste(toMatch, collapse="|"), ignore.case=TRUE, gf_data)
  gf_data <- gf_data[, (drop.cols) := NULL]
  
  dates <- rep(start_date, qtr_num) # 
  for (i in 1:length(dates)){
    if (i==1){
      dates[i] <- start_date
    } else if(i==2&grant=="UGD-708-G07-H"){
      dates[i] <- ymd("2011-10-01")
    } else {
      dates[i] <- dates[i-1]%m+% months(3)
    }
  }
  
  col_num = qtr_num+1
  
  gf_data <- gf_data[, 1:col_num]
  gf_data <- gf_data[-1,]
  
  setnames(gf_data, c("module", as.character(dates)))

  setDT(gf_data)
  budget_dataset<- melt(gf_data,id="module", variable.name = "start_date", value.name="budget")

  ##add categories
  budget_dataset$disease <- disease 
  budget_dataset$period <- period
  budget_dataset$intervention <- "All"
  budget_dataset$sda_activity <- "All"
  budget_dataset$expenditure <- 0 ## since we don't have expenditure data yet 
  budget_dataset$grant_number <- grant
  budget_dataset$recipient <- recipient
  budget_dataset$data_source <- source
  budget_dataset$start_date <- as.Date(budget_dataset$start_date)
  budget_dataset$year <- year(budget_dataset$start_date)
  return(budget_dataset)
  
}


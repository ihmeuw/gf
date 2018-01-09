# ----------------------------------------------
# Irena Chen
#
# 11/14/2017
# Template for prepping UGA GF budget data that is grouped by "cost category"
# Inputs:
# inFile - name of the file to be prepped
# Outputs:
# budget_dataset - prepped data.table object
# ---------------------------------------------
library(lubridate)
library(data.table)
library(readxl)
library(stats)
library(stringr)
library(rlang)
library(zoo)
# ----------------------------------------------


prep_detailed_uga_budget = function(dir, inFile, sheet_name, start_date, qtr_num, cashText, grant, disease, period, source) {
  
  # ----------------------------------------------
  ##prep functions that will be used in cleaning the code: 
  create_qtr_names = function(qtr_names, cashText){
    for(i in 1:qtr_num+3){
      if(i <4) {
        i=i+1
      } else { 
        qtr_names[i] <- paste("Q", i-3,  cashText, sep="")
      }
      i=i+1
    }
    return(qtr_names)
  }
  
  get_malaria_categories <- function(cost_category, program_activity){
    x <- as.character(cost_category)
    if(grepl("nets", tolower(program_activity))){
      x <- as.character(program_activity)
    }
    return(x)
  }
  
  ##create list of column names: 
  qtr_names <- c("Module", "Intervention", "Recipient", rep(1, qtr_num))
  qtr_names <- create_qtr_names(qtr_names, cashText)
  
  # ----------------------------------------------
  ##read the data from excel: 
  gf_data <- data.table(read_excel(paste0(dir, inFile), sheet=sheet_name))
  
  #3only grab the columns we want (program activity, recipient, and quarterly data) :
  gf_data <- gf_data[,names(gf_data)%in%qtr_names, with=FALSE]
  
  ##rename the columns: 
  colnames(gf_data)[1] <- "cost_category"
  colnames(gf_data)[2] <- "activity_description"
  colnames(gf_data)[3] <- "recipient"
  
  ##only keep data that has a value in the "category" column 
  gf_data <- na.omit(gf_data, cols=1, invert=FALSE)
  
  if(disease=="malaria"){
    gf_data$cost_category <- mapply(get_malaria_categories, gf_data$cost_category, gf_data$activity_description)
  } 
  gf_data$activity_description <- NULL
  
  ## invert the dataset so that budget expenses and quarters are grouped by category
  setDT(gf_data)
  gf_data1<- melt(gf_data,id=c("cost_category", "recipient"), variable.name = "qtr", value.name="budget")
  
  ##create vector that maps quarters to their start dates: 
  dates <- rep(start_date, qtr_num) # 
  for (i in 1:length(dates)){
    if (i==1){
      dates[i] <- start_date
    } else {
      dates[i] <- dates[i-1]%m+% months(3)
    }
  }
  
  if(length(dates) != length(unique(gf_data1$qtr))){
    stop('quarters were dropped!')
  }
  ##turn the list of dates into a dictionary (but only for quarters!) : 
  dates <- setNames(dates,unique(gf_data1$qtr))
  

  ## now match quarters with start dates 
  kDT = data.table(qtr = names(dates), value = TRUE, start_date = unname(dates))
  budget_dataset <-gf_data1[kDT, on=.(qtr), start_date := i.start_date ]
  budget_dataset$qtr <- NULL
  budget_dataset$period <- period
  budget_dataset$expenditure <- 0 
  budget_dataset$source <- source
  budget_dataset$grant_number <- grant
  budget_dataset$disease <- disease
  return(budget_dataset)
}

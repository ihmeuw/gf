# ----------------------------------------------
# Irena Chen
#
# 11/7/2017
# Template for prepping GF PUDR data
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

## Notes: running this will throw a warning: 
#Warning messages:
#1: In `[.data.table`(ghe_data, , `:=`((drop.cols), NULL)) :
# length(LHS)==0; no columns to delete or assign RHS to.

#But this shouldn't affect the final output. 


# ----------------------------------------------
dir <- 'J:/Project/Evaluation/GF/resource_tracking/gtm/'
file_list <- read.csv("H:/CIESAR/ciesar_data.csv", stringsAsFactors = FALSE)

for(i in 1:length(file_list$filename)){
  tmpData <- prep_pudr(dir, as.character(paste0(file_list$folder[i],'/',file_list$filename[i],file_list$extension[i])), as.character(file_list$sheet[i]), file_list$format[i], ymd(file_list$start_date[i]), file_list$disease[i],file_list$qtr_num[i], file_list$period[i], file_list$grant_number[i])
  if(i==1){
    resource_database = tmpData
  }
  if(i>1){
    resource_database = rbind(resource_database, tmpData, use.names=TRUE)
  }
  print(i)
}
# ----------------------------------------------

# Enforce variable classes
if (!is.numeric(resource_database$budget)) resource_database[,budget:=as.numeric(budget)]
if (!is.numeric(resource_database$disbursement)) resource_database[,disbursement:=as.numeric(disbursement)]
if (!is.numeric(resource_database$expenditure))resource_database[,expenditure:=as.numeric(expenditure)]


## since we only have budget data, include exp and disbursed as 0:  
resource_database$data_source <- "pudr_budget"

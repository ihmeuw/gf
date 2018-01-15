# ----------------------------------------------
# Irena Chen
#
# 11/7/2017
# Template for prepping GF budget data
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


dir <- 'C:/Users/irenac2/Documents/gf_budgets/'
file_list <- read.csv("C:/Users/irenac2/repos/gf/resource_tracking/prep/gf_budget_filelist.csv")

for(i in 1:length(file_list$filename)){
  if(file_list$format[i]=="gf_budget_cat"){
    tmpData <- prep_gtm_budget(dir, file_list$filename[i], file_list$extension[i], as.character(file_list$sheet[i]), ymd(file_list$start_date[i]), file_list$qtr_number[i])
  } else if (file_list$format[i]=="gf_budget_cost"){
    tmpData <- prep_module_budget(dir, file_list$filename[i], file_list$extension[i], as.character(file_list$sheet[i]), ymd(file_list$start_date[i]), file_list$qtr_number[i])
  }
  ## replace the "Q1" category with the associated dates that the quarters map to   
  tmpData1 <- map_quarters(tmpData, ymd(file_list$start_date[i]),file_list$qtr_number[i], file_list$loc_id[i], file_list$period[i],file_list$disease[i], file_list$source[i], file_list$grant_number[i])
    if(i==1){
        resource_database = tmpData1
    }
  
    if(i>1){
        resource_database = rbind(resource_database, tmpData1, use.names=TRUE)
    }
  print(i)

}

resource_database$budget <- as.numeric(resource_database$budget)

## since we only have budget data, include exp and disbursed as 0:  
resource_database$expenditure <- 0 
resource_database$disbursement<- 0 
resource_database$data_source <- "fpm_budget"

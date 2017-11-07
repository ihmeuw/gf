# ----------------------------------------------
# Irena Chen
#
# 11/2/2017
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


dir <- 'J:/Project/Evaluation/GF/resource_tracking/gtm/gf/'
inFile <- 'GUA-610-G04-T_SB_Year_6_7_ENG'
start_quarter <- ymd("2012-08-01")
file_list <- read.csv("C:/Users/irenac2/repos/gf/resource_tracking/prep/gf_budget_filelist.csv")

for(i in 1:length(file_list$filename)){
  if(file_list$format[i]=="gf_budget_cat"){
    tmpData <- prep_gtm_budget(dir, file_list$filename[i], file_list$extension[i], as.character(file_list$sheet[i]), ymd(file_list$start_date[i]), file_list$qtr_number[i])
  } else if (file_list$format[i]=="gf_budget_cost"){
    tmpData <- prep_module_budget(dir, file_list$filename[i], file_list$extension[i], as.character(file_list$sheet[i]), ymd(file_list$start_date[i]), file_list$qtr_number[i])
  }
    tmpData1 <- map_quarters(tmpData, ymd(file_list$start_date[i]),file_list$qtr_number[i], file_list$loc_id[i], file_list$period[i],file_list$disease[i], file_list$source[i])
    if(i==1){
        resource_database = tmpData1
    }
  
    if(i>1){
        resource_database = rbind(resource_database, tmpData1, use.names=TRUE)
    }

}

resource_database$budget <- as.numeric(resource_database$budget)

ghe_data <- data.table(read_excel(paste0('J:/Project/Evaluation/GF/resource_tracking/gtm/gf/', 'GUA-610-G04-T_SB_Year_6_7_ENG', '.xls'), sheet='Budget summary GF'))

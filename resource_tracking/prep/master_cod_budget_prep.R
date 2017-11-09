# ----------------------------------------------
# Irena Chen
#
# 11/8/2017
# Template for prepping GF COD budget data 
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


dir <- 'where the downloaded basecamp files are'
file_list <- read.csv("C:/Users/irenac2/repos/gf/resource_tracking/prep/cod_gf_filelist.csv", na.strings=c("","NA"))

for(i in 1:length(file_list$file_name)){
  if(file_list$type[i]=="fr"){
    tmpData <- prep_cod_fr_budget(dir, as.character(file_list$file_name[i]), file_list$sheet[i], ymd(file_list$start_date[i]), file_list$qtr_number[i], file_list$disease[i], file_list$loc_id[i], file_list$period[i])
   } else if (file_list$type[i]=="gtmb"){
     tmpData <- prep_gtmb_cod_budget(dir, file_list$file_name[i], file_list$sheet[i], ymd(file_list$start_date[i]), file_list$qtr_number[i],file_list$disease[i], file_list$loc_id[i], file_list$period[i])
   } else if (file_list$type[i]=="cat"){
    tmpData <- prep_cod_cat_budget(dir, file_list$file_name[i], file_list$sheet[i], ymd(file_list$start_date[i]), file_list$qtr_number[i],file_list$disease[i], file_list$loc_id[i], file_list$period[i])
   }
  ## replace the "Q1" category with the associated dates that the quarters map to   
  # tmpData1 <- map_quarters(tmpData, ymd(file_list$start_date[i]),file_list$qtr_number[i], file_list$loc_id[i], file_list$period[i],file_list$disease[i], file_list$source[i])
  if(i==1){
    resource_database = tmpData
  } 
  
  if(i>1){
    resource_database = rbind(resource_database, tmpData, use.names=TRUE)
  }
}

resource_database$budget <- as.numeric(resource_database$budget)
resource_database$qtr <- NULL

## since we only have budget data, include exp and disbursed as 0:  
resource_database$expenditures <- 0 
resource_database$disbursed <- 0 

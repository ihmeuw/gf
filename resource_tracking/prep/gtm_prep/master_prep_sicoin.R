# ----------------------------------------------
# Irena Chen
#
# 11/1/2017
# Master prep code that runs all other functions
# The current working directory should be the same as this code


##NOTE: after running lines 42-59, a warning message usually appears: 
##Warning message:
  ## In grep("GUATEM", gf_data$X__13):.N :
  ## numerical expression has 4 elements: only the first used

## This is alright because of the way thesicoin files are set up, but in the future
## this should be revisited to ensure that the data is being handled correctly 


# ----------------------------------------------
# Set up R
library(data.table)
library(reshape2)
library(stringr)
library(readxl)
library(rlang)
library(zoo)
library(lubridate)


# ----------------------------------------------
#define variables: 

## download files from basecamp into a folder on your desktop. 
## You will want to download the files in the multi_source and ghe_s folders, even though we will not be using all of these files 
# (only the ones that contain actual budget/expenditure data and are in c_coin format). 
## 

dir <- 'J:/Project/Evaluation/GF/resource_tracking/gtm/gf/sicoin/hiv/'


# ----------------------------------------------

# load csv from github repository (file_format_list.csv)
file_list <- read.csv("J:/Project/Evaluation/GF/resource_tracking/gtm/gf/sicoin/sicoin_file_list.csv")


source('./prep_sicoin.r')
source('./prep_sicoin_costcat_data.r')
source('./prep_sicoin_ghe.r')

## loop over all of the files 
for(i in 1:length(file_list$file_name)){
  tmpData <- prep_cost_sicoin(as.character(paste0(dir,file_list$file_name[i])), ymd(file_list$start_date[i]), file_list$disease[i], file_list$period[i], file_list$source[i])
  
  if(i==1){
  resource_database = tmpData
  }
  
  if(i>1){
    resource_database = rbind(resource_database, tmpData, use.names=TRUE)
  }
  print(i)
}

resource_database$data_source <- "SICOIN"

##remove rows where loc_ids are in the SDA column: 
loc_ids <- unique(resource_database$loc_id)
cleaned_database <- resource_database[!resource_database$sda_orig%in%loc_ids,]


##output the data to the correct folder 

write.csv(resource_database, "prepped_sicoin_data_1201_ic.csv", row.names=FALSE, fileEncoding="UTF-8")




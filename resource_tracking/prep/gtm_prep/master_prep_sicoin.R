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


# ----------------------------------------------
#define variables: 

## download files from basecamp into a folder on your desktop. 
## You will want to download the files in the multi_source and ghe_s folders, even though we will not be using all of these files 
# (only the ones that contain actual budget/expenditure data and are in c_coin format). 
## 

dir <- 'J:/Project/Evaluation/GF/resource_tracking/gtm/'
period <-365
cost_category <- "All"
loc_id <- "gtm"

# ----------------------------------------------

# load csv from github repository (file_format_list.csv)
file_list <- read.csv("C:/Users/irenac2/repos/gf/resource_tracking/prep/file_format_list_gtm.csv")


source('./prep_sicoin.r')
source('./prep_sicoin_costcat_data.r')
source('./prep_sicoin_ghe.r')

## loop over all of the files 
for(i in 1:length(file_list$filename)){
  ## handles municipality data 
  if(file_list$format[i]=="c_coin_muni"){
  tmpData <- prepSicoin(dir, as.character(paste0(file_list$folder[i],'/',file_list$filename[i])), file_list$year[i], file_list$disease[i], period, cost_category, file_list$source[i], file_list$grant_number[i])
  ## handles cost category data 
  } else if (file_list$format[i]=="c_coin_cost") { 
  tmpData <- prep_cost_sicoin(dir, as.character(paste0(file_list$folder[i],'/',file_list$filename[i])), file_list$year[i], file_list$disease[i], period, file_list$source[i], file_list$grant_number[i], cost_category)
## handles GHE (including GF) expenditure data 
  } else if (file_list$format[i]=="c_coin_ghe") {
  tmpData <- prep_ghe_sicoin(dir, as.character(paste0(file_list$folder[i],'/',file_list$filename[i])), file_list$year[i], file_list$loc_id[i],period, file_list$disease[i], file_list$source[i], file_list$grant_number[i])
  } else if (file_list$format[i]=="c_coin_gf") {
    tmpData <- prep_gf_sicoin(dir, as.character(paste0(file_list$folder[i],'/',file_list$filename[i])), file_list$year[i], file_list$loc_id[i],period, file_list$disease[i], file_list$source[i], file_list$grant_number[i])
  }  else {
  tmpData <- prep_muni_sicoin(dir, as.character(paste0(file_list$folder[i],'/',file_list$filename[i])), file_list$year[i],loc_id, period, file_list$disease[i], file_list$source[i], file_list$grant_number[i])
}
   if(i==1){
    resource_database = tmpData
  }
  if(i>1){
    resource_database = rbind(resource_database, tmpData, use.names=TRUE)
  }
  print(i)
}

resource_database$data_source <- "SICOIN"

##output the data to the correct folder 

write.csv(resource_database, "prepped_sicoin_data_1201_ic.csv", row.names=FALSE, fileEncoding="UTF-8")




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
rm(list=ls())
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

country <- "gtm"
dir <- 'J:/Project/Evaluation/GF/resource_tracking/gtm/'

# ----------------------------------------------

# load csv from github repository (file_format_list.csv)
file_list <- read.csv(paste0(dir, "sicoin_file_list.csv")
                      , stringsAsFactors = FALSE)

##create a summary file to track the data that we have (and that we still need)
summary_file <- setnames(data.table(matrix(nrow = length(file_list$file_name), ncol = 11)), 
                         c("data_source", "source", "year","start_date", "end_date", "sda_detail", 
                           "geographic_detail", "period",	"grant", "disease", "loc_id"))

summary_file$data_source<- as.character(summary_file$data_source)
summary_file$loc_id <- as.character(summary_file$loc_id)
summary_file$loc_id <- country

##source the functions that we will use to prep the files: 
source('./prep_sicoin.r')
source('./prep_sicoin_yearly_data.r')
source('./prep_sicoin_monthly_data.r')

## loop over all of the files 
for(i in 1:length(file_list$file_name)){
  summary_file$disease[i] <- file_list$disease[i]
  summary_file$grant[i] <- file_list$grant[i]
  summary_file$source[i] <- file_list$source[i]
  summary_file$period[i] <- file_list$period[i] 
  summary_file$year[i] <- "N/A"
  summary_file$start_date[i] <- ymd(file_list$start_date[i])
  summary_file$end_date[i] <- ymd(file_list$start_date[i])+file_list$period[i]
  if(file_list$format[i]=="detailed"){
    tmpData <- prep_detailed_sicoin(as.character(paste0(dir,file_list$file_path[i],file_list$file_name[i])), ymd(file_list$start_date[i]), file_list$disease[i], file_list$period[i], file_list$source[i])
  } else if (file_list$format[i]=="summary"){
    tmpData <- prep_summary_sicoin(as.character(paste0(dir,file_list$file_path[i],file_list$file_name[i])), ymd(file_list$start_date[i]), file_list$disease[i], file_list$period[i], file_list$source[i])
  } else if (file_list$format[i]=="blank"){
    tmpData <- prep_blank_sicoin(country, ymd(file_list$start_date[i]), file_list$disease[i], file_list$period[i], file_list$source[i])
  } else if(file_list$format[i]=="donacions"){
    tmpData <- prep_donacions_sicoin(as.character(paste0(dir,file_list$file_path[i],file_list$file_name[i])), ymd(file_list$start_date[i]), file_list$disease[i], file_list$period[i], file_list$source[i])
  }
  if(i==1){
  resource_database = tmpData
  }
  
  if(i>1){
    resource_database = rbind(resource_database, tmpData, use.names=TRUE)
  }
  if(is.na(tmpData$sda_orig[1])){
    summary_file$sda_detail[i] <- "None"
  }else if(!(tmpData$sda_orig[1]=="All")){
    summary_file$sda_detail[i] <- "Summary"
  } else {
    summary_file$sda_detail[i] <- "None"
  }
  
  if((any(!(tmpData$loc_id%in%c("GUATEMALA", country))))){
    summary_file$geographic_detail[i] <- "Municipality"
  } else {
    summary_file$geographic_detail[i] <- "National"
  }

  print(i)
}

summary_file$end_date <- as.Date(summary_file$end_date)
summary_file$start_date <- as.Date(summary_file$start_date)
resource_database$data_source <- "sicoin"
summary_file$data_source <- "sicoin"


setnames(summary_file, c("Data Source",	"Source","Grant Time Frame",	"Start Date", "End Date", "SDA Detail",	"Geographic Detail", "Temporal Detail",	"Grant", "Disease", "Location"))

##export the summary table to J Drive
##(you might get a warning message about appending column names to the files; this should not affect the final output)
write.table(summary_file, "J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/resource_tracking_data_summary.csv",
            col.names=FALSE, append = TRUE, row.names=FALSE, sep=",")


##remove rows where loc_ids are in the SDA column: 
loc_ids <- unique(resource_database$loc_id)
cleaned_database <- resource_database[!resource_database$sda_orig%in%loc_ids,]

##output the data to the correct folder 






write.csv(resource_database, "prepped_sicoin_data.csv", row.names=FALSE, fileEncoding="UTF-8")




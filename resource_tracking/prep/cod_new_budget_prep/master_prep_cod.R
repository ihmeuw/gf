# ----------------------------------------------

# Irena Chen
# Master code file for UGA data prep
### this code will output a prepped and mapped file of the upcoming COD budgets: 
rm(list=ls())
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
##set up some variables: 
loc_id <- 'cod'
implementer <- "CAGF"

####DOWNLOAD THE FOLDER "FPM - grant budgets" from BASECAMP ONTO YOUR LOCAL DRIVE: 

dir <- 'J:/Project/Evaluation/GF/resource_tracking/cod/gf/fpm_budgets/' ##where the files are stored locally
file_list <- read.csv(paste0(dir, "cod_budget_filelist.csv"), na.strings=c("","NA"), stringsAsFactors = FALSE) 
file_list$start_date <- ymd(file_list$start_date)

##create a summary file to track the data that we have (and that we still need)
summary_file <- setnames(data.table(matrix(nrow = length(file_list$file_name), ncol = 10)), 
                         c("data_source", "year","start_date", "end_date", "sda_detail", 
                           "geographic_detail", "period",	"grant", "disease", "loc_id"))

summary_file$loc_id <- as.character(summary_file$loc_id)
summary_file$loc_id <- loc_id

##run the for loop to clean all of the COD data: 
for(i in 1:length(file_list$file_name)){
  ##fill in the summary tracking file with what we know already: 
  summary_file$disease[i] <- file_list$disease[i]
  summary_file$grant[i] <- file_list$grant[i]
  summary_file$period[i] <- file_list$period[i] 
  if(file_list$sr[i]=="unknown"){
    summary_file$geographic_detail[i] <- "National"
  } else {
    summary_file$geographic_detail[i] <- file_list$sr[i]
  }
  summary_file$year[i] <- file_list$grant_time[i]
  
  if(file_list$type[i]=="summary"){
    tmpData <- prep_summary_budget(dir, as.character(file_list$file_name[i]),
                                  file_list$sheet[i], file_list$start_date[i], file_list$qtr_number[i], 
                                  file_list$disease[i], file_list$loc_id[i], file_list$period[i], file_list$grant[i], implementer, file_list$source[i])
  } else if (file_list$type[i]=="detailed"){
    tmpData <- prep_detailed_budget(dir, file_list$file_name[i], file_list$sheet[i], file_list$start_date[i], file_list$qtr_number[i],
                                        file_list$disease[i], file_list$period[i],  file_list$lang[i], file_list$grant[i], loc_id, file_list$source[i])
  } else if(file_list$type[i]=="module"){
    tmpData <- prep_old_module_budget(dir, as.character(file_list$file_name[i]),
                                   file_list$sheet[i], file_list$start_date[i], file_list$qtr_number[i], 
                                   file_list$disease[i], file_list$loc_id[i], file_list$period[i], file_list$grant[i], implementer, file_list$source[i])
  }
  tmpData$source <- "gf"
  if(i==1){
    resource_database = tmpData
  } 
  if(i>1){
    resource_database = rbind(resource_database, tmpData, use.names=TRUE)
  }
  if(file_list$type[i]=="detailed"){
    summary_file$sda_detail[i] <- "Detailed"
  } else if (file_list$type[i]=="summary"){
    summary_file$sda_detail[i] <- "Summary"
  } else if(!(tmpData$sda_activity[1]=="All")){
    summary_file$sda_detail[i] <- "Detailed"
  } else {
    summary_file$sda_detail[i] <- "None"
  }
  summary_file$end_date[i] <- ((max(tmpData$start_date))+file_list$period[i]-1)
  summary_file$start_date[i] <- min(tmpData$start_date)
  summary_file$data_source[i] <- tmpData$data_source[1]
  
  print(i) ## if the code breaks, you know which file it broke on
}

summary_file$start_date <- as.Date(summary_file$start_date)
summary_file$end_date <- as.Date(summary_file$end_date)

resource_database$budget <- as.numeric(resource_database$budget)


setnames(summary_file, c("Data Source",	"Grant Time Frame",	"Data Inventory Start Date", "Data Inventory End Date", 
                         "SDA Detail",	"Geographic Detail", "Temporal Detail",	"Grant", "Disease", "Location"))

## since we only have budget data, include exp and disbursed as 0:  
resource_database$expenditure <- 0 
resource_database$disbursement <- 0 

# ----------------------------------------------


data_check1<- as.data.frame(resource_database[, sum(budget, na.rm = TRUE),by = c("grant_number", "disease")])

## optional: do a check on data to make sure values aren't dropped: 
# data_check2<- as.data.frame(resource_database[, sum(budget, na.rm = TRUE),by = c("grant_number", "disease")])


## do a check on data to make sure values aren't dropped: 
data_check2<- as.data.frame(resource_database[, list(budget = sum(budget, na.rm = TRUE)),by = c("grant_number", "disease")])


## write as csv 
write.csv(resource_database, "J:/Project/Evaluation/GF/resource_tracking/cod/prepped/prepped_fpm_budgets.csv", fileEncoding = "latin1", row.names = FALSE)




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

loc_name <- "gtm"
dir <- 'J:/Project/Evaluation/GF/resource_tracking/gtm/gf/'
file_list <- read.csv(paste0(dir, "fpm/fpm_budget_filelist.csv"))

for(i in 1:length(file_list$file_name)){
  if(file_list$format[i]=="detailed"){ ## fpm detailed budgets 
    tmpData <- prep_fpm_detailed_budget(dir, file_list$file_name[i], as.character(file_list$sheet[i]),
                                        ymd(file_list$start_date[i]), file_list$qtr_number[i], file_list$disease[i], file_list$period[i], 
                                        file_list$lang[i], file_list$grant_number[i])
  } else if (file_list$format[i]=="summary"){ ## only summary level data - no municipalities 
    tmpData <- prep_fpm_summary_budget(dir, file_list$file_name[i], as.character(file_list$sheet[i]),
                                       ymd(file_list$start_date[i]), file_list$qtr_number[i], file_list$disease[i], file_list$period[i], 
                                       file_list$grant_number[i], file_list$recipient[i])
    tmpData$loc_name <- "gtm"
  } else if (file_list$format[i]=="other"){ ## there's an older version of detailed fpm budgets
    tmpData <- prep_other_detailed_budget(dir, file_list$file_name[i], as.character(file_list$sheet[i]),
                                        ymd(file_list$start_date[i]), file_list$qtr_number[i], file_list$disease[i], file_list$period[i], 
                                        file_list$lang[i], file_list$grant_number[i])
  } else if (file_list$format[i]=="pudr"){ 
    tmpData <- prep_gtm_pudr(dir, file_list$file_name[i], as.character(file_list$sheet[i]),
                                          ymd(file_list$start_date[i]), file_list$qtr_number[i], file_list$disease[i], file_list$period[i], 
                                          file_list$grant_number[i], file_list$data_source[i])
    tmpData$recipient <- loc_name ##change this when we get SR info
  }
  tmpData$loc_name <- loc_name
  tmpData$data_source <- file_list$data_source[i]
  
  if(i==1){
    resource_database = tmpData
  }
  if(i>1){
    resource_database = rbind(resource_database, tmpData, use.names=TRUE)
  }
  print(i)
}


resource_database$adm1 <- 128
resource_database$adm2 <- adm1
resource_database$budget <- as.numeric(resource_database$budget)
resource_database$expenditure<- as.numeric(resource_database$expenditure)
## since we only have budget data, include exp and disbursed as 0:  
resource_database$disbursement<- 0 
resource_database$source <- "gf"

# ----------------------------------------------
##check for any dropped data/clean up the sda activities: 
data_check1<- as.data.frame(resource_database[, sum(budget, na.rm = TRUE),by = c("grant_number", "disease")])

# ----------------------------------------------
##output dataset to the correct folder as a csv: 


write.csv(resource_database, "J:/Project/Evaluation/GF/resource_tracking/gtm/prepped/prepped_fpm_pudr.csv", row.names = FALSE,
          fileEncoding = "latin1")








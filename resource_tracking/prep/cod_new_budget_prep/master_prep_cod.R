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

loc_id <- 'cod'
implementer <- "CAGF"

##DOWNLOAD THE FOLDER "FPM - grant budgets" from BASECAMP ONTO YOUR LOCAL DRIVE: 

dir <- 'J:/Project/Evaluation/GF/resource_tracking/cod/gf/7. Grant Making/' ##where the files are stored locally
file_list <- read.csv(paste0(dir, "cod_new_budget.csv"), na.strings=c("","NA")) 

for(i in 1:length(file_list$file_name)){
  if(file_list$type[i]=="summary"){
    tmpData <- prep_cat_summary_budget(dir, as.character(file_list$file_name[i]),
                                  file_list$sheet[i], ymd(file_list$start_date[i]), file_list$qtr_number[i], 
                                  file_list$disease[i], file_list$loc_id[i], file_list$period[i], file_list$grant[i], implementer)
  } else if (file_list$type[i]=="detailed"){
    tmpData <- prep_cod_detailed_budget(dir, file_list$file_name[i], file_list$sheet[i], ymd(file_list$start_date[i]), file_list$qtr_number[i],file_list$disease[i], file_list$period[i],  file_list$lang[i], file_list$grant[i], loc_id)
  } else if (file_list$type[i]=="cat"){
    tmpData <- prep_cod_recip_budget(dir, file_list$file_name[i], file_list$sheet[i], ymd(file_list$start_date[i]), file_list$qtr_number[i],file_list$disease[i], file_list$loc_id[i], file_list$period[i], file_list$lang[i], file_list$grant[i])
  }
  
  if(i==1){
    resource_database = tmpData
  } 
  if(i>1){
    resource_database = rbind(resource_database, tmpData, use.names=TRUE)
  }
  print(i) ## if the code breaks, you know which file it broke on
}

resource_database$budget <- as.numeric(resource_database$budget)

## CHANGE THIS LATER IF WE WANT: tb/hiv map to something else: 

## since we only have budget data, include exp and disbursed as 0:  
resource_database$expenditure <- 0 
resource_database$disbursement <- 0 
resource_database$data_source <- "fpm"

## function to use the activity descriptions to get the program areas we want:  
map_activity_descriptions <- function(program_activity, activity_description){
  if(program_activity%in%c("Gestion des subventions", "Prise en charge",collapse="|")){
    program_activity <- activity_description
  }
  return(program_activity)
}

test_dataset <- copy(resource_database)
test_dataset$cost_category <- mapply(map_activity_descriptions,
                                     resource_database$cost_category, resource_database$activity_description)


test_dataset$cost_category <- gsub("[\u2018\u2019\u201A\u201B\u2032\u2035]", "'", test_dataset$cost_category)

## write as csv 
write.csv(resource_database, "J:/Project/Evaluation/GF/resource_tracking/cod/prepped/new_cod_budgets.csv", fileEncoding = "latin1", row.names = FALSE)




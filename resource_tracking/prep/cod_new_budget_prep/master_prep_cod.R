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

dir <- 'J:/Project/Evaluation/GF/resource_tracking/cod/gf/cod_budget_prep_grants/' ##where the files are stored locally
file_list <- read.csv(paste0(dir, "cod_new_budget.csv"), na.strings=c("","NA"), stringsAsFactors = FALSE) 

##create a summary file to track the data that we have (and that we still need)
summary_file <- setnames(data.table(matrix(nrow = length(file_list$file_name), ncol = 10)), 
                         c("data_source", "year","start_date", "end_date", "sda_detail", 
                           "geographic_detail", "period",	"grant", "disease", "loc_id"))

summary_file$loc_id <- as.character(summary_file$loc_id)
summary_file$loc_id <- loc_id


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
    tmpData <- prep_cat_summary_budget(dir, as.character(file_list$file_name[i]),
                                  file_list$sheet[i], ymd(file_list$start_date[i]), file_list$qtr_number[i], 
                                  file_list$disease[i], file_list$loc_id[i], file_list$period[i], file_list$grant[i], implementer)
  } else if (file_list$type[i]=="detailed"){
    tmpData <- prep_cod_detailed_budget(dir, file_list$file_name[i], file_list$sheet[i], ymd(file_list$start_date[i]), file_list$qtr_number[i],file_list$disease[i], file_list$period[i],  file_list$lang[i], file_list$grant[i], loc_id)
  } else if (file_list$type[i]=="cat"){
    tmpData <- prep_cod_recip_budget(dir, file_list$file_name[i], file_list$sheet[i], ymd(file_list$start_date[i]), file_list$qtr_number[i],file_list$disease[i], file_list$loc_id[i], file_list$period[i], file_list$lang[i], file_list$grant[i])
  }
  tmpData$data_source <- "fpm"
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
  } else if(!(tmpData$cost_category[1]=="All")){
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
resource_database$data_source <- "fpm"
# ----------------------------------------------


data_check1<- as.data.frame(resource_database[, sum(budget, na.rm = TRUE),by = c("grant_number", "disease")])

## function to use the activity descriptions to get the program areas we want:  
map_activity_descriptions <- function(program_activity, activity_description){
  if(program_activity%in%c("Gestion des subventions", "Prise en charge", "Traitement, prise en charge et soutien", collapse="|")){
    program_activity <- activity_description
  }
  return(program_activity)
}

resource_database$sda_orig<- mapply(map_activity_descriptions,
                                     resource_database$sda_orig, resource_database$activity_description)
resource_database$sda_orig<-gsub(paste(c(" ", "[\u2018\u2019\u201A\u201B\u2032\u2035]", "\\\\", "[\r\n]"), collapse="|"), "", resource_database$sda_orig)
resource_database$sda_orig <-tolower(resource_database$sda_orig)
resource_database$sda_orig <- gsub("[[:punct:]]", "", resource_database$sda_orig)
resource_database <- resource_database[!(sda_orig%in%c("6", "4"))]

## optional: do a check on data to make sure values aren't dropped: 
# data_check2<- as.data.frame(resource_database[, sum(budget, na.rm = TRUE),by = c("grant_number", "disease")])


# ----------------------------------------------
## map program level data: 
mapping_for_R <- read.csv(paste0(dir, "mapping_for_R.csv"),
                          fileEncoding="latin1")
mapping_for_graphs <- read.csv(paste0(dir, "mapping_for_graphs.csv"))



# test for missing SDAs from map
sdas_in_map = unique(mapping_for_R$cost_category)
sdas_in_data = unique(resource_database$sda_orig)
if (any(!sdas_in_data %in% sdas_in_map)) { 
  stop('Map doesn\'t include cost categories that are in this data file!')
}
#unmapped_values <- resource_database[sda_orig%in%sdas_in_data[!sdas_in_data %in% sdas_in_map]]
#View(unique(unmapped_values$sda_orig))


# test to make sure map doesn't contain duplicates
d1 = nrow(mapping_for_R)
d2 = nrow(unique(mapping_for_R))
if (d1!=d2) stop('Map contains duplicates!') 


program_level_mapped <- merge(resource_database, mapping_for_R, by=c("disease","sda_orig"), allow.cartesian=TRUE)
mappedCod <- merge(program_level_mapped, mapping_for_graphs, by="code", allow.cartesian=TRUE) ##some categories will be split

mappedCod$budget <- mappedCod$budget*mappedCod$coeff
mappedCod$expenditure <- mappedCod$expenditure*mappedCod$coeff
mappedCod$year <- year(mappedCod$start_date)

## do a check on data to make sure values aren't dropped: 
data_check2<- as.data.frame(mappedCod[, list(budget = sum(budget, na.rm = TRUE)),by = c("grant_number", "disease")])


## write as csv 
write.csv(mappedCod, "J:/Project/Evaluation/GF/resource_tracking/cod/prepped/fpm_cod_budgets.csv", fileEncoding = "latin1", row.names = FALSE)




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

loc_id <- "gtm"
dir <- 'J:/Project/Evaluation/GF/resource_tracking/gtm/gf/fpm/'
file_list <- read.csv("C:/Users/irenac2/repos/gf/resource_tracking/prep/gf_budget_filelist.csv")

for(i in 1:length(file_list$filename)){
  if(file_list$format[i]=="detailed"){
    tmpData <- prep_fpm_detailed_budget(dir, paste0(file_list$filename[i], file_list$extension[i]), as.character(file_list$sheet[i]),
                                        ymd(file_list$start_date[i]), file_list$qtr_number[i], file_list$disease[i], file_list$period[i], 
                                        file_list$lang[i], file_list$grant_number[i])
  } else if (file_list$format[i]=="summary"){
    tmpData <- prep_fpm_summary_budget(dir, paste0(file_list$filename[i], file_list$extension[i]), as.character(file_list$sheet[i]),
                                       ymd(file_list$start_date[i]), file_list$qtr_number[i], file_list$disease[i], file_list$period[i], 
                                       file_list$grant_number[i], file_list$recipient[i])
    tmpData$loc_id <- "gtm"
  } else if (file_list$format[i]=="other"){
    tmpData <- prep_other_detailed_budget(dir, paste0(file_list$filename[i], file_list$extension[i]), as.character(file_list$sheet[i]),
                                        ymd(file_list$start_date[i]), file_list$qtr_number[i], file_list$disease[i], file_list$period[i], 
                                        file_list$lang[i], file_list$grant_number[i])
    tmpData$loc_id <- "gtm"
  }
  
  if(file_list$filename[i]%in%"FR100-GTM-H_DB_INCAP_06ene2018"){
    tmpData$data_source <- "init2_fpm"
  } else if (file_list$filename[i]%in%"FR100-GTM-H_DB_INCAP_06ene2018") {
    tmpData$data_source <- "init_fpm"
  } else {
    tmpData$data_source <- "fpm"
  }
  
  if(i==1){
    resource_database = tmpData
  }
  if(i>1){
    resource_database = rbind(resource_database, tmpData, use.names=TRUE)
  }
  print(i)
}

resource_database$budget <- as.numeric(resource_database$budget)

## since we only have budget data, include exp and disbursed as 0:  
resource_database$expenditure <- 0 
resource_database$disbursement<- 0 


# ----------------------------------------------
data_check1<- as.data.frame(resource_database[, sum(budget, na.rm = TRUE),by = c("grant_number", "disease")])


resource_database$sda_orig<-gsub(paste(c(" ", "[\u2018\u2019\u201A\u201B\u2032\u2035]", "\\\\", "[\r\n]"), 
                                       collapse="|"), "", resource_database$activity_description)
resource_database$sda_orig <-tolower(resource_database$sda_orig)
resource_database$sda_orig <- gsub("[[:punct:]]", "", resource_database$sda_orig)

# ----------------------------------------------
## map program level data: 
mapping_for_R <- read.csv(paste0(dir, "mapping_for_R.csv"),
                          fileEncoding="latin1")
mapping_for_graphs <- read.csv(paste0(dir, "mapping_for_graphs.csv"))


# test for missing SDAs from map
sdas_in_map = unique(mapping_for_R$sda_orig)
sdas_in_data = unique(resource_database$sda_orig)
if (any(!sdas_in_data %in% sdas_in_map)) { 
  stop('Map doesn\'t include cost categories that are in this data file!')
}
#unmapped_values <- resource_database[sda_orig%in%sdas_in_data[!sdas_in_data %in% sdas_in_map]]
#unmapped_values = unmapped_values[!duplicated(unmapped_values, by=c("module", "sda_orig"))]
#View(unique(unmapped_values$sda_orig))



# test to make sure map doesn't contain duplicates
d1 = nrow(mapping_for_R)
d2 = nrow(unique(mapping_for_R))
if (d1!=d2) stop('Map contains duplicates!') 

##map the categories to the standard SDAs: 
program_level_mapped <- merge(resource_database, mapping_for_R, by=c("disease","cost_category"), allow.cartesian=TRUE)
mappedGtm <- merge(program_level_mapped, mapping_for_graphs, by="code", allow.cartesian=TRUE) ##some categories will be split

mappedGtm$budget <- mappedGtm$budget*mappedGtm$coeff
mappedGtm$expenditure <- mappedGtm$expenditure*mappedGtm$coeff

## do a check on data to make sure values aren't dropped: 
data_check2<- as.data.frame(mappedGtm[, list(budget = sum(budget, na.rm = TRUE)),by = c("disease")])


write.csv(mappedGtm, "J:/Project/Evaluation/GF/resource_tracking/gtm/prepped/rejected_budgets.csv", row.names = FALSE,
          fileEncoding = "latin1")








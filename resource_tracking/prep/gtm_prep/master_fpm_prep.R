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
    tmpData$disbursement<- 0 
  } else if (file_list$format[i]=="summary"){ ## only summary level data - no municipalities 
    tmpData <- prep_fpm_summary_budget(dir, file_list$file_name[i], as.character(file_list$sheet[i]),
                                       ymd(file_list$start_date[i]), file_list$qtr_number[i], file_list$disease[i], file_list$period[i], 
                                       file_list$grant_number[i], file_list$recipient[i], file_list$lang[i])
    tmpData$loc_name <- "gtm"
    tmpData$disbursement<- 0 
  } else if (file_list$format[i]=="other"){ ## there's an older version of detailed fpm budgets
    tmpData <- prep_other_detailed_budget(dir, file_list$file_name[i], as.character(file_list$sheet[i]),
                                        ymd(file_list$start_date[i]), file_list$qtr_number[i], file_list$disease[i], file_list$period[i], 
                                        file_list$lang[i], file_list$grant_number[i])
    tmpData$disbursement<- 0 
  } else if (file_list$format[i]=="pudr"){ 
    tmpData <- prep_gtm_pudr(dir, file_list$file_name[i], as.character(file_list$sheet[i]),
                                          ymd(file_list$start_date[i]), file_list$qtr_number[i], file_list$disease[i], file_list$period[i], 
                                          file_list$grant_number[i], file_list$data_source[i], loc_name, file_list$lang[i])
   
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
resource_database$adm2 <- resource_database$adm1
resource_database$budget <- as.numeric(resource_database$budget)
resource_database$expenditure<- as.numeric(resource_database$expenditure)
resource_database$disbursement<- as.numeric(resource_database$disbursement)
## since we only have budget data, include exp and disbursed as 0:  
resource_database$source <- "gf"

# ----------------------------------------------
##check for any dropped data/clean up the sda activities: 
data_check1<- as.data.frame(resource_database[, sum(budget, na.rm = TRUE),by = c("grant_number", "disease")])

# ----------------------------------------------
##### Map to the GF Modules and Interventions #####
##run the map_modules_and_interventions.R script first

gtmData <- strip_chars(resource_database, unwanted_array, remove_chars)
gtmData[is.na(module), module:=intervention]


mapping_list <- load_mapping_list("J:/Project/Evaluation/GF/mapping/multi_country/intervention_categories/intervention_and_indicator_list.xlsx")

## before we get it ready for mapping, copy over so we have the correct punctuation for final mapping: 
final_mapping <- copy(mapping_list)
final_mapping$disease <- NULL
setnames(final_mapping, c("module", "intervention"), c("gf_module", "gf_intervention"))
mapping_list$coefficient <- 1


gf_mapping_list <- total_mapping_list("J:/Project/Evaluation/GF/mapping/multi_country/intervention_categories/intervention_and_indicator_list.xlsx",
                                      mapping_list, unwanted_array, remove_chars)
# ----------------------------------------------
# USE THIS TO CHECK FOR ANY MODULE/INTERVENTION COMBOS IN THE DATA THAT AREN'T IN THE MAPPING
# gf_concat <- paste0(gf_mapping_list$module, gf_mapping_list$intervention)
# gtm_concat <- paste0(gtmData$module, gtmData$intervention)
# unmapped_mods <- gtm_concat[!gtm_concat%in%gf_concat]


gtm_init_mapping <- merge(gtmData, gf_mapping_list, by=c("module", "intervention", "disease"), all.x=TRUE,allow.cartesian = TRUE)

##use this to check if any modules/interventions were dropped:
# dropped_gf <- gtm_init_mapping[is.na(gtm_init_mapping$code)]

mappedGtm <- merge(gtm_init_mapping, final_mapping, by="code")
mappedGtm$budget <- mappedGtm$budget*mappedGtm$coefficient
mappedGtm$expenditure <- mappedGtm$expenditure*mappedGtm$coefficient
mappedGtm$disbursement <- mappedGtm$disbursement*mappedGtm$coefficient

##sum to make sure that budget numbers aren't dropped:

# data_check1 <- gtmData[, sum(budget, na.rm = TRUE),by = c( "module","intervention","disease")]
# data_check2 <-mappedGtm[, sum(budget, na.rm = TRUE),by = c("module", "intervention","disease")]

mappedGtm$year <- year(mappedGtm$start_date)

# ----------------------------------------------

##output dataset to the correct folder as a csv: 


write.csv(mappedGtm, "J:/Project/Evaluation/GF/resource_tracking/gtm/prepped/prepped_fpm_pudr.csv", row.names = FALSE,
          fileEncoding = "latin1")








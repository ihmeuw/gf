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


##create a summary file to track the data that we have (and that we still need)
summary_file <- setnames(data.table(matrix(nrow = length(file_list$file_name), ncol = 10)), 
                         c("data_source","year", "start_date",  "end_date", "sda_detail",
                           "geographic_detail", "period",	"grant", "disease", "loc_name"))

summary_file$loc_name <- as.character(summary_file$loc_name)
summary_file$loc_name <- loc_name


for(i in 1:length(file_list$file_name)){
  ##fill in the summary tracking file with what we know already: 
  summary_file$disease[i] <- as.character(file_list$disease[i])
  summary_file$grant[i] <- as.character(file_list$grant[i])
  summary_file$period[i] <- file_list$period[i] 
  summary_file$geographic_detail[i] <- as.character(file_list$geography_detail[i])
  summary_file$data_source[i] <- as.character(file_list$data_source[i])
  
  
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
  
  if(file_list$format[i]=="detailed"){
    summary_file$sda_detail[i] <- "Detailed"
  } else if (file_list$format[i]=="summary"){
    summary_file$sda_detail[i] <- "Summary"
  } else if(!(tmpData$sda_activity[1]=="All")){
    summary_file$sda_detail[i] <- "Detailed"
  } else {
    summary_file$sda_detail[i] <- "None"
  }
  summary_file$end_date[i] <- ((max(tmpData$start_date))+file_list$period[i]-1)
  summary_file$start_date[i] <- min(tmpData$start_date) ##since there are multiple values in this, get the earliest start date 
  
  
  print(i)
}


summary_file$end_date <- as.Date(summary_file$end_date)
summary_file$start_date <- as.Date(summary_file$start_date)
resource_database$start_date <- as.Date(resource_database$start_date)


setnames(summary_file, c("Data Source",	"Grant Time Frame",	"Start Date", "End Date", "SDA Detail",	"Geographic Detail", "Temporal Detail",	"Grant", "Disease", "Location"))

##export the summary table to J Drive
##(you might get a warning message about appending column names to the files; this should not affect the final output)
write.table(summary_file, "J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/resource_tracking_data_summary.csv",
            append = TRUE, row.names=FALSE, sep=",")

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








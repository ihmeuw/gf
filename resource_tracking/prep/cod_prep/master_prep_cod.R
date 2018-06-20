# ----------------------------------------------

# Irena Chen
## May 15, 2018
# Master code file for DRC data prep
# ----------------------------------------------
###### Set up R / install packages  ###### 
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
##set up some variables: 
# ----------------------------------------------
loc_name <- 'cod' ##use the ISO3 country code for DRC
implementer <- "CAGF"


# ----------------------------------------------
## Notes: running this will throw a warning: 
#Warning messages:
#In read_fun(path = path, sheet = sheet, limits = limits, shim = shim,  :
#              NA inserted for impossible 1900-02-29 datetime

#But this shouldn't affect the final output. 

# ----------------------------------------------
##STEP 1: Download the "Resource Tracking Data" folder from Basecamp and save it on your local drive 
# ----------------------------------------------
dir <- 'filepath here' ##where the files are stored locally

# ----------------------------------------------
###### Load the list of the RT files we want to process   ###### 
# ----------------------------------------------

file_list <- read.csv(paste0(dir, "cod_budget_filelist.csv"), na.strings=c("","NA"), stringsAsFactors = FALSE) 
file_list$start_date <- ymd(file_list$start_date)

##create a summary file to track the data that we have (and that we still need)
summary_file <- setnames(data.table(matrix(nrow = length(file_list$file_name), ncol = 10)), 
                         c("data_source", "year","start_date", "end_date", "sda_detail", 
                           "geographic_detail", "period",	"grant", "disease", "loc_id"))

summary_file$loc_id <- as.character(summary_file$loc_id)
summary_file$loc_id <- loc_name

# ----------------------------------------------
###### For loop that preps data and aggregates it
# ----------------------------------------------
for(i in 1:length(file_list$file_name)){
  ##fill in the summary tracking file with what we know already: 
  summary_file$disease[i] <- file_list$disease[i]
  summary_file$grant[i] <- file_list$grant[i]
  summary_file$period[i] <- file_list$period[i] 
  summary_file$year[i] <- file_list$grant_period[i]
  if(file_list$sr[i]=="unknown"){
    summary_file$geographic_detail[i] <- "National"
  } else {
    summary_file$geographic_detail[i] <- file_list$sr[i]
  }
  
  if(file_list$type[i]=="summary"){
    tmpData <- prep_summary_budget(dir, as.character(file_list$file_name[i]),
                                  file_list$sheet[i], file_list$start_date[i], file_list$qtr_number[i], 
                                  file_list$disease[i], file_list$loc_id[i], file_list$period[i]
                                  , file_list$grant[i], implementer, file_list$source[i], file_list$lang[i])
    tmpData$year <- year(tmpData$start_date)
    tmpData$data_source <- file_list$source[i]
   } else if (file_list$type[i]=="detailed"){
    tmpData <- prep_detailed_budget(dir, file_list$file_name[i], file_list$sheet[i], file_list$start_date[i], file_list$qtr_number[i],
                                        file_list$disease[i], file_list$period[i],  file_list$lang[i], file_list$grant[i], loc_name, file_list$source[i])
    tmpData$year <- year(tmpData$start_date)
    tmpData$data_source <- file_list$source[i]
  } else if(file_list$type[i]=="module"){
    tmpData <- prep_old_module_budget(dir, as.character(file_list$file_name[i]),
                                   file_list$sheet[i], file_list$start_date[i], file_list$qtr_number[i], 
                                   file_list$disease[i], file_list$loc_id[i], file_list$period[i]
                                   , file_list$grant[i], implementer, file_list$source[i], file_list$lang[i])
    tmpData$year <- year(tmpData$start_date)
    tmpData$data_source <- file_list$source[i]
  } else if(file_list$type[i]=="rejected"){
    tmpData <- prep_cod_rejected(paste0(dir, file_list$file_name[i]))
    tmpData$data_source <- "iterated_fpm"
    
  }  else if (file_list$type[i]=="pudr"){ ##has expenditure data 
    tmpData <- prep_pudr_cod(dir, file_list$file_name[i], as.character(file_list$sheet[i]), 
                             file_list$start_date[i], file_list$disease[i], file_list$period[i], 
                             file_list$grant[i], file_list$sr[i],file_list$data_source[i], file_list$lang[i], loc_name)
  tmpData$data_source <- "pudr"
  } else if (file_list$type[i]=="old_detailed"){
    tmpData <- prep_old_detailed_budget(dir, file_list$file_name[i], file_list$sheet[i], file_list$start_date[i], file_list$qtr_number[i],
                                    file_list$disease[i], file_list$period[i],  file_list$lang[i], file_list$grant[i], loc_name, file_list$source[i],
                                    file_list$pr[i])
    tmpData$year <- year(tmpData$start_date)
    tmpData$data_source <- file_list$source[i]
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

setnames(summary_file, c("Data Source",	"Grant Time Frame",	"Data Inventory Start Date", "Data Inventory End Date", 
                         "SDA Detail",	"Geographic Detail", "Temporal Detail",	"Grant", "Disease", "Location"))


# ---------------------------------------------
##export the summary table
# ---------------------------------------------
write.table(summary_file, paste0("file path where you want the summary file","resource_tracking_data_summary.csv"),
            append = TRUE, row.names=FALSE, sep=",")

# ---------------------------------------------
########## Modify the prepped data variables as necessary ########
# ---------------------------------------------


## since we only have budget and exp, set disbursed as 0:  
resource_database$budget <- as.numeric(resource_database$budget)
resource_database$expenditure <- as.numeric(resource_database$expenditure)
resource_database$disbursement <- 0 

resource_database <- resource_database[!(module%in%c("6", "4"))]

# ----------------------------------------------
######## Optional: do a data check for dropped values ########
# ----------------------------------------------
# data_check<- resource_database[, sum(budget, na.rm = TRUE),by = c("grant_number","data_source","year", "disease")]

# ----------------------------------------------
##### Load the mapping files  #####
##run the map_modules_and_interventions.R script first
# ----------------------------------------------

codData <- strip_chars(resource_database, unwanted_array, remove_chars)
mapping_list <- load_mapping_list(paste0(dir, "intervention_and_indicator_list.xlsx"),
                                  include_rssh_by_disease = FALSE)

## before we get it ready for mapping, copy over so we have the correct punctuation for final mapping: 
final_mapping <- copy(mapping_list)
final_mapping$disease <- NULL
setnames(final_mapping, c("module", "intervention"), c("gf_module", "gf_intervention"))
mapping_list$coefficient <- 1

gf_mapping_list <- total_mapping_list(paste0(dir,"intervention_and_indicator_list.xlsx"),
                                      mapping_list, unwanted_array, remove_chars)


# ----------------------------------------------
########### USE THIS TO CHECK FOR UNMAPPES MODULE/INTERVENTIONS ##########
# ----------------------------------------------
# gf_concat <- paste0(gf_mapping_list$module, gf_mapping_list$intervention)
# cod_concat <- paste0(codData$module, codData$intervention)
# unmapped_mods <- cod_concat[!cod_concat%in%gf_concat]

# ----------------------------------------------
########### map the RT data to the GF modular framework ##########
# ----------------------------------------------
cod_init_mapping <- merge(codData, gf_mapping_list, by=c("module", "intervention", "disease"), all.x=TRUE,allow.cartesian = TRUE)

##use this to check if any modules/interventions were dropped:
# dropped_gf <- cod_init_mapping[is.na(cod_init_mapping$code)]

mappedCod <- merge(cod_init_mapping, final_mapping, by="code")
mappedCod$budget <- mappedCod$budget*mappedCod$coefficient
mappedCod$expenditure <- mappedCod$expenditure*mappedCod$coefficient
mappedCod$disbursement <- mappedCod$disbursement*mappedCod$coefficient


# ----------------------------------------------
######## Optional: sum to make sure that budget numbers aren't dropped ######## 
# ----------------------------------------------
#mappedCod$concat <- NULL
# data_check1 <- codData[, sum(budget, na.rm = TRUE),by = c( "module","intervention","disease")]
# data_check2 <-mappedCod[, sum(budget, na.rm = TRUE),by = c("module", "intervention","disease")]
# data_check1[!module%in%data_check2$module]
# data_check1$ind <- "pre"
# data_check2$ind <- "post"
# data_check <- rbind(data_check1, data_check2)
# write.csv(data_check, "data_check.csv", row.names = FALSE)



##change this when we get geo locations for DRC: 
mappedCod$adm1 <- 171
mappedCod$adm2 <- 171
mappedCod$country <- "Congo (Democratic Republic)"


# ----------------------------------------------
## write as csv 
# ----------------------------------------------
write.csv(mappedCod, paste0(dir, "prepped_fpm_budgets.csv"), fileEncoding = "latin1", row.names = FALSE)




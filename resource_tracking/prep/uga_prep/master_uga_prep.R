# ----------------------------------------------

# Irena Chen
# Master code file for UGA data prep
###
rm(list=ls())
library(lubridate)
library(data.table)
library(readxl)
library(stats)
library(stringr)
library(rlang)
library(zoo)
# ----------------------------------------------
## STEP 1: Download the prep_budget_data folder from UGA Basecamp and save somewhere on your local drive: 
##this has all of the files we will be using: 
## Notes: running this will throw a warning: 
#Warning messages:
#In read_fun(path = path, sheet = sheet, limits = limits, shim = shim,  :
#              NA inserted for impossible 1900-02-29 datetime

#But this shouldn't affect the final output. 

# ---------------------------------------------
##global variables: 
cashText <- " Cash Outflow"
loc_id <- 'uga'
source <- "gf"

## set up the directory and grab the file list: 
dir <- 'your local drive here' ##where the files are stored locally
file_list <- read.csv(paste0(dir, "uga_budget_file_list.csv"), na.strings=c("","NA"),
                      stringsAsFactors = FALSE) 
file_list$start_date <- ymd(file_list$start_date)

##create a summary file to track the data that we have (and that we still need)
summary_file <- setnames(data.table(matrix(nrow = length(file_list$file_name), ncol = 10)), 
                         c("data_source","year", "start_date",  "end_date", "sda_detail",
                           "geographic_detail", "period",	"grant", "disease", "loc_id"))

summary_file$loc_id <- as.character(summary_file$loc_id)
summary_file$loc_id <- loc_id



for(i in 1:length(file_list$file_name)){ 
  ##fill in the summary tracking file with what we know already: 
  summary_file$disease[i] <- file_list$disease[i]
  summary_file$grant[i] <- file_list$grant[i]
  summary_file$period[i] <- file_list$period[i] 
  summary_file$geographic_detail[i] <- "National"
  summary_file$data_source[i] <- file_list$data_source[1]
  
  if(file_list$type[i]=="detailed"){##most detailed level of budgets 
    tmpData <- prep_detailed_uga_budget(dir, file_list$file_name[i], as.character(file_list$sheet[i]), 
                                       file_list$start_date[i], file_list$qtr_number[i], cashText, file_list$grant[i], 
                                        file_list$disease[i], file_list$period[i],file_list$data_source[i])
  } else if (file_list$type[i]=="summary"){ ##not much detail, only high level SDAs: 
    tmpData <- prep_summary_uga_budget(dir, file_list$file_name[i], as.character(file_list$sheet[i]), 
                                       file_list$start_date[i], file_list$qtr_number[i], cashText, file_list$grant[i], 
                                       file_list$disease[i], file_list$period[i], file_list$recipient[i], file_list$data_source[i])
    tmpData$disbursement <- 0 
  ##LFA data cleaning: 
  } else if (file_list$type[i]=="pudr"){ ##has expenditure data 
    tmpData <- prep_pudr_uga(dir, file_list$file_name[i], as.character(file_list$sheet[i]), 
                             file_list$start_date[i], file_list$disease[i], file_list$period[i], 
                             file_list$grant[i], file_list$recipient[i],file_list$data_source[i])
  }
  if(i==1){
    resource_database = tmpData
  } 
  if(i>1){
    resource_database = rbind(resource_database, tmpData, use.names=TRUE)
  }
  tmpData$start_date <- as.Date(tmpData$start_date,  "%Y-%m-%d")
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
  
  print(i)
}

summary_file$end_date <- as.Date(summary_file$end_date)
summary_file$start_date <- as.Date(summary_file$start_date)
resource_database$start_date <- as.Date(resource_database$start_date)


setnames(summary_file, c("Data Source",	"Year",	"Start Date", "End Date", "SDA Detail",	"Geographic Detail", "Temporal Detail",	"Grant", "Disease", "Location"))

##export the summary table to J Drive
##(you might get a warning message about appending column names to the files; this should not affect the final output)
write.table(summary_file, "J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/resource_tracking_data_summary.csv",
            append = TRUE, row.names=FALSE, sep=",")


##make sure to change the budget variable type to be "numeric" 
resource_database$budget <- as.numeric(resource_database$budget)
resource_database$expenditure <- as.numeric(resource_database$expenditure)
resource_database$disbursement <- as.numeric(resource_database$disbursement)
## since we only have budget/exp data, include disbursed as 0:  
resource_database$loc_id <- loc_id
resource_database$source <- source


## optional: do a check on data to make sure values aren't dropped: 
# data_check1<- as.data.frame(resource_database[, sum(budget, na.rm = TRUE),by = c("grant_number", "disease")])

## we have some junk "modules"
toMatch <- c("0", "Please sel", "PA")
cleaned_database <- resource_database[!grepl(paste(toMatch, collapse="|"), resource_database$module),]

# ---------------------------------------------
###DUPLICATE CHECK: 

d1 <- nrow(cleaned_database)
d2 <- nrow(unique(cleaned_database))
if(d1 !=d2){
  stop("Dataset has duplicates!")
}

## one reason we might have duplicates is because of the activity description: same "umbrella" module, but maybe the activities aren't:
##commented code below sums duplicates up:
# byVars = names(cleaned_database)[!names(cleaned_database)%in%c('budget', 'disbursement', 'expenditure')]
# cleaned_database <- cleaned_database[, list(budget=sum(na.omit(budget)),
#                                             expenditure=sum(na.omit(expenditure)), disbursement=sum(na.omit(disbursement))), by=byVars]

## data check to verify data hasn't been dropped: 
# data_check3 <- as.data.frame(cleaned_database[, list(budget = sum(budget, na.rm = TRUE)),by = c("grant_number", "disease")])

# ---------------------------------------------
## map program level data: 
mapping_for_R <- read.csv(paste0(dir, "mapping_for_R.csv"),
                          fileEncoding="latin1")
mapping_for_graphs <- read.csv(paste0(dir, "mapping_for_graphs.csv"))

## get rid of spaces, special characters, unnecessary punctuation....
resource_database$cost_category <-gsub(paste(c(" ", "[\u2018\u2019\u201A\u201B\u2032\u2035]"), collapse="|"), "", resource_database$cost_category)
resource_database$cost_category <-tolower(resource_database$cost_category)
resource_database$cost_category <- gsub("[[:punct:]]", "", resource_database$cost_category)


## split hiv/tb into hiv or tb: 

get_hivtb_split <- function(disease, cost_category){
  x <- disease
 if(disease=="hiv/tb"){
   if(grepl(paste(c("tb", "tuber"), collapse="|"), cost_category)){
    x <- "tb"
  } else {
    x <- "hiv"
  }
 }
return(x)
}

resource_database$disease <- mapply(get_hivtb_split, resource_database$disease, resource_database$cost_category)
resource_database$disease <- as.factor(resource_database$disease)


# test for missing SDAs from map
sdas_in_map = unique(mapping_for_R$cost_category)
sdas_in_data = unique(resource_database$cost_category)
if (any(!sdas_in_data %in% sdas_in_map)) { 
  stop('Map doesn\'t include cost categories that are in this data file!')
}

# uncomment if there are unmapped cost categories - figure out which ones: 
# unmapped_values <- resource_database[cost_category%in%sdas_in_data[!sdas_in_data %in% sdas_in_map]]
# unique(unmapped_values$cost_category)

## if necessary: resource_database <- resource_database[!cost_category=="pleaseselect."]

# test to make sure map doesn't contain duplicates
d1 = nrow(mapping_for_R)
d2 = nrow(unique(mapping_for_R))
if (d1!=d2) stop('Map contains duplicates!') 

# ---------------------------------------------
##map program activities from GOS data to our standard categories:

program_level_mapped <- merge(resource_database, mapping_for_R, by=c("disease","cost_category"), allow.cartesian=TRUE)
mappedUga <- merge(program_level_mapped, mapping_for_graphs, by="code", allow.cartesian=TRUE) ##some categories will be split

mappedUga$budget <- mappedUga$budget*mappedUga$coeff
mappedUga$expenditure <- mappedUga$expenditure*mappedUga$coeff

## do a check on data to make sure values aren't dropped: 
data_check3 <- as.data.frame(mappedUga[, list(budget = sum(budget, na.rm = TRUE)),by = c("grant_number", "disease")])

##write csv to correct folder: 

write.csv(mappedUga, "J:/Project/Evaluation/GF/resource_tracking/uga/prepped/fpm_prepped_budgets.csv", row.names = FALSE,
          fileEncoding = "latin1")





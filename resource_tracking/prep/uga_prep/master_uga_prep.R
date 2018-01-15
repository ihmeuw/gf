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
dir <- 'J:/Project/Evaluation/GF/resource_tracking/uga/gf/' ##where the files are stored locally
file_list <- read.csv("C:/Users/irenac2/repos/gf/resource_tracking/prep/uga_prep/uga_budget_file_list.csv", na.strings=c("","NA"),
                      stringsAsFactors = FALSE) 

for(i in 1:length(file_list$file_name)){ ##most detailed level of budgets 
  if(file_list$type[i]=="detailed"){
    tmpData <- prep_detailed_uga_budget(dir, file_list$file_name[i], as.character(file_list$sheet[i]), 
                                        ymd(file_list$start_date[i]), file_list$qtr_number[i], cashText, file_list$grant[i], 
                                        file_list$disease[i], file_list$period[i],file_list$source[i])
    tmpData$data_source <- "fpm"
  } else if (file_list$type[i]=="summary"){ ##not much detail, only high level SDAs: 
    tmpData <- prep_summary_uga_budget(dir, file_list$file_name[i], as.character(file_list$sheet[i]), 
                                       ymd(file_list$start_date[i]), file_list$qtr_number[i], cashText, file_list$grant[i], 
                                       file_list$disease[i], file_list$period[i], file_list$recipient[i], file_list$source[i])
    tmpData$start_date <- ymd(tmpData$start_date)
    tmpData$data_source <- "fpm"
  ##LFA data cleaning: 
  } else if (file_list$type[i]=="pudr"){ ##has expenditure data 
    tmpData <- prep_pudr_uga(dir, file_list$file_name[i], as.character(file_list$sheet[i]), 
                             ymd(file_list$start_date[i]), file_list$disease[i], file_list$period[i], 
                             file_list$grant[i], file_list$recipient[i],file_list$source[i])
    tmpData$data_source <- "pudr"
    }
  if(i==1){
    resource_database = tmpData
  } 
  if(i>1){
    resource_database = rbind(resource_database, tmpData, use.names=TRUE)
  }
  print(i)
}

##make sure to change the budget variable type to be "numeric" 
resource_database$budget <- as.numeric(resource_database$budget)
resource_database$expenditure <- as.numeric(resource_database$expenditure)
## since we only have budget/exp data, include disbursed as 0:  
resource_database$disbursement <- 0 
resource_database$loc_id <- loc_id

###MAPPING CODE TO FOLLOW BELOW: 

# ---------------------------------------------
## map program level data: 
mapping_for_R <- read.csv("C:/Users/irenac2/repos/gf/resource_tracking/prep/mapping_for_R.csv",
                          fileEncoding="latin1")
mapping_for_graphs <- read.csv("J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/mapping_for_graphs.csv")

## do a check on data to make sure values aren't dropped: 
data_check1<- as.data.frame(resource_database[, sum(budget, na.rm = TRUE),by = c("grant_number", "disease")])

## get rid of spaces, special characters, unnecessary punctuation....
resource_database$stripped <-gsub(paste(c(" ", "[\u2018\u2019\u201A\u201B\u2032\u2035]"), collapse="|"), "", resource_database$cost_category)
resource_database$stripped <-tolower(resource_database$stripped)
resource_database$stripped <- gsub("[[:punct:]]", "", resource_database$stripped)

## we have some junk "cost categories"
resource_database <- resource_database[!grepl(paste("0", "pleaseselect", sep="|"), resource_database$stripped),]
resource_database$cost_category <- resource_database$stripped
resource_database$stripped <- NULL 

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
data_check2<- as.data.frame(mappedUga[, sum(budget, na.rm = TRUE),by = c("grant_number", "disease")])

##write csv to correct folder: 

write.csv(mappedUga, "J:/Project/Evaluation/GF/resource_tracking/uga/prepped/all_fpm_mapped_budgets.csv", row.names = FALSE,
          fileEncoding = "latin1")





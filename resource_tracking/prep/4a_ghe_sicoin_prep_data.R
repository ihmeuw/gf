# ----------------------------------------------TESTING
# Irena Chen
#
# 11/1/2017
# Master prep code that runs all other functions
# The current working directory should be the same as this code
##NOTE: after running lines 42-59, a warning message usually appears: 
##Warning message:
  ## In grep("GUATEM", gf_data$X__13):.N :
  ## numerical expression has 4 elements: only the first used

## This is alright because of the way the sicoin files are set up, but in the future
## this should be revisited to ensure that the data is being handled correctly 
# ----------------------------------------------
# Set up R
rm(list=ls())

#specify the packages of interest
packages = c("data.table","reshape2","stringr","readxl", "zoo",
             "rlang", "zoo", "lubridate")

#use this function to check if each package is on the local machine
#if a package is installed, it will be loaded
#if any are not, the missing package(s) will be installed and loaded
package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

# ----------------------------------------------
#define variables: 
# ----------------------------------------------
adm1 <- 0100
country <- "gtm"

# ----------------------------------------------
###### source the functions that we need 
# ----------------------------------------------
prep_dir <-" your local repo + gf/resource_tracking/prep/"
prep_dir = "H:/gf/resource_tracking/prep/"
source(paste0(prep_dir, "gtm_prep/prep_sicoin/prep_sicoin_detailed_data.R"))
source(paste0(prep_dir, "gtm_prep/prep_sicoin/prep_sicoin_summary_data.R"))
source(paste0(prep_dir, "gtm_prep/prep_sicoin/prep_sicoin_blank_data.R"))
source(paste0(prep_dir, "gtm_prep/prep_sicoin/prep_sicoin_donacions_data.R"))
source(paste0(prep_dir, "gtm_prep/prep_sicoin/prep_sicoin_report_data.R"))
source(paste0(prep_dir, "map_modules_and_interventions.R"))

# ----------------------------------------------
###### source the functions that we need 
# ----------------------------------------------
# load the list of sicoin files 
dir <- 'J:/Project/Evaluation/GF/resource_tracking/gtm/'
file_list <- read.csv(paste0(dir, "sicoin_file_list.csv")
                      , stringsAsFactors = FALSE)

##create a summary file to track the data that we have (and that we still need)
summary_file <- setnames(data.table(matrix(nrow = length(file_list$file_name), ncol = 11)), 
                         c("data_source", "source", "year","start_date", "end_date", "sda_detail", 
                           "geographic_detail", "period",	"grant", "disease", "loc_id"))

summary_file$data_source<- as.character(summary_file$data_source)
summary_file$loc_id <- as.character(summary_file$loc_id)
summary_file$loc_id <- country
summary_file$year <- as.character(summary_file$year)
summary_file$year <- "none"

## loop over all of the files 
for(i in 1:length(file_list$file_name)){
  summary_file$disease[i] <- file_list$disease[i]
  summary_file$grant[i] <- file_list$grant[i]
  summary_file$source[i] <- file_list$source[i]
  summary_file$period[i] <- file_list$period[i] 
  summary_file$year[i] <- "none"
  summary_file$start_date[i] <- ymd(file_list$start_date[i])
  summary_file$end_date[i] <- ymd(file_list$start_date[i])+file_list$period[i]
  if(file_list$format[i]=="detailed"){
    tmpData <- prep_detailed_sicoin(as.character(paste0(dir,file_list$file_path[i],file_list$file_name[i])), ymd(file_list$start_date[i]), file_list$disease[i], file_list$period[i], file_list$source[i])
  } else if (file_list$format[i]=="summary"){
    tmpData <- prep_summary_sicoin(as.character(paste0(dir,file_list$file_path[i],file_list$file_name[i])), ymd(file_list$start_date[i]), file_list$disease[i], file_list$period[i], file_list$source[i])
  } else if (file_list$format[i]=="blank"){
    tmpData <- prep_blank_sicoin(country, adm1, ymd(file_list$start_date[i]), file_list$disease[i], file_list$period[i], file_list$source[i])
  } else if(file_list$format[i]=="donacions"){
    tmpData <- prep_donacions_sicoin(as.character(paste0(dir,file_list$file_path[i],file_list$file_name[i])),
                                     ymd(file_list$start_date[i]), file_list$disease[i], file_list$period[i], file_list$source[i],
                                     country,  adm1)
  } else if (file_list$format[i]=="report"){
    tmpData <- prep_report_sicoin(as.character(paste0(dir,file_list$file_path[i],file_list$file_name[i])), ymd(file_list$start_date[i]), file_list$disease[i], file_list$period[i], file_list$source[i])
  }
  tmpData$fileName =  file_list$file_name[i]
  if(i==1){
  resource_database = tmpData
  }
  
  if(i>1){
    resource_database = rbind(resource_database, tmpData, use.names=TRUE)
  }
  ##add info to the summary tracking file:
  if(is.na(tmpData$sda_orig[1])){
    summary_file$sda_detail[i] <- "None"
  }else if(!(tmpData$sda_orig[1]=="All")){
    summary_file$sda_detail[i] <- "Summary"
  } else {
    summary_file$sda_detail[i] <- "None"
  }
  
  if((any(!(tmpData$loc_id%in%c("GUATEMALA", country))))){
    summary_file$geographic_detail[i] <- "Municipality"
  } else {
    summary_file$geographic_detail[i] <- "National"
  }

  print(i)
}

summary_file$end_date <- as.Date(summary_file$end_date)
summary_file$start_date <- as.Date(summary_file$start_date)
resource_database$data_source <- "sicoin"
summary_file$data_source <- "sicoin"


setnames(summary_file, c("Data Source",	"Source","Grant Time Frame",	"Start Date", "End Date", "SDA Detail",	"Geographic Detail", "Temporal Detail",	"Grant", "Disease", "Location"))

##export the summary table to J Drive
##(you might get a warning message about appending column names to the files; this should not affect the final output)
write.table(summary_file, "J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/resource_tracking_data_summary.csv",
            col.names=FALSE, append = TRUE, row.names=FALSE, sep=",")


# ---------------------------------------------
##########  Clean up the data/duplicate check ########
# ---------------------------------------------


##remove rows where loc_ids are in the SDA column: 
cleaned_database <- resource_database[!resource_database$loc_name%in%"REGISTRO, CONTROL Y VIGILANCIA DE LA MALARIA"]
cleaned_database <-cleaned_database[!(cleaned_database$adm1%in%"TOTAL"|cleaned_database$adm2%in%"TOTAL")]
setnames(cleaned_database, "sda_orig", "module")

##adding a column to track original language: 
cleaned_database$lang <- "esp"
##output the data to the correct folder 

cleaned_database[is.na(module), module:="all"]
cleaned_database$intervention <- "all"

##check for duplicates:

dups<-cleaned_database[duplicated(cleaned_database) | duplicated(cleaned_database, fromLast=TRUE)]

#EMILY THIS CAN'T HAPPEN. 
# Convert from Quitzal to USD
conversion_table = data.table("year" = c("2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018"), 
                              "conversion" = c(7.74022,	7.5532,	7.3301,	7.45875,	7.42153,	8.01039,	7.92282,	7.64965,	7.68406,	7.71407,	7.59794,	7.49704,	7.43533,	7.18309,	7.31697))

cleaned_database$year = substring(cleaned_database$start_date, 1, 4)
cleaned_database = merge(cleaned_database, conversion_table, by = "year", allow.cartesian = TRUE)
cleaned_database$budget = cleaned_database$budget / cleaned_database$conversion
cleaned_database$disbursement = cleaned_database$disbursement / cleaned_database$conversion

cleaned_database$year = NULL
cleaned_database$conversion = NULL

# ----------------------------------------------
##### Load the mapping files  #####
# ----------------------------------------------

sicoin_data <- strip_chars(cleaned_database, unwanted_array, remove_chars)

mapping_list <- load_mapping_list("J:/Project/Evaluation/GF/mapping/multi_country/intervention_categories/intervention_and_indicator_list.xlsx",
                                  include_rssh_by_disease = FALSE)

## before we get it ready for mapping, copy over so we have the correct punctuation for final mapping: 
final_mapping <- copy(mapping_list)
final_mapping$disease <- NULL
setnames(final_mapping, c("module", "intervention"), c("gf_module", "gf_intervention"))
mapping_list$coefficient <- 1
mapping_list$abbrev_intervention <- NULL
mapping_list$abbrev_module<- NULL

gf_mapping_list <- total_mapping_list("J:/Project/Evaluation/GF/mapping/multi_country/intervention_categories/intervention_and_indicator_list.xlsx",
                                      mapping_list, unwanted_array, remove_chars)


sicoin_init_mapping <- merge(sicoin_data, gf_mapping_list, by=c("module", "intervention", "disease"), all.x=TRUE,allow.cartesian = TRUE)

##use this to check if any modules/interventions were dropped:
# dropped_sicoin<- sicoin_init_mapping[is.na(sicoin_init_mapping$code)]

mapped_sicoin <- merge(sicoin_init_mapping, final_mapping, by="code")
mapped_sicoin$budget <- mapped_sicoin$budget*mapped_sicoin$coefficient
mapped_sicoin$expenditure <- mapped_sicoin$expenditure*mapped_sicoin$coefficient
mapped_sicoin$disbursement <- mapped_sicoin$disbursement*mapped_sicoin$coefficient
mapped_sicoin$cost_category <- "all"
mapped_sicoin$sda_activity <- "Unspecified (Summary budget)"
mapped_sicoin$grant_number <- "none"
mapped_sicoin$country <- "Guatemala"
mapped_sicoin$recipient <- mapped_sicoin$country
mapped_sicoin$data_source <- "sicoin"
mapped_sicoin$lang <- "esp"
mapped_sicoin$year <- year(mapped_sicoin$start_date)
mapped_sicoin$grant_number <- "none"
mapped_sicoin$loc_name = "gtm"

mapped_sicoin$financing_source = ifelse(mapped_sicoin$financing_source == "donacions", "other_dah", as.character(mapped_sicoin$financing_source))
# data_check1 <- sicoin_data[, sum(budget, na.rm = TRUE),by = c( "module","intervention","disease")]
# data_check2 <-mapped_sicoin[, sum(budget, na.rm = TRUE),by = c("module", "intervention","disease")]

write.csv(mapped_sicoin, "J:/Project/Evaluation/GF/resource_tracking/gtm/prepped/prepped_sicoin_data.csv"
          ,row.names=FALSE, fileEncoding="latin1")








# ----------------------------------------------

# Irena Chen
# Master code file for UGA data prep
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
## STEP 1: Download the prep_budget_data folder from UGA Basecamp and save somewhere on your local drive: 
##this has all of the files we will be using: 
## Notes: running this will throw a warning: 
#Warning messages:
#In read_fun(path = path, sheet = sheet, limits = limits, shim = shim,  :
#              NA inserted for impossible 1900-02-29 datetime

#But this shouldn't affect the final output. 
# ----------------------------------------------


# ----------------------------------------------
###### source the functions that we need 
# ----------------------------------------------
prep_dir <- "local repo where the prep files are"
source(paste0(prep_dir, "prep_detailed_uga_budget.R"))
source(paste0(prep_dir, "prep_summary_uga_budget.R"))
source(paste0(prep_dir, "prep_pudr_uga.R"))


# ---------------------------------------------
########## Set up variables and load the prep files ########
# ---------------------------------------------
cashText <- " Cash Outflow" ##we'll need this to grab the right columns from the budgets 
loc_name <- 'uga' ##change this when we can get subnational data 
source <- "gf" ## denotes the type of data (e.g. government expenditures, Global fund, etc.)


## set up the directory & file list: 
file_dir <- 'your local drive here' ##where the files are stored locally - on the J drive, the filepath is:  "J:/Project/Evaluation/GF/resource_tracking/uga/gf/"
file_list <- read.csv(paste0(file_dir, "uga_budget_file_list.csv"), na.strings=c("","NA"),
                      stringsAsFactors = FALSE) 
file_list$start_date <- ymd(file_list$start_date)


# ---------------------------------------------
########## Create a summary file of the data ########
# ---------------------------------------------

summary_file <- setnames(data.table(matrix(nrow = length(file_list$file_name), ncol = 10)), 
                         c("data_source","year", "start_date",  "end_date", "sda_detail",
                           "geographic_detail", "period",	"grant", "disease", "loc_name"))

summary_file$loc_name <- as.character(summary_file$loc_name)
summary_file$loc_name <- loc_name

# ---------------------------------------------
########## Run the for loop that preps data ########
# ---------------------------------------------

for(i in 1:length(file_list$file_name)){ 
  ##fill in the summary tracking file with what we know already: 
  summary_file$disease[i] <- file_list$disease[i]
  summary_file$year[i] <- file_list$grant_period[i]
  summary_file$grant[i] <- file_list$grant[i]
  summary_file$period[i] <- file_list$period[i] 
  summary_file$geographic_detail[i] <- file_list$geography_detail[i]
  summary_file$data_source[i] <- file_list$data_source[i]
  
  if(file_list$type[i]=="detailed"){##most detailed level of budgets 
    tmpData <- prep_detailed_uga_budget(file_dir, file_list$file_name[i], as.character(file_list$sheet[i]), 
                                       file_list$start_date[i], file_list$qtr_number[i],
                                       cashText, file_list$grant[i], 
                                        file_list$disease[i], file_list$period[i],file_list$data_source[i])
  } else if (file_list$type[i]=="summary"){ ##not much detail, only high level SDAs: 
    tmpData <- prep_summary_uga_budget(file_dir, file_list$file_name[i], as.character(file_list$sheet[i]), 
                                       file_list$start_date[i], file_list$qtr_number[i], 
                                       cashText, file_list$grant[i], 
                                       file_list$disease[i], file_list$period[i], file_list$recipient[i], 
                                       file_list$data_source[i])
    tmpData$disbursement <- 0 
  ##LFA data cleaning: 
  } else if (file_list$type[i]=="pudr"){ ##has expenditure data 
    tmpData <- prep_pudr_uga(file_dir, file_list$file_name[i], as.character(file_list$sheet[i]), 
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
  summary_file$start_date[i] <- min(tmpData$start_date) ##since there are multiple values in this, get the earliest start date 
  
  print(i)
}

summary_file$end_date <- as.Date(summary_file$end_date)
summary_file$start_date <- as.Date(summary_file$start_date)
resource_database$start_date <- as.Date(resource_database$start_date)

##change the column names of the summary file variables so that they make sense: 
setnames(summary_file, c("Data Source",	"Year",	"Start Date", "End Date", "SDA Detail",	"Geographic Detail", "Temporal Detail",	"Grant", "Disease", "Location"))

##export the summary table
##(you might get a warning message about appending column names to the files; this should not affect the final output)
write.table(summary_file, paste0("file path where you want the summary file","resource_tracking_data_summary.csv"),
            append = TRUE, row.names=FALSE, sep=",")


# ---------------------------------------------
########## Modify the prepped data variables as necessary ########
# ---------------------------------------------

##make sure to change the financial data variables to be "numeric" 
resource_database$budget <- as.numeric(resource_database$budget)
resource_database$expenditure <- as.numeric(resource_database$expenditure)
resource_database$disbursement <- as.numeric(resource_database$disbursement)

##assign loc_name and source: 
resource_database$loc_name <- loc_name
resource_database$financing_source <- source

## optional: do a check on data to make sure values aren't dropped: 
# data_check1<- as.data.frame(resource_database[, sum(budget, na.rm = TRUE),by = c("grant_number", "data_source","disease")])

## we have some junk "modules" that should be dropped:
toMatch <- c("0", "Please sel", "6", "4")
cleaned_database <- resource_database[!grepl(paste(toMatch, collapse="|"), resource_database$module),]

dups<-cleaned_database[duplicated(cleaned_database) | duplicated(cleaned_database, fromLast=TRUE)]

##most of the time, these duplicates are either budget values with NA or 0
##UGA-T-MoFPED has duplicated rows for a treatment category, but these were present in the original budget, so we'll aggregate them together


##sum up to remove duplicates: 
byVars = names(cleaned_database)[!names(cleaned_database)%in%c('budget', 'disbursement', 'expenditure')]
cleaned_database <- cleaned_database [, list(budget=sum(na.omit(budget)), 
                                            disbursement=sum(na.omit(disbursement)),expenditure=sum(na.omit(expenditure))), by=byVars]

# ---------------------------------------------
########## We'll split the TB/HIV grants as follows: ########
########## If the module explicitly says TB, then assign as TB ########
########## Otherwise, default it to HIV ########
# ---------------------------------------------
## split hiv/tb into hiv or tb (for module/intervention mapping purposes): 
get_hivtb_split <- function(disease,module){
  x <- disease
 if(disease=="hiv/tb"){
   if(grepl(paste(c("tb", "tuber"), collapse="|"), module)){ 
    x <- "tb"
  } else { ##otherwise, map it to HIV
    x <- "hiv"
  }
 }
return(x)
}

cleaned_database$disease <- mapply(get_hivtb_split, cleaned_database$disease, cleaned_database$module)

# ---------------------------------------------
########## Strip special characters from the SDA descriptions ########
# ---------------------------------------------
##list of punctions to remove: 
sda_remove_chars <- c(" ", "[\u2018\u2019\u201A\u201B\u2032\u2035]","[\u201C\u201D\u201E\u201F\u2033\u2036]"
                  , "[[:punct:]]", "\"", ",")

##get rid of punctuation and accents in the SDA activities: 
cleaned_database$sda_activity <-gsub(paste(sda_remove_chars, collapse="|"), "",cleaned_database$sda_activity)
cleaned_database$sda_activity <-tolower(cleaned_database$sda_activity)

# ----------------------------------------------
##### Map to the GF Modules and Interventions #####
##run the map_modules_and_interventions.R script first
# ----------------------------------------------

## on the J Drive: map_dir <- J:/Project/Evaluation/GF/mapping/multi_country/intervention_categories/
map_dir <- "where the intervention_and_indicator_list.xlsx file lives"
mapping_list <- load_mapping_list(paste0(map_dir, "intervention_and_indicator_list.xlsx")
                                  , include_rssh_by_disease = FALSE) ##set the boolean to false for just mapping

## before we get it ready for mapping, copy over so we have the correct punctuation for final mapping: 
final_mapping <- copy(mapping_list)
final_mapping$disease <- NULL ## we will be joining on code 
setnames(final_mapping, c("module", "intervention"), c("gf_module", "gf_intervention"))
mapping_list$coefficient <- 1
mapping_list$abbrev_intervention <- NULL
mapping_list$abbrev_module <- NULL


##this loads the list of modules/interventions with their assigned codes
gf_mapping_list <- total_mapping_list(paste0(dir, "intervention_and_indicator_list.xlsx"),
                                      mapping_list, unwanted_array, remove_chars)

##strip all of the special characters, white space, etc. from the RT database
cleaned_database <- strip_chars(cleaned_database, unwanted_array, remove_chars)
## we have some junk "modules" that should be dropped:
ugaData  <- cleaned_database[!grepl("pleasesel", cleaned_database$module),]

#optional: check again for any dropped data: 
# data_check2<- as.data.frame(ugaData[, sum(budget, na.rm = TRUE),by = c("grant_number", "data_source","disease")])



# ----------------------------------------------
########### USE THIS TO CHECK FOR ANY UNMAPPED MODULE/INTERVENTIONS ###########
# ----------------------------------------------


# gf_concat <- paste0(gf_mapping_list$module, gf_mapping_list$intervention)
# uga_concat <- paste0(ugaData$module, ugaData$intervention)
# unmapped_mods <- uga_concat[!uga_concat%in%gf_concat]



# ----------------------------------------------
########### Map the RT dataset to the GF Modular Framework ###########
# ----------------------------------------------
##merge the RT dataset and the initial mapping list to assign codes: 
uga_init_mapping <- merge(ugaData, gf_mapping_list, by=c("module", "intervention", "disease"), 
                          all.x=TRUE,allow.cartesian = TRUE)

##use this to check if any modules/interventions were dropped:
# dropped_gf <- uga_init_mapping[is.na(uga_init_mapping$code)]

##finally, merge the RT dataset to the GF framework list by code: 
mappedUga <- merge(uga_init_mapping, final_mapping, by="code")

##if any of the modules are "split", this performs the $$ split across modules 
mappedUga$budget <- mappedUga$budget*mappedUga$coefficient
mappedUga$expenditure <- mappedUga$expenditure*mappedUga$coefficient
mappedUga$disbursement <- mappedUga$disbursement*mappedUga$coefficient

##change this when we get geo locations for UGA: 
mappedUga$adm1 <- 190
mappedUga$adm2 <- 190
mappedUga$country <- "Uganda"
mappedUga$lang <- "eng"


# ----------------------------------------------
###Check that nothing got dropped: ### 
# ----------------------------------------------
# data_check1 <- ugaData[, sum(budget, na.rm = TRUE),by = c( "module","intervention","disease")]
# data_check2 <-mappedUga[, sum(budget, na.rm = TRUE),by = c("module", "intervention","disease")]
# data_check1[!module%in%data_check2$module]
# data_check1$ind <- "pre"
# data_check2$ind <- "post"
# data_check <- rbind(data_check1, data_check2)
# write.csv(data_check, "data_check.csv", row.names = FALSE)

# ----------------------------------------------
#####write csv to correct folder ############
# ----------------------------------------------
write.csv(mappedUga, "prepped_uga_data.csv", row.names = FALSE,
          fileEncoding = "latin1")

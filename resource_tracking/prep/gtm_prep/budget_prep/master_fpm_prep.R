# ----------------------------------------------
# Irena Chen
# May 16th, 2018
# Master code file for GTM FPM/PUDR data cleaning 
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

##STEP 1: Download the "gf" folder from Basecamp (in the Resource Tracking Data folder) 
##and save it on your local drive 


# ---------------------------------------------
### assign some variables
# ---------------------------------------------
loc_name <- "gtm"

# ----------------------------------------------
###### source the functions that we need 
## set "prep_dir" to 
# ----------------------------------------------
prep_dir <- "your local repo folder + gf/resource_tracking/prep/"
prep_dir <- "H:/gf/resource_tracking/prep/"

source(paste0(prep_dir, "gtm_prep/budget_prep/prep_fpm_detailed_budget.R"))
source(paste0(prep_dir, "gtm_prep/budget_prep/prep_fpm_summary_budget.R"))
source(paste0(prep_dir, "gtm_prep/budget_prep/prep_fpm_other_budget.R"))
source(paste0(prep_dir, "gtm_prep/budget_prep/prep_fpm_other_detailed_budget.R"))
source(paste0(prep_dir, "gtm_prep/budget_prep/prep_gtm_pudr.R"))
source(paste0(prep_dir,"map_modules_and_interventions.R"))
# ----------------------------------------------
###### Load the list of RT files we want to process
# ----------------------------------------------
file_dir <- 'J:/Project/Evaluation/GF/resource_tracking/gtm/gf/'
file_list <- read.csv(paste0(file_dir, "official_budgets/gtm_budget_filelist.csv"))


# ##create a summary file to track the data that we have (and that we still need)
# summary_file <- setnames(data.table(matrix(nrow = length(file_list$file_name), ncol = 10)), 
#                          c("data_source","year", "start_date",  "end_date", "sda_detail",
#                            "geographic_detail", "period",	"grant", "disease", "loc_name"))
# 
# summary_file$loc_name <- as.character(summary_file$loc_name)
# summary_file$loc_name <- loc_name

# ----------------------------------------------
###### For loop that preps data and aggregates it
# ----------------------------------------------

for(i in 1:length(file_list$file_name)){
  # ##fill in the summary tracking file with what we know already: 
  # summary_file$disease[i] <- as.character(file_list$disease[i])
  # summary_file$grant[i] <- as.character(file_list$grant_number[i])
  # summary_file$period[i] <- file_list$period[i] 
  # summary_file$geographic_detail[i] <- as.character(file_list$geography_detail[i])
  # summary_file$data_source[i] <- as.character(file_list$data_source[i])
  # summary_file$year[i] <- as.character(file_list$grant_period[i])
  # 
  if(file_list$format[i]=="detailed"){ ## fpm detailed budgets 
    tmpData <- prep_fpm_detailed_budget(file_dir, file_list$file_name[i], as.character(file_list$sheet[i]),
                                        ymd(file_list$start_date[i]), file_list$qtr_number[i], file_list$disease[i], file_list$period[i], 
                                        file_list$lang[i], file_list$grant_number[i], file_list$recipient[i])
    tmpData$disbursement<- 0 
  } else if (file_list$format[i]=="summary"){ ## only summary level data - no municipalities 
    tmpData <- prep_fpm_summary_budget(file_dir, file_list$file_name[i], as.character(file_list$sheet[i]),
                                       ymd(file_list$start_date[i]), file_list$qtr_number[i], file_list$disease[i], file_list$period[i], 
                                       file_list$grant_number[i], file_list$recipient[i], file_list$lang[i])
    tmpData$loc_name <- "gtm"
    tmpData$disbursement<- 0 
    
  } else if (file_list$format[i]=="detailed_other"){ ## there's an older version of detailed fpm budgets
    tmpData <- prep_other_detailed_budget(file_dir, file_list$file_name[i], as.character(file_list$sheet[i]),
                                        ymd(file_list$start_date[i]), file_list$qtr_number[i], file_list$disease[i], file_list$period[i], 
                                        file_list$lang[i], file_list$grant_number[i])
    tmpData$disbursement<- 0 

  } else if (file_list$format[i]=="pudr"){ 
    tmpData <- prep_gtm_pudr(file_dir, file_list$file_name[i], as.character(file_list$sheet[i]),
                                          ymd(file_list$start_date[i]), file_list$qtr_number[i], file_list$disease[i], file_list$period[i], 
                                          file_list$grant_number[i], file_list$data_source[i], loc_name, file_list$lang[i])

  } else if (file_list$format[i]=="other"){
    tmpData <- prep_other_budget(file_dir, file_list$file_name[i], as.character(file_list$sheet[i]),
                                          ymd(file_list$start_date[i]), file_list$qtr_number[i], file_list$disease[i], file_list$period[i], 
                                          file_list$lang[i], file_list$grant_number[i])
    tmpData$disbursement<- 0 
  }
  tmpData$loc_name <- loc_name
  tmpData$data_source <- file_list$data_source[i]
  tmpData$fileName <- file_list$file_name[i]
  tmpData$grant_period <- file_list$grant_period[i]
  
  message(paste(i, "colNames: ", length(colnames(tmpData))))
  
  if(i==1){
    resource_database = tmpData
  }
  if(i>1){
    resource_database = rbind(resource_database, tmpData, use.names=TRUE)
  }
  
  # if(file_list$format[i]=="detailed"){
  #   summary_file$sda_detail[i] <- "Detailed"
  # } else if (file_list$format[i]=="summary"){
  #   summary_file$sda_detail[i] <- "Summary"
  # } else if(!(tmpData$sda_activity[1]=="All")){
  #   summary_file$sda_detail[i] <- "Detailed"
  # } else {
  #   summary_file$sda_detail[i] <- "None"
  # }
  # summary_file$end_date[i] <- ((max(tmpData$start_date))+file_list$period[i]-1)
  # summary_file$start_date[i] <- min(tmpData$start_date)

  
  print(i)
}


# summary_file$end_date <- as.Date(summary_file$end_date,"%Y%m%d")
# summary_file$start_date <- as.Date(summary_file$start_date,"%Y%m%d")
# setnames(summary_file, c("Data Source",	"Grant Time Frame",	"Start Date",
#                          "End Date", "SDA Detail",	"Geographic Detail", "Temporal Detail",	"Grant", "Disease", "Location"))

# ----------------------------------------------
##export the summary table to J Drive
# ----------------------------------------------
##(you might get a warning message about appending column names to the files; this should not affect the final output)
# write.table(summary_file, paste0(dir, "resource_tracking_data_summary.csv"),
#             append = TRUE, row.names=FALSE, sep=",")

# ----------------------------------------------
##Add more RT variables and clean up any rows with "junk" data
# ----------------------------------------------

resource_database$adm1 <- 128 ## change if we get department data in the budgets 
resource_database$adm2 <- resource_database$adm1 ## change if we get municipality data in the budgets 
resource_database$start_date <- as.Date(resource_database$start_date)
resource_database$budget <- as.numeric(resource_database$budget)
resource_database$expenditure<- as.numeric(resource_database$expenditure)
resource_database$disbursement<- as.numeric(resource_database$disbursement)
## since we only have budget data, include exp and disbursed as 0:  
resource_database$financing_source <- "gf"

resource_database <- resource_database[!grepl("Fondos pendientes de asignar a SR",resource_database$recipient)]

##most of the time, these duplicates are either budget values with NA or 0
##GTM-T-MSPAS and GUA-M-MSPAS has duplicated rows, but these were present in the original budget, so we'll aggregate them together
dups<-resource_database[duplicated(resource_database) | duplicated(resource_database, fromLast=TRUE)]

##sum up to remove duplicates: 
byVars = names(resource_database)[!names(resource_database)%in%c('budget', 'disbursement', 'expenditure')]
cleaned_database= resource_database[, list(budget=sum(na.omit(budget)), 
                                            disbursement=sum(na.omit(disbursement)),expenditure=sum(na.omit(expenditure))), by=byVars]
# ----------------------------------------------
##optional: check for any dropped data 
# ----------------------------------------------
data_check1<- as.data.frame(resource_database[, sum(budget, na.rm = TRUE),by = c("grant_number","data_source", "disease")])
data_check2<- as.data.frame(cleaned_database[, sum(budget, na.rm = TRUE),by = c("grant_number","data_source", "disease")])
# ----------------------------------------------
##### Map to the GF Modules and Interventions #####
##run the map_modules_and_interventions.R script first
# ----------------------------------------------

gtmData <- strip_chars(cleaned_database, unwanted_array, remove_chars)
gtmData[is.na(module), module:=intervention]

## the directory on the J Drive for the intervention list is:
map_dir <- "J:/Project/Evaluation/GF/mapping/multi_country/intervention_categories/"
mapping_list <- load_mapping_list(paste0(map_dir, "intervention_and_indicator_list.xlsx"),
                                  include_rssh_by_disease = FALSE)

## before we get it ready for mapping, copy over so we have the correct punctuation for final mapping: 
final_mapping <- copy(mapping_list)
final_mapping$disease <- NULL
setnames(final_mapping, c("module", "intervention"), c("gf_module", "gf_intervention"))
mapping_list$coefficient <- 1
mapping_list$abbrev_intervention <- NULL
mapping_list$abbrev_module<- NULL
gf_mapping_list <- total_mapping_list(paste0(map_dir,"intervention_and_indicator_list.xlsx"),
                                      mapping_list, unwanted_array, remove_chars)

# ---------------------------------------------------------------------------------------
# Correct any unmapped modules, leaving initials, date, and applicable budget filepath. 
# These should all be written as if they could modify any other file to catch general errors. 
# ---------------------------------------------------------------------------------------

#EMILY PULL THESE CHANGES OUT INTO DIFFERENT FILES. 

#EKL 10/25/18, official_budgets/03. Presupuesto detallado.xlsx 
gtmData$intervention = ifelse(gtmData$intervention == "detecciondecasosydiagnosticotbmdr", "detecciondecasosydiagnosticotbmr", 
                              gtmData$intervention)
gtmData$intervention = ifelse(gtmData$intervention == "detecciondecasosydiagnostico" & gtmData$module == "paqueteparatbmr", "detecciondecasosydiagnosticotbmr", 
                              gtmData$intervention)
gtmData$module = ifelse(gtmData$intervention == "detecciondecasosydiagnosticotbmr", "paqueteparatbmr", gtmData$module)
gtmData$intervention = ifelse(gtmData$module == "paqueteparatbmr" & gtmData$intervention == "tratamiento", "tratamientotbmr", gtmData$intervention)        
gtmData$intervention = ifelse(gtmData$module == "paqueteparatbmr" & gtmData$intervention == "prestaciondeatencioncomunitariaparatbmdr", 
                              "prestaciondeserviciosdeatenciondelatuberculosisenlacomunidad", gtmData$intervention)

#EKL 10/30/18, official_budgets/03. Presupuesto detallado.xlsx
gtmData$module = ifelse(gtmData$module == "atencionyprevenciondetuberculosis" & gtmData$intervention == "srssrespuestaysistemacomunitrio", 
                        "ssrsrespuestasysistemascomunitarios", 
                        gtmData$module)
gtmData$intervention = ifelse(gtmData$module == "ssrsrespuestasysistemascomunitarios" & gtmData$intervention == "srssrespuestaysistemacomunitrio", 
                              "otrasintervencionespararespuestasysistemascomunitarios", gtmData$intervention)
gtmData$intervention = ifelse(gtmData$module =="atencionyprevenciondetuberculosis" & gtmData$intervention == "implicaratodoslosproveedoresdeatencionatencionyprevenciondetb", 
                              "implicaratodoslosproveedoresdeasistencia", gtmData$intervention)

# ----------------------------------------------
# Use this to check for any unmapped modules/interventions
# ----------------------------------------------
gf_concat <- paste0(gf_mapping_list$module, gf_mapping_list$intervention)
gtm_concat <- paste0(gtmData$module, gtmData$intervention)
unmapped_mods <- gtmData[!gtm_concat%in%gf_concat]

if(nrow(unmapped_mods)>0){
  print(unique(unmapped_mods[, c("module", "intervention", "fileName"), with= FALSE]))
  stop("You have unmapped original modules/interventions!")
}

# --------------------------------------------------------------------------
# Correct any modules that are dropping here. Comment initials, date, and filename. 
# --------------------------------------------------------------------------

# test_merge1<-data.frame(a = seq(1,16,by=2), b = LETTERS[1:8], x= month.abb[1:8], y = sample(10:20,8, replace = TRUE), z=letters[1:8])
# test_merge2<-data.frame(a = rep.int(1, 8), b = rep('A', 8), c = month.abb[1:8])
# 
# test_merge3 <- merge(test_merge1, test_merge2, by=c("a", "b"), all.x=TRUE)
# test_merge4 <- merge(test_merge1, test_merge2, by=c("a", "b"), all.x=TRUE, allow.cartesian = TRUE) #Now I'm really not sure what this is doing. Because this is just appending duplicates into file. 


# ----------------------------------------------
# Merge the datasets on the GF codes to map to framework 
# ----------------------------------------------
gtm_init_mapping <- merge(gtmData, gf_mapping_list, by=c("module", "intervention", "disease"), all.x=TRUE, allow.cartesian = TRUE)

##use this to check if any modules/interventions were dropped:
dropped_gf <- gtm_init_mapping[is.na(gtm_init_mapping$code)]

mappedGtm <- merge(gtm_init_mapping, final_mapping, by="code", all.x=TRUE) 

if(sum(is.na(mappedGtm$gf_module)) > 0){
  # Check if anything is dropped in the merge -> if you get an error. Check the mapping spreadsheet
  print(unique(dropped_gf[, c("module", "intervention", "disease"), with= FALSE]))
  stop("Modules/interventions were dropped! - Check Mapping Spreadsheet codes vs intervention tabs")
}

mappedGtm$budget <- mappedGtm$budget*mappedGtm$coefficient
mappedGtm$expenditure <- mappedGtm$expenditure*mappedGtm$coefficient
mappedGtm$disbursement <- mappedGtm$disbursement*mappedGtm$coefficient

mappedGtm$sda_activity <- ifelse(tolower(mappedGtm$sda_activity) == "all" | mappedGtm$sda_activity == "0", "Unspecified (Summary budget)", mappedGtm$sda_activity)

# ----------------------------------------------
##Optional: sum to make sure that budget numbers aren't dropped:
# ----------------------------------------------
# data_check1 <- gtmData[, sum(budget, na.rm = TRUE),by = c( "module","intervention","disease")]
# data_check2 <-mappedGtm[, sum(budget, na.rm = TRUE),by = c("module", "intervention","disease")]

mappedGtm$year <- year(mappedGtm$start_date)

# ----------------------------------------------
##output dataset to the correct folder as a csv: 
# ----------------------------------------------

write.csv(mappedGtm, "J:/Project/Evaluation/GF/resource_tracking/gtm/prepped/prepped_budget_data.csv", row.names = FALSE,
          fileEncoding = "latin1")








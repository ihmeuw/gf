

# ----------------------------------------------
# Irena Chen
#
# 11/27/2017
# ### GOS DATA graphs 

# TO DO
# fix time series graph so that there are gaps where appropriate (use `group` aesthetic)
# ----------------------------------------------
###### Set up R / install packages  ###### 
# ----------------------------------------------
rm(list=ls())
library(ggplot2)
library(dplyr)
library(tools)
library(data.table)
library(lubridate)
library(grDevices)
library(RColorBrewer)
library(readxl)

prep_dir <- " your local repo + gf/resource_tracking/prep/"
prep_dir <- "H:/gf/resource_tracking/prep/"
source(paste0(prep_dir, "map_modules_and_interventions.R"))

# ----------------------------------------------
###### Load the GOS tab from the Excel book  ###### 
# ----------------------------------------------

gos_data  <- data.table(read_excel('J:/Project/Evaluation/GF/resource_tracking/multi_country/gf/Expenditures from GMS and GOS for PCE IHME countries.xlsx',
                                   sheet=as.character('GOS Mod-Interv - Extract')))

## reset column names
oldNames <-  c("Country","Grant Number", "Year", "Financial Reporting Period Start Date",
               "Financial Reporting Period End Date","Module", "Intervention", "Total Budget Amount (in budget currency)", 
               "Total Expenditure Amount (in Budget currency)", "Component", "Current IP  Start Date", "Current IP  End Date")
newNames <-  c("country","grant_number", "year","start_date","end_date","module","intervention", 
               "budget", "expenditure", "disease", "grant_period_start", "grant_period_end")

setnames(gos_data,oldNames, newNames)

##subset the columns that we want 
gos_clean <- gos_data[, newNames, with=FALSE]

gos_clean$grant_period = paste0(year(as.Date(gos_clean$grant_period_start)), "-",year(as.Date(gos_clean$grant_period_end)))
gos_clean$grant_period_start = NULL
gos_clean$grant_period_end = NULL

# ----------------------------------------------
###### Load the GMS tab from the Excel book  ###### 
# ----------------------------------------------
gms_data  <- data.table(read_excel('J:/Project/Evaluation/GF/resource_tracking/multi_country/gf/Expenditures from GMS and GOS for PCE IHME countries.xlsx',
                                   sheet=as.character('GMS SDAs - extract')))

##repeat the subsetting that we did above (grabbing only the columns we want)
gmsOld <- c(oldNames[1:5], "Service Delivery Area", "Total Budget Amount (USD equ)", "Total Expenditure Amount (USD equ)", "Component", "Program Start Date", "Program End Date")
gmsNew <- c(newNames[1:6], newNames[8:10], "grant_period_start", "grant_period_end")
setnames(gms_data, gmsOld, gmsNew)
gms_clean <- gms_data[, gmsNew, with=FALSE]
gms_clean$intervention <- "All"


gms_clean$grant_period = paste0(year(as.Date(gms_clean$grant_period_start)), "-",year(as.Date(gms_clean$grant_period_end)))
gms_clean$grant_period_start = NULL
gms_clean$grant_period_end = NULL

##combine both GOS and GMS datasets into one dataset
totalGos <- rbind(gms_clean, gos_clean)

# ----------------------------------------------
###### Load the GMS tab from the Excel book  ###### 
# ----------------------------------------------
map_disease <- unique(totalGos$disease)
names(map_disease) <- c("tb", "malaria", "hiv", "hss", "hiv/tb")
  
kDT = data.table(map_disease = names(map_disease), value = TRUE, disease = unname(map_disease))
totalGos[kDT, on=.(disease), disease := i.map_disease]
totalGos[disease=='hiv/tb', disease:='hiv']

totalGos$sda_activity <- "All"
totalGos$recipient <- totalGos$grant_number
totalGos$data_source <- "gos"
totalGos$financing_source <- "gf"
totalGos$loc_name <- totalGos$country
totalGos$disbursement <- 0

# ----------------------------------------------
###### Map the GOS/GMS modules to the current GF Framework ###### 
# Run the map_modules_and_interventions.R script first
# ----------------------------------------------
totalGos <- strip_chars(totalGos, unwanted_array, remove_chars)

mapping_list <- load_mapping_list(paste0("J:/Project/Evaluation/GF/mapping/multi_country/intervention_categories/intervention_and_indicator_list.xlsx")
                                  , include_rssh_by_disease = FALSE) ##set the boolean to false for just mapping

## before we get it ready for mapping, copy over so we have the correct punctuation for final mapping: 
final_mapping <- copy(mapping_list)
final_mapping$disease <- NULL ## we will be joining on code 
setnames(final_mapping, c("module", "intervention"), c("gf_module", "gf_intervention"))
mapping_list$coefficient <- 1
mapping_list$abbrev_intervention <- NULL
mapping_list$abbrev_module <- NULL


##this loads the list of modules/interventions with their assigned codes
gf_mapping_list <- total_mapping_list(paste0("J:/Project/Evaluation/GF/mapping/multi_country/intervention_categories/intervention_and_indicator_list.xlsx"),
                                      mapping_list, unwanted_array, remove_chars)

gos_init_mapping <- merge(totalGos, gf_mapping_list, by=c("module", "intervention", "disease"), all.x=TRUE,allow.cartesian = TRUE)

##use this to check if any modules/interventions were dropped:
# dropped_gf <- gos_init_mapping[is.na(gos_init_mapping$code)]

mappedGos <- merge(gos_init_mapping, final_mapping, by="code")
mappedGos$budget <-mappedGos$budget*mappedGos$coefficient
mappedGos$expenditure <-mappedGos$expenditure*mappedGos$coefficient
mappedGos$disbursement <-mappedGos$disbursement*mappedGos$coefficient

# optional: check for any dropped data that might have happened during the mapping: 

# data_check1 <- totalGos[, sum(budget, na.rm = TRUE),by = c( "module","intervention","disease")]
# data_check2 <-mappedGos[, sum(budget, na.rm = TRUE),by = c("module", "intervention","disease")]
mappedGos$fileName = "Expenditures from GMS and GOS for PCE IHME countries.xlsx"
# ----------------------------------------------
###### export the mapped dataset ###### 
# ----------------------------------------------

write.csv(mappedGos, "J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/prepped_gos_data.csv",
          row.names = FALSE, fileEncoding = "latin1")




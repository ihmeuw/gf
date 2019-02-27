# ----------------------------------------------
# Naomi Provost
# September 6, 2018
# Master code file for GTM HIV data cleaning 
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
library(raster)
library(rgeos)
library(ggplot2)
library(maptools)
library(RColorBrewer)

# ----------------------------------------------
#----------------------------------
# Set the directory to download the data
# detect if operating on windows or on the cluster 
root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

#------------ Prep Data------------------------
#### Prep Data
# define main directory
dir <- paste0(root, '/Project/Evaluation/GF/outcome_measurement/gtm/HIV/SIGSA/')
prep_dir <- paste0(root, '/Project/Evaluation/GF/outcome_measurement/gtm/prepped_data/')
outFile = paste0(root, '/Project/Evaluation/GF/outcome_measurement/gtm/visualizations/SIGSA_hiv_test.pdf')
mapping_dir = paste0(root, '/Project/Evaluation/GF/mapping/gtm/')

# translation file
translate_data = fread(paste0(dir, "translation_of_HIV_variables.csv"), encoding = 'Latin-1')

#read in file
dt <-data.table(read_excel(paste0(dir,"Solicitud 0593-2018/Solicitud 0593-2018 SIGSA SIDA 1.2 años 2014 al 2017.xlsx")))

total_data = dt

# rename columns
new_names = c("year", "month", "hospital_DAS", "hospital_health_district", "health_service", "service_type", "municipality", "date", "identifier","birth_department",
              "birth_municipality", "birth_day", "birth_month", "birth_year", "nationality", "sex", "residence_department", "residence_municipality",
              "sexual_orientation", "town", "community_linguistics", "risk_condition", "reason_for_visit", "pregnancy_stage", "pre_orientaiton_test", "did_test",
              "completed_hiv_screening_test", "hiv_screening_result", "hiv_confirm_test", "hiv_confirm_test_result",
              "sifilis_treponemal_test", "sifilis_treponemal_test_result", "sifilis_non_treponemal_test", "sifilis_dilution_test",
              "hepB_screening_test", "hepB_screening_result", "recieved_post_rest_results", "reference")

names(total_data) = new_names

# Remove first and last 3 rows 
total_data = total_data[-(1:3), , drop = FALSE] 
total_data = total_data[1:(nrow(total_data) - 3),, drop = FALSE]

#create a unique anonymus identifier
dt_unique = unique(total_data[,"identifier"])
dt_unique$id = row(dt_unique)
total_data = merge(total_data, dt_unique, by = "identifier")

# Drop columns of personal identifying information
total_data$birth_municipality = NULL
total_data$birth_department = NULL
total_data$identifier = NULL

# Translate data that we wil be using to English
reason_eng = translate_data[1:14]
risk_eng = translate_data[16:24]
setnames(risk_eng, old = c("reason_for_visit", "reason_for_visit_eng"), new = c("risk_condition", "risk_condition_eng"))
preg_eng = translate_data[26:31]
setnames(preg_eng, old = c("reason_for_visit", "reason_for_visit_eng"), new = c("pregnancy_stage", "pregnancy_stage_eng"))

total_data = merge(total_data, reason_eng, by = "reason_for_visit")
total_data = merge(total_data, risk_eng, by = "risk_condition")
total_data = merge(total_data, preg_eng, by = "pregnancy_stage")

#Replace "-" with NAs
total_data = total_data[, lapply(.SD, function(x) replace(x, which(x=='-'), NA))]

# Create date variable and calculate age
total_data$date <- as.Date(with(total_data, paste(year, month, "1",sep="-")), "%Y-%m-%d")
total_data$birth_date <- as.Date(paste0(total_data$birth_year, "-", total_data$birth_month, "-", total_data$birth_day))
total_data$age <-  as.integer((total_data$date - total_data$birth_date) / 365)

# Drop age identifying columns
total_data$birth_day = NULL
total_data$birth_month = NULL
total_data$birth_year = NULL
total_data$birth_date = NULL

# Prep for mapping
total_data$completed_hiv_screening_test = gsub("Si", "Yes", total_data$completed_hiv_screening_test)
total_data$completed_hiv_screening_test = gsub("Reactivo", "Yes", total_data$completed_hiv_screening_test)

total_data$reason_for_visit = NULL
total_data$pregnancy_stage = NULL
total_data$risk_condition = NULL

# Find facility level for each facility
total_data$level = ifelse(grepl("Hospital", total_data$service_type), 3,
                          ifelse(grepl("Centro", total_data$service_type), 2, 
                                 ifelse(grepl("Maternidad", total_data$service_type), 2, 1)))

# Count which visit number this is (to check how many poeple are attending the clinic)
total_data = total_data[order(date)]
total_data[, visit_num := seq_len(.N), by = id]

# Write csv & RDS to folderpath
write.csv(total_data, paste0(prep_dir, "hiv_sigsa_data_prepped_patientlvl_testing.csv"), row.names = FALSE)
saveRDS(total_data, paste0(prep_dir, "hiv_sigsa_data_prepped_patientlvl_testing.rds"))

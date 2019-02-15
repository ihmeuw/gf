# ----------------------------------------------
# Naomi Provost, updated by Emily Linebarger 
# Last updated: February 2019
# Combine patient level SIGSA and SIGPRO data 
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
fix_diacritics <- function(x) {
  replacement_chars = list('S'='S', 's'='s', 'Z'='Z', 'z'='z', '?'='A', '?'='A', '?'='A', '?'='A', '?'='A', '?'='A', '?'='A', '?'='C', '?'='E', '?'='E',
                           '?'='E', '?'='E', '?'='I', '?'='I', '?'='I', '?'='I', '?'='N', '?'='O', '?'='O', '?'='O', '?'='O', '?'='O', '?'='O', '?'='U',
                           '?'='U', '?'='U', '?'='U', '?'='Y', '?'='B', '?'='Ss', '?'='a', '?'='a', '?'='a', '?'='a', '?'='a', '?'='a', '?'='a', '?'='c',
                           '?'='e', '?'='e', '?'='e', '?'='e', '?'='i', '?'='i', '?'='i', '?'='i', '?'='o', '?'='n', '?'='o', '?'='o', '?'='o', '?'='o',
                           '?'='o', '?'='o', '?'='u', '?'='u', '?'='u', '?'='y', '?'='y', '?'='b', '?'='y')
  
  replace_me <- paste(names(replacement_chars), collapse='')
  replace_with <- paste(replacement_chars, collapse = '')
  return(chartr(replace_me, replace_with, x))
  
}


#----------------------------------
# Set the directory to download the data
# detect if operating on windows or on the cluster 
root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
dir <- paste0(root, '/Project/Evaluation/GF/outcome_measurement/gtm/HIV/sigsa_sigpro/')
sigsa_raw_dir = paste0(dir, "raw_sigsa/")
sigpro_raw_dir = paste0(dir, "raw_sigpro/")
output_dir <- paste0(dir, 'prepped_data/')

#------------ Prep Data------------------------
#### Prep Data
# define main directory
dir <- paste0(root, '/Project/Evaluation/GF/outcome_measurement/gtm/HIV/sigsa_sigpro/')
prep_dir <- paste0(dir, 'prepped_data/')

# translation file
translate_data = fread(paste0(dir, "treatment_translation_variables.csv"), encoding = 'Latin-1')

#read in file
dt <-data.table(read_excel(paste0(sigsa_raw_dir,"Patient Level Data/Solicitud 0593-2018 SIGSA SIDA 1.2 años 2014 al 2017.xlsx")))

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


# Create Department variable
total_data$hospital_department = total_data$hospital_DAS
total_data$hospital_department = sub("GUATEMALA", "GUATEMALA", total_data$hospital_department)
total_data[grepl("GUATEMALA", hospital_department), hospital_department := "GUATEMALA"]
total_data[grepl("PET?N", hospital_department), hospital_department := "PET?N"]
total_data[grepl("IXC?N", hospital_department), hospital_department := "QUICH?"]
total_data[grepl("QUETZALTENANGO", hospital_department), hospital_department := "QUEZALTENANGO"]


# SIGSA COLUMNS THAT MERGE WITH SIGPRO
sigsa_data = total_data[,c("date", "identifier", "pre_orientaiton_test", "completed_hiv_screening_test", "hiv_screening_result", "risk_condition_eng", "sexual_orientation", "age", "sex", "municipality", "hospital_department", "health_service")]
sigsa_data$data_source = "SIGSA"

#SIGPRO
sigpro_dir <- paste0(root, '/Project/Evaluation/GF/outcome_measurement/gtm/HIV/sigsa_sigpro/raw_sigpro/')
sigpro_file = 'SIGRPRO_masterlist.csv'
file_list = fread(paste0(sigpro_dir, sigpro_file))
#test = file_list[type == "HIVTEST"]
test = file_list[sheet_name == "sigpro_f4_JanNov2018 - PB_TVC"] #EKL We're only reading in one file here - how are the others getting brought in? 
testing_person_sigpro = fread(paste0(sigpro_dir, test$file_name[1])) #EKL We're only reading in one file here - how are the others getting brought in? 

# Fixing input errors in the input files
testing_person_sigpro$fechareal = ifelse(testing_person_sigpro$tema == "San Benito", as.Date(as.numeric(testing_person_sigpro$departamento),  origin = "1899-12-30"), as.Date(testing_person_sigpro$fechareal))
testing_person_sigpro$fechareal = as.Date(as.numeric(testing_person_sigpro$fechareal),  origin = "1969-12-30")
testing_person_sigpro$departamento = ifelse(testing_person_sigpro$tema == "San Benito", "Peten", testing_person_sigpro$departamento)
testing_person_sigpro$municipio = ifelse(testing_person_sigpro$tema == "San Benito", "San Benito", testing_person_sigpro$municipio)


# calculate age based on birthday
testing_person_sigpro$DOB = substr(testing_person_sigpro$codigounico, 2, 7)
testing_person_sigpro$DOB = as.Date(testing_person_sigpro$DOB, "%d%m%y")
year(testing_person_sigpro$DOB) = ifelse(year(testing_person_sigpro$DOB) > 2018, year(testing_person_sigpro$DOB) - 100, year(testing_person_sigpro$DOB))
testing_person_sigpro$age <-  as.integer((as.Date(testing_person_sigpro$fechareal) - testing_person_sigpro$DOB) / 365)

testing_person_sigpro$sex = substr(testing_person_sigpro$codigounico, 1, 1)
testing_person_sigpro$sexual_orientation = "undetermined"

sigpro_data = testing_person_sigpro[,c("fechareal", "codigounico", "prePruebaVIH", "pruebaVIH", "resultadoVIH", "subgrupo", "sexual_orientation", "age", "sex", "municipio", "departamento", 'lugar')]
names(sigpro_data) = c("date", "identifier", "pre_orientaiton_test", "completed_hiv_screening_test", "hiv_screening_result", "risk_condition_eng", "sexual_orientation", "age", "sex", "municipality", "hospital_department", "health_service")
sigpro_data$data_source = "SIGPRO"
sigpro_data$date = as.Date(sigpro_data$date)

testing_data = rbind(sigpro_data, sigsa_data)

#create a unique anonymus identifier
dt_unique = unique(testing_data[,"identifier"])
dt_unique$id = row(dt_unique)
testing_data = merge(testing_data, dt_unique, by = "identifier")
testing_data$identifier = NULL

# Count which visit number this is (to check how many poeple are attending the clinic)
testing_data = testing_data[order(date)]
testing_data[, visit_num := seq_len(.N), by = id]

# find sex variable and make all the same
testing_data$sex = substring(testing_data$sex, 1, 1)
testing_data$hiv_screening_result = toupper(testing_data$hiv_screening_result)
testing_data$completed_hiv_screening_test = toupper(testing_data$completed_hiv_screening_test)
testing_data$pre_orientaiton_test = toupper(testing_data$pre_orientaiton_test)

testing_data$completed_hiv_screening_test = ifelse(toupper(testing_data$completed_hiv_screening_test) == "S" | toupper(testing_data$completed_hiv_screening_test) == "REACTIVO", "SI", ifelse(
  toupper(testing_data$completed_hiv_screening_test) == "N", "NO", testing_data$completed_hiv_screening_test))

testing_data$pre_orientaiton_test = ifelse(toupper(testing_data$pre_orientaiton_test) == "S", "SI", ifelse(
  toupper(testing_data$pre_orientaiton_test) == "N", "NO", testing_data$pre_orientaiton_test))

# make departments the same
testing_data$hospital_department = toupper(fix_diacritics(testing_data$hospital_department))
testing_data$municipality = toupper(fix_diacritics(testing_data$municipality))
testing_data[grepl("QUETZALTENANGO", hospital_department), hospital_department := "QUEZALTENANGO"]


# create binary
testing_data$isTrans = ifelse(grepl("TRANS", testing_data$risk_condition_eng) | grepl("Trans", testing_data$sexual_orientation) | grepl("T", testing_data$sex), 1, 0)
testing_data$isMSM = ifelse(grepl("HSH", testing_data$risk_condition_eng) | testing_data$sex == "M" & (testing_data$sexual_orientation == 'Homosexual' | testing_data$sexual_orientation == 'Bisexual'), 1, 0)

testing_data = testing_data[date != "2001-01-24"]


# remove repeat positive tests for patients, only keep the first postivie test and all negative tests before it
dt_graphing = testing_data
dt_graphing_pos = testing_data[hiv_screening_result == "REACTIVO"]
dt_graphing_pos[,keep := min(visit_num), by = id]
keep_list = unique(dt_graphing_pos[,.(id, keep)])

hiv_pos_patients = dt_graphing_pos$id
# if you want to calculate by patients instead of tests done, make visit_num == 1 for neg
dt_neg = testing_data[!id %in% hiv_pos_patients]

dt_pos = testing_data[id %in% hiv_pos_patients]
dt_pos = merge(dt_pos, keep_list)
# if you want to calculate by patients instead of tests done, make visit_num == keep for pos
dt_pos = dt_pos[visit_num <= keep]
dt_pos$keep =NULL

# bind both positive and negative testing together
total_dt = rbind(dt_neg, dt_pos)


# Write csv & RDS to folderpath
write.csv(total_dt, paste0(prep_dir, "hiv_patientlvl_combined.csv"), row.names = FALSE)
saveRDS(total_dt, paste0(prep_dir, "hiv_patientlvl_combined.rds"))

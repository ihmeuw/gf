# ----------------------------------------------
# Emily Linebarger, based on code by Naomi Provost
# February 2019
# Master code file for GTM HIV data cleaning 
# ----------------------------------------------
###### Set up R / install packages  ###### 
# ----------------------------------------------
rm(list=ls())
library(data.table)
library(readxl)

#----------------------------------
# Set the directory to download the data
# detect if operating on windows or on the cluster 
root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

#--------------------------------------------
# Set directories 
#--------------------------------------------
dir <- paste0(root, '/Project/Evaluation/GF/outcome_measurement/gtm/HIV/sigsa_sigpro')
prep_dir <- paste0(dir, '/raw_sigpro')


# translation file
translate_data = fread(paste0(dir, "/translation_of_HIV_variables.csv"), encoding = 'Latin-1')

#---------------------------------------------
# Read in files
#---------------------------------------------
sigpro_complete <- read_xlsx(paste0(prep_dir, "/sigpro_f4_completo 2018.xlsx"))
setDT(sigpro_complete)
sigpro_aa <- fread(paste0(prep_dir, "/sigpro_f4_JanNov2018 - AA.csv"), encoding = "UTF-8")
sigpro_its <- fread(paste0(prep_dir, "/sigpro_f4_JanNov2018 - ITS.csv"), encoding = "UTF-8")
sigpro_pb_tvc <- fread(paste0(prep_dir, "/sigpro_f4_JanNov2018 - PB_TVC.csv"), encoding = "UTF-8")
sigpro_psico <- fread(paste0(prep_dir, "/sigpro_f4_JanNov2018 - Psico.csv"), encoding = "UTF-8")
sigpro_vin <- fread(paste0(prep_dir, "/sigpro_f4_JanNov2018 - Vinculacion.csv"), encoding = "UTF-8")

#---------------------------------------------
#Review the data. 
#---------------------------------------------
print_descriptives <- function(dt){
  print("Dates covered by this file")
  print(unique(dt[, .(Date)]))
  print("Unique activities")
  print(unique(dt[, .(tipoActividad)]))
}

print_descriptives(sigpro_aa)
print_descriptives(sigpro_its)
print_descriptives(sigpro_pb_tvc)
print_descriptives(sigpro_psico)
print_descriptives(sigpro_vin)

#--------------------------------------------
# Data quality issues 
#--------------------------------------------
#Some examples of issues found - 
print(unique(sigpro_aa$tratado)) #These should all be N, S, or NULL. 
print(unique(sigpro_aa$Date)) #We have some empty dates here. 
print(unique(sigpro_aa$codgrupo)) #A mix of characters and numbers 
print(unique(sigpro_aa$lubriSachet)) #It looks like there are some dates here? 
print(unique(sigpro_aa$codproyecto)) #This has a mix of numbers and geographic info, while...
print(unique(sigpro_vin$codproyecto)) #This is just a number. 

#---------------------------------------------
# Format datasets as numeric
#---------------------------------------------
#Reformat columns to be numeric for merge - need to resolve why some NAs are getting created here. 
format_cols_numeric = function(dt, columns){
  new_dt = dt[, (columns):=lapply(.SD, as.numeric), .SDcols=columns]
  return(new_dt)
}

cols = c('codigoActividad', 'numeroInforme', 'codgrupo', 'condonesSabores', 'lubriSachet', 
         'codproyecto')
sigpro_aa <- format_cols_numeric(sigpro_aa, cols)
sigpro_its <- format_cols_numeric(sigpro_its, cols)

#-------------------------------------------------
# Merge datasets 
#-------------------------------------------------
# mergeVars <- c('codigounico')
# commonVars <- c('Age', 'codanno', 'codejecutor', 'codgrupo', 'codigoActividad', 'codigoTipoActividad', 
#                 'codigounico', 'codmes') #These are common variables across all files, but they may not be the same across all files. 
# 
# #Create a merged activity dataset by unique identifier. 
# activity_cols <- c('Age', 'Gender', 'codigounico', 'tipoActividad')
# activity_aa <- sigpro_aa[, .activity_cols]
# merged_file <- merge(sigpro_aa[, names(sigpro_aa)%in%activity_cols], sigpro_vin, by=mergeVars) #Can we get more of these rows to merge? 

#Create a merged treatment dataset by unique identifier 
#Create a merged diagnosis dataset by unique identifier
#-----------------------------------------------
# Prep the data. 
#-----------------------------------------------
# Fix diacritical marks and characters that weren't pulled in correctly by unicode 
fix_diacritics = function(x){
  replacement_chars = list('S'='S', 's'='s', 'Z'='Z', 'z'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
                           'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
                           'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
                           'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
                           'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y')
  replace_me <- paste(names(replacement_chars), collapse='')
  replace_with <- paste(replacement_chars, collapse = '')
  return(chartr(replace_me, replace_with, x))
}

fix_wonky_characters = function(x){ 
  gsub("AA‘OS", "ANOS", x)
  gsub("FundaciA³n", "Fundacion", x)
  gsub("DIAGNA“STICO", "DIAGNOSTICO", x)
}

cols = c('subgrupo', 'lugar', 'tipoActividad')
sigpro_aa = sigpro_aa[, (cols):=lapply(.SD, fix_diacritics), .SDcols=cols]
sigpro_aa = sigpro_aa[, (cols):=lapply(.SD, fix_wonky_characters), .SDcols=cols]




#Naomi's code - Pull in if helpful. 
# # Drop columns of personal identifying information
# total_data$birth_municipality = NULL
# total_data$birth_department = NULL
# total_data$identifier = NULL
# 
# # Translate data that we wil be using to English
# reason_eng = translate_data[1:14]
# risk_eng = translate_data[16:24]
# setnames(risk_eng, old = c("reason_for_visit", "reason_for_visit_eng"), new = c("risk_condition", "risk_condition_eng"))
# preg_eng = translate_data[26:31]
# setnames(preg_eng, old = c("reason_for_visit", "reason_for_visit_eng"), new = c("pregnancy_stage", "pregnancy_stage_eng"))
# 
# total_data = merge(total_data, reason_eng, by = "reason_for_visit")
# total_data = merge(total_data, risk_eng, by = "risk_condition")
# total_data = merge(total_data, preg_eng, by = "pregnancy_stage")
# 
# #Replace "-" with NAs
# total_data = total_data[, lapply(.SD, function(x) replace(x, which(x=='-'), NA))]
# 
# # Create date variable and calculate age
# total_data$date <- as.Date(with(total_data, paste(year, month, "1",sep="-")), "%Y-%m-%d")
# total_data$birth_date <- as.Date(paste0(total_data$birth_year, "-", total_data$birth_month, "-", total_data$birth_day))
# total_data$age <-  as.integer((total_data$date - total_data$birth_date) / 365)
# 
# # Drop age identifying columns
# total_data$birth_day = NULL
# total_data$birth_month = NULL
# total_data$birth_year = NULL
# total_data$birth_date = NULL
# 
# # Prep for mapping
# total_data$completed_hiv_screening_test = gsub("Si", "Yes", total_data$completed_hiv_screening_test)
# total_data$completed_hiv_screening_test = gsub("Reactivo", "Yes", total_data$completed_hiv_screening_test)
# 
# total_data$reason_for_visit = NULL
# total_data$pregnancy_stage = NULL
# total_data$risk_condition = NULL
# 
# # Find facility level for each facility
# total_data$level = ifelse(grepl("Hospital", total_data$service_type), 3,
#                           ifelse(grepl("Centro", total_data$service_type), 2, 
#                                  ifelse(grepl("Maternidad", total_data$service_type), 2, 1)))
# 
# # Count which visit number this is (to check how many poeple are attending the clinic)
# total_data = total_data[order(date)]
# total_data[, visit_num := seq_len(.N), by = id]

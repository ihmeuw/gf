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


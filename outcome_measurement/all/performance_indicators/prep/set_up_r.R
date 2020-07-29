#------------------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Sets up R to run PUDR indicator extraction. 
# DATE: Last updated June 2019. 
#------------------------------------------------------

#Read in packages 
library(data.table)
library(openxlsx)
library(readxl)
library(stringr)

#Set options 
options(scipen=100)
options(digits=6)

# ---------------------------------------
# Set global variables and filepaths.  
# ---------------------------------------

#J:drive filepaths
j = ifelse(Sys.info()[1]=='Windows','J:','/home/j')
dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/multi_country/performance_indicators/')
#Box filepaths - these should be used to source raw files, and to save final prepped files. 

box = paste0("C:/Users/", user, "/Box Sync/Global Fund Files/")

code_dir = paste0(repo_root, "outcome_measurement/all/performance_indicators/prep/")

prep_functions = list.files(paste0(code_dir, "_prep_functions"), full.names=TRUE)
for (file in prep_functions){
  source(file, encoding="UTF-8")
}

prepped_dir = paste0(dir, "pudr_indicator_extraction/prepped_data/")
book_dir = paste0(dir, "pudr_indicator_extraction/documentation/codebooks/") # repository for storing codebooks

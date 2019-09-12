#------------------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Sets up R to run PUDR indicator extraction. 
# DATE: Last updated June 2019. 
#------------------------------------------------------

#Read in packages 
library(data.table)
library(openxlsx)

#Set options 
options(scipen=100)
options(digits=6)

# ---------------------------------------
# Set global variables and filepaths.  
# ---------------------------------------

#J:drive filepaths
j = ifelse(Sys.info()[1]=='Windows','J:','/home/j')
dir = paste0(j, '/Project/Evaluation/GF/process_evaluation/')

code_dir = paste0(repo_root, "process_evaluation/")

prep_functions = list.files(paste0(code_dir, "_prep_functions"), full.names=TRUE)
for (file in prep_functions){
  source(file, encoding="UTF-8")
}

prepped_dir = paste0(dir, "pudr_indicator_extraction/prepped_data/")

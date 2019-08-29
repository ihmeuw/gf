#------------------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Sets up R to run budgeted unit costs database
# DATE: Last updated August 2019. 
#------------------------------------------------------

#Read in packages 
library(data.table)
library(readxl)

#Set options 
options(scipen=100)
options(digits=6)

# ---------------------------------------
# Set global variables and filepaths.  
# ---------------------------------------

#J:drive filepaths
j = ifelse(Sys.info()[1]=='Windows','J:','/home/j')
dir = paste0(j, '/Project/Evaluation/GF/vfm/unit_cost_data/gf_budgets/')

code_dir = paste0(repo_root, "vfm/budgeted_unit_costs/")

prep_functions = list.files(paste0(code_dir, "_prep_functions"), full.names=TRUE)
for (file in prep_functions){
  source(file, encoding="UTF-8")
}


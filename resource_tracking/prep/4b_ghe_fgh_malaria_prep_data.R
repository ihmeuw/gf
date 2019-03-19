# ----------------------------------------------------------------------
# AUTHOR: Emily Linebarger
# PURPOSE: Preps the GHE malaria data from the FGH team, + extractions PCE
#   has done. 
# DATE: Last updated March 2019. 
# ----------------------------------------------------------------------

#The input here is the output of the FGH prep code. 
fgh_malaria = readRDS(paste0(fgh_ghe_malaria_raw, "ghe_actuals_malaria.rds"))
#What data sources do you have for public expenditure? 
fgh_malaria = fgh_malaria[value_code == 'fs_malaria_domestic_public']


#Check PNCNS numbers against resource tracking database expenditure numbers 
drc_expenditures = readRDS(paste0(dir, "_gf_files_gos/cod/prepped_data/final_expenditures.rds"))

pce_extractions 
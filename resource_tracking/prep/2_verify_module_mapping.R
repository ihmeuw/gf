# ----------------------------------------------
# AUTHOR: Emily Linebarger, elineb@uw.edu 
# LAST UPDATED: November 2018 
# PURPOSE: Clean module mapping file from original version created by Naomi Provost/Irena Chen, 
#           archived on October 30, 2018. This cleaned version will have no duplicates between 
#           module, intervention, and disease that could lead to duplicate mappings, and 
#           close spellings of modules/interventions will be corrected to a single version. 
# ----------------------------------------------
###### Set up R / install packages  ###### 
# ----------------------------------------------
library(doBy)
# ---------------------------------------------
#Set global variables and read in files. 
# ---------------------------------------------
file_dir <- "J:/Project/Evaluation/GF/mapping/multi_country/intervention_categories/"
inFile <- "intervention_and_indicator_list.xlsx"
inSheet<- "module_mapping"
  
mod_map <- data.table(read_excel(paste0(file_dir, inFile), sheet=as.character(inSheet)))

# -------------------------------
#
#   VERIFICATION STEPS 
#
#--------------------------------

#Remove all duplicates of module, intervention and disease. These three variables matter because they're what merges with the raw budget data to pull in the intervention code. 
mod_map$duplicates <- duplicated(mod_map, by = c("module", "intervention", "disease"))
duplicates_check <- mod_map[mod_map$duplicates == TRUE]
if(nrow(duplicates_check)>0){
  print(unique(duplicates_check[, by = c("module", "intervention", "disease", "code")]))
  stop(paste0(print(nrow(duplicates_check)), " duplicates in key variables found in dataset."))
}
#N = 301. For these variables, we just need to make sure the merge and the coefficient is working correctly. 

duplicates_coeff_one <- duplicates_check[ which(duplicates_check$coefficient == 1), ]

#Check which grants these duplicates might have impacted. 
#Import final prepped files 
gtm_prep <- read.csv("J:/Project/Evaluation/GF/resource_tracking/gtm/prepped/prepped_budget_data.csv")
gtm_prep <- unique(gtm_prep, by = "fileName")
rows_gtm = nrow(gtm_prep)

cod_prep <- read.csv("J:/Project/Evaluation/GF/resource_tracking/cod/prepped/prepped_budget_data.csv")
cod_prep <- unique(cod_prep, by = "fileName")
rows_cod = nrow(cod_prep)

uga_prep <- read.csv("J:/Project/Evaluation/GF/resource_tracking/uga/prepped/prepped_budget_data.csv")
uga_prep <- unique(uga_prep, by = "fileName")
rows_uga = nrow(uga_prep)

#Isolate the files you're working with to just module and intervention to check merge
duplicates_check <- unique(duplicates_check[, c("module", "intervention")])
duplicates_check <- duplicates_check[, c("module", "intervention")]
colnames(duplicates_check)[1] <- "gf_module"
colnames(duplicates_check)[2] <- "gf_intervention"

#GUATEMALA 
gtm_merge <- merge(gtm_prep, duplicates_check, all.x=TRUE) 
affected_budgets_gtm_2017<-gtm_merge[ which(gtm_merge$year>=2017), ]
affected_rows<-nrow(affected_budgets_gtm_2017)
print("% of Guatemala data impacted")
print(affected_rows/rows_gtm)

#DRC 
cod_merge <- merge(cod_prep, duplicates_check, all.x=TRUE) 
affected_budgets_cod_2017<-cod_merge[ which(cod_merge$year>=2017), ]
affected_rows<-nrow(affected_budgets_cod_2017)
print("% of DRC data impacted")
print(affected_rows/rows_cod)

#UGANDA
uga_merge <- merge(uga_prep, duplicates_check, all.x=TRUE) 
affected_budgets_uga_2017<-uga_merge[ which(uga_merge$year>=2017), ]
affected_rows<-nrow(affected_budgets_uga_2017)
print("% of Uganda data impacted")
print(affected_rows/rows_uga)

#Drop created variables. 
mod_map$duplicates <-NULL

#Make sure there are no "NAs" or "All" in either module or intervention. 
check_na_all_modules<-mod_map[ which( mod_map$module == "all" | mod_map$module == "na")]

if(nrow(check_na_all_modules)>0){
    print(unique(check_na_all_modules[, c("module", "intervention"), with = FALSE]))
    stop(paste0(print(nrow(check_na_all_modules)), " modules have na or all as values."))
}

check_na_all_interventions<- mod_map[ which(mod_map$intervention == "all" | mod_map$intervention == "na")]
if(nrow(check_na_all_interventions)>0){
  print(unique(check_na_all_interventions[, c("module", "intervention"), with = FALSE]))
  stop(paste0(print(nrow(check_na_all_interventions)), " interventions have na or all as values."))
}

#Check for close spellings of the same module and intervention 
print(sort(unique(mod_map$module)))
print(sort(unique(mod_map$intervention)))
#We have some modules in all capital letters showing up here, with spaces. 


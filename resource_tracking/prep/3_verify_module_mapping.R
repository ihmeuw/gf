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
library(readxl)
library(data.table)
# ---------------------------------------------
#Set global variables and read in files. 
# ---------------------------------------------
  
map = read_xlsx('J:/Project/Evaluation/GF/mapping/multi_country/intervention_categories/intervention_and_indicator_list.xlsx', sheet='module_mapping')
map = data.table(map)

# -------------------------------
#
#   VERIFICATION STEPS 
#
#--------------------------------


#--------------------------------------------------------------------------------
#1. Remove all duplicates of module, intervention and disease. 
#     These three variables matter because they're what merges 
#     with the raw budget data to pull in the intervention code. 
#--------------------------------------------------------------------------------

map$duplicates <- duplicated(map, by = c("module", "intervention", "disease"))
duplicates_check <- map[map$duplicates == TRUE]
if(nrow(duplicates_check)>0){
  print(unique(duplicates_check[, by = c("module", "intervention", "disease", "code")]))
  stop(paste0(print(nrow(duplicates_check)), " duplicates in key variables found in dataset."))
}
#N = 302. For these variables, we just need to make sure the merge and the coefficient is working correctly. 


#--------------------------------------------------------------------------------
#2. Make sure you don't have any coefficients across unique 
#   observations of module, intervention, and disease that don't sum to 1.
#--------------------------------------------------------------------------------

duplicates_coeff_one <- duplicates_check[ which(duplicates_check$coefficient == 1), ]
duplicates_coeff_one <- duplicates_coeff_one[order(module, intervention)]

if (nrow(duplicates_coeff_one) != 0){
  print(unique(duplicates_coeff_one[, c("module", "intervention", "disease", "coefficient")])) #EMILY KEEP WORKING HERE- Right now it's okay to have coefficients of 1 if disease is unique. It looks like some dupes are being dropped 
  #that don't have the same coefficients. 
  stop("Module/Intervention/Disease duplicates with coefficients of 1!")
  
}


#--------------------------------------------------------------------------------
#3. Remove all cases where "na" or "all" was mapping to a specific code. ***EMILY KEEP WORKING HERE- CHECK IF CODE IS GOING TO 'UNSPECIFIED'. 
#--------------------------------------------------------------------------------

check_na_all_modules<-map[ which( map$module == "all" | map$module == "na")]

if(nrow(check_na_all_modules)>0){
    print(unique(check_na_all_modules[, c("module", "intervention"), with = FALSE]))
    stop(paste0(print(nrow(check_na_all_modules)), " modules have na or all as values."))
}

check_na_all_interventions<- map[ which(map$intervention == "all" | map$intervention == "na")]
if(nrow(check_na_all_interventions)>0){
  print(unique(check_na_all_interventions[, c("module", "intervention"), with = FALSE]))
  stop(paste0(print(nrow(check_na_all_interventions)), " interventions have na or all as values."))
}

#--------------------------------------------------------------------------------
# 4. Check for close spellings of the same module and intervention  ***EMILY KEEP WORKING HERE 
#--------------------------------------------------------------------------------
print(sort(unique(map$module)))
print(sort(unique(map$intervention)))
#We have some modules in all capital letters showing up here, with spaces. 



#--------------------------------------------------------------------------------
# 5. Make sure that duplicates of module/intervention don't have different mapping codes. ***EMILY KEEP WORKING HERE
#--------------------------------------------------------------------------------

# identify rows with more than one code per module/intervention
map_unique = unique(map[,c('module','intervention','code','coefficient')])
dups = duplicated(map_unique[,c('module','intervention')])
bad_combos = map_unique[dups==TRUE & coefficient==1 & !intervention %in% c('all','na'), c('module','intervention')]
map_subset = merge(map, bad_combos, by=c('module','intervention'))

# grant management, PBF and TB/HIV are allowed to exist in more than one disease
allowableModules = c('gestiondeprogramas', 'gestiondeprogramme', 'gestiondessubventions', 
                     'performancebasedfinancing', 'programmanagement', 'tbhiv', 'tbhivcollaborativeactivities', 
                     'tbvih', 'tuberculosevih', 'tuberculosisvih')
map_subset = map_subset[!module %in% allowableModules]

if(nrow(map_subset)>0){
  print(unique(map_subset[, c("module", "intervention"), with = FALSE]))
  stop(paste0(print(nrow(map_subset)), " duplicates in module/intervention have different mapping codes"))
}

#--------------------------------------------------------------------------------
# 6. You should only have one phrase in each language mapping to an intervention code.
#     For budget line items that don't match exactly, we should change them line by line 
#     in an R-script so they are documented. ***EMILY KEEP WORKING HERE
#--------------------------------------------------------------------------------

#7- make sure we don't have any codes mapped where their prefix doesn't match their disease. 



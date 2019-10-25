# -------------------------------------------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Review new mappings as compared to archived map
# DATE: April 3, 2019
# --------------------------------------------------------------------------------

rm(list=ls())

# ----------------------------------------------
# STEP 1: SET UP R
# ----------------------------------------------

user = "elineb" #Change to your username 
code_dir = ifelse(Sys.info()[1]=='Windows', paste0("C:/Users/", user, "/Documents/gf/"), paste0('/homes/', user, '/gf/'))
source(paste0(code_dir, "resource_tracking/prep/_common/set_up_r.R"), encoding="UTF-8")
library(openxlsx)

map_v3 = readRDS(paste0(mapping_dir, "gf_mapping.rds"))
map_v2 = fread(paste0(mapping_dir, "archive/Module Mapping Version 2/gf_mapping.csv"))
map_v1 = data.table(read.xlsx("J:/Project/Evaluation/GF/resource_tracking/modular_framework_mapping/archive/Module Mapping Version 1/Copy of intervention_and_indicator_list_NEW.xlsx", 
                              sheet = "module_mapping"))

#----------------------------------------------------------------------
#Standardize variables across map
#----------------------------------------------------------------------

map_v1 = map_v1[, .(module, intervention, code, coefficient, disease)]
map_v2 = map_v2[, .(module, intervention, code, coefficient, disease)]
map_v3 = map_v3[, .(module, intervention, code, coefficient, disease)]

setnames(map_v1, c('code', 'coefficient'), c('code_v1', 'coefficient_v1'))
setnames(map_v2, c('code', 'coefficient'), c('code_v2', 'coefficient_v2'))
setnames(map_v3, c('code', 'coefficient'), c('code_v3', 'coefficient_v3'))

map_v1 = map_v1[order(module, intervention, disease, code_v1)]
map_v2 = map_v2[order(module, intervention, disease, code_v2)]
map_v3 = map_v3[order(module, intervention, disease, code_v3)]

#----------------------------------------------------------------------
#Compare different versions of the map to each other 
#----------------------------------------------------------------------
m3_to_m2 = merge(map_v3, map_v2, by = c('module', 'intervention', 'disease'), all.x = TRUE)
unmatched = m3_to_m2[is.na(code_v2)] #Check this, but it isn't necesarily bad because we've changed the way we're processing the strings of the mdoules, and we have some new files. 
discrepancies_2_3 = m3_to_m2[ (code_v2 != code_v3 & !is.na(code_v2)) | (round(coefficient_v2, 3) != round(coefficient_v3, 3) & !is.na(coefficient_v2))]
write.csv(discrepancies_2_3, paste0(mapping_dir, "Map discrepancies between version 2 and version 3.csv"), row.names = FALSE)

m3_to_m1 = merge(map_v3, map_v1, by = c('module', 'intervention', 'disease'), all.x = TRUE)
unmatched = m3_to_m1[is.na(code_v1)] #Check this, but it isn't necesarily bad because we've changed the way we're processing the strings of the mdoules, and we have some new files. 
discrepancies_1_3 = m3_to_m1[ (code_v1 != code_v3 & !is.na(code_v1)) | (round(coefficient_v1, 3) != round(coefficient_v3, 3) & !is.na(coefficient_v1))]
write.csv(discrepancies_1_3, paste0(mapping_dir, "Map discrepancies between version 1 and version 3.csv"), row.names = FALSE)

#----------------------------------------------------------------------
# Which rows are getting moved away from RSSH, or getting called program 
# management, in the most recent version of the map? 
#----------------------------------------------------------------------
map_v3[, cat:=substring(code_v3, 1, 1)]

# RSSH
rssh_strings <- c('hss', 'rssh', 'srss', 'ssrs', 'fss', 'rss', 'systcmesdesanteresiliantsetperennes')

map_v3[, review_rssh:=FALSE] #Create a flag to review cases where module has been classified away from RSSH. 
map_v3[grep(paste0(rssh_strings, collapse = "|"), module), review_rssh:=TRUE] #If the module has an RSSH string, review it. 
review_mods = unique(map_v3[review_rssh == FALSE, .(module)])

rssh_mods = c('gestiondesachatsetdelachainedapprovisionnement', 'financialmanagementsystems', 'healthmanagementinformationsystemandmonitoringandevaluation', 'humanresources', 'humanresourcesforhealthincludingcommunityhealthworkers', 
              'integratedservicedeliveryandqualityimprovement', 'me', 'monitoringandevaluation', 'nationalhealthstrategies', 'procurementandsupplychainmanagementsystems', 'procurementandsupplymanagement', 
              'renforcementdessystcmescommunautaires', 'rsssuivievaluation', 'suivietevaluation')
map_v3[module%in%rssh_mods, review_rssh:=TRUE] #If the module is part of one of these RSSH mods, also review it. 
map_v3[review_rssh==TRUE & cat=='R', review_rssh:=FALSE] #But, for the ones you've called 'true' above, if they're already classified as RSSH you don't need to review them. 

#Program management
map_v3[, review_pm:=FALSE]
pm_codes = c('H9', 'H9_1', 'H9_2', 'H9_3', 'T4', 'T4_1', 'T4_2', 'T4_3', 'M4', 'M4_1', 'M4_2', 'M4_3', 'R8', 'R8_1', 'R8_2', 'R8_3')
map_v3[code_v3%in%pm_codes, review_pm:=TRUE]

#These modules are pretty uncontroversial to call program management. 
ok_pm_mods = c('programmanagement', 'programmanagementandadministration', 'gestiondeprogramas', 'gestiondessubventions', 'gestiondeprogramme', 'nfmmoduleprogrammanagement')
map_v3[module%in%ok_pm_mods, review_pm:=FALSE]

review = map_v3[review_rssh==TRUE | review_pm==TRUE]
write.csv(review, paste0(mapping_dir, "Review RSSH and PM reclassifications.csv"), row.names = FALSE)

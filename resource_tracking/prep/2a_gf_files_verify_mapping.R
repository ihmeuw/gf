# ----------------------------------------------
# AUTHOR: Emily Linebarger, elineb@uw.edu 
# LAST UPDATED: November 2018 
# PURPOSE: Clean module mapping file from original version created by Naomi Provost/Irena Chen, 
#           archived on October 30, 2018. This cleaned version will have no duplicates between 
#           module, intervention, and disease that could lead to duplicate mappings, and 
#           close spellings of modules/interventions will be corrected to a single version. 
#
# ---------------------------------------------
#Set global variables and read in files. 
# This function will return a cleaned version of the module 
# map to be used for mapping modules and interventions. 
# ---------------------------------------------

post_2017_map = readRDS(paste0(j, "/Project/Evaluation/GF/mapping/multi_country/intervention_categories/post_2017_map.rds"))
post_2017_map[, module:=as.character(module)]
post_2017_map[, intervention:=as.character(intervention)]
post_2017_map[, disease:=as.character(disease)]
post_2017_map[, loc_name:=as.character(loc_name)]
post_2017_map[, lang:=as.character(lang)]

post_2017_map = post_2017_map[, .(code, module, intervention, coefficient, disease)]

all_interventions = fread(paste0(j, "/Project/Evaluation/GF/mapping/multi_country/intervention_categories/all_interventions.csv"))
all_eng = all_interventions[, .(code, module_eng, intervention_eng, disease)]
setnames(all_eng, old=c('module_eng', 'intervention_eng'), new=c('module', 'intervention'))
# all_fr = all_interventions[, .(code, module_fr, intervention_fr, disease)]
# setnames(all_fr, old=c('module_fr', 'intervention_fr'), new=c('module', 'intervention'))
# all_esp = all_interventions[, .(code, module_esp, intervention_esp, disease)]
# setnames(all_esp, old=c('module_esp', 'intervention_esp'), new=c('module', 'intervention'))
# 
all_langs = list(all_eng)
all_langs = rbindlist(all_langs)
all_langs[, coefficient:=1]
all_langs = strip_chars(all_langs)
all_langs = all_langs[, -c('orig_module', 'orig_intervention')]

module_map = rbind(post_2017_map, all_langs)
module_map = unique(module_map)

module_map = prep_map(module_map)
  
# -------------------------------
#       FORMAT DATA 
#--------------------------------
  
  all_interventions = fread(paste0(dir, "mapping/multi_country/intervention_categories/all_interventions.csv"))
  setnames(all_interventions, old=c('module_eng', 'intervention_eng', 'module_fr', 'intervention_fr', 'abbrev_mod_eng'), 
           new=c('gf_module', 'gf_intervention', 'gf_module_fr', 'gf_intervention_fr', 'abbreviated_module'))

  original_map <- copy(map) #Save an original copy for comparison later 
  new_rows <- fread(paste0(dir, "mapping/multi_country/intervention_categories/gf_mapping_additions.csv")) #Add in new rows to previously approved map
  new_rows = new_rows[, .(module, intervention, code, coefficient, disease)]
  map = rbind(map, new_rows, fill = TRUE)

# -------------------------------
#
#   CLEAN DATA BEFORE VALIDATING 
#
#--------------------------------
  #These are the variables that are merged onto the raw data, so it's important to check duplicates with these. 
  keyVars = c('module', 'intervention', 'disease')
  
#--------------------------------------------------------------------------------
# CLEANING- Dropping modules/interventions that map to codes that don't make sense. 
#--------------------------------------------------------------------------------


#--------------------------------------------------------------------------------
# CLEANING- Removing typos and close string matches from map  
#--------------------------------------------------------------------------------

#--------------------------------------------------------------------------------
# CLEANING- Replacing modules/interventions that are typos or unspecified. 
#--------------------------------------------------------------------------------


#--------------------------------------------------------------------------------
# CLEANING (Checks 1 & 2)- Remove duplicates in module, intervention, and disease 
# with coefficients of 1, then check. 
#--------------------------------------------------------------------------------


#--------------------------------------------------------------------------------
# CLEANING (Check 3) Remove specific mappings to 'na', 'all', or 'other'. 
#--------------------------------------------------------------------------------

#--------------------------------------------------------------------------------
# CLEANING (Check 4) Remove close spellings of module/intervention. 
#--------------------------------------------------------------------------------

#--------------------------------------------------------------------------------
# CLEANING (Check 5)- Remove duplicates in module/intervention 
#     with different mapping codes.  
#--------------------------------------------------------------------------------


#--------------------------------------------------------------------------------
# CLEANING (Check 6) Remove duplicate mappings in the same language
#--------------------------------------------------------------------------------

#--------------------------------------------------------------------------------
# CLEANING (Check 7)- Fix cases where module/intervention is mapping
#   to incorrect code by disease. 
#--------------------------------------------------------------------------------


#--------------------------------------------------------------------------------
# CLEANING (Check 8)- Remove RSSH mappings that don't make sense. 
#--------------------------------------------------------------------------------

#--------------------------------------------------------------------------------
# CLEANING (Check 10) Make sure that program management, performance based financing, and unspecified
#     in 'module' are NOT categorized as RSSH, unless they're with an RSSH grant. 
#   (They should have the disease of the grant they're in)
#--------------------------------------------------------------------------------


#--------------------------------------------------------------------------------
# CLEANING - remove duplicates created by running checks above.
#--------------------------------------------------------------------------------
module_map = unique(module_map)

#-------------------------------------------------------------------------------
#
# RUN VALIDATION CHECKS ON module_map, AND CORRECT module_map IN CORRESPONDING SECTIONS ABOVE 
#
#-------------------------------------------------------------------------------

#--------------------------------------------------------------------------------
# 1. Make sure you don't have any coefficients across unique 
#   observations of module, intervention, and disease that sum to 1.
#--------------------------------------------------------------------------------
duplicates_check <- module_map[duplicated(module_map, by = keyVars) == TRUE, ]
duplicates_coeff_one <- duplicates_check[coefficient == 1]
duplicates_coeff_one <- merge(duplicates_coeff_one, module_map, by = keyVars) #Merge back onto module_map because some duplicates don't have coefficients of 1. 
duplicates_coeff_one <- duplicates_coeff_one[order(module, intervention)]

# Check
if (nrow(duplicates_coeff_one) != 0 & include_stops == TRUE){
  print(duplicates_coeff_one) 
  stop("Module/Intervention/Disease duplicates with coefficients of 1!")
}

#--------------------------------------------------------------------------------
# 2. Make sure you don't have any coefficients across unique 
#   observations of module, intervention, and disease that sum to 
#   greater than or less than 1 (budget shrinking or growing) 
#--------------------------------------------------------------------------------
redistribution_map <- module_map[coefficient!=1]
redistribution_map[, coeff_sum:=round(sum(coefficient), 1), by=keyVars]

redistribution_error = redistribution_map[coeff_sum != 1.0]
if(nrow(redistribution_error)>0 & include_stops == TRUE){
  print(unique(redistribution_error[, c("module", "intervention", "code"), with = FALSE]))
  stop(paste0(print(nrow(redistribution_error)), " lines have coefficients that don't sum to 1 by key variables."))
}

#--------------------------------------------------------------------------------
# 3. Remove all cases where "na" or "all" was mapping to a specific code. ***EMILY KEEP WORKING HERE- CHECK IF CODE IS GOING TO 'UNSPECIFIED'. 
#--------------------------------------------------------------------------------

unspecified_mods <- c('all', 'na', 'other', 'unspecified', 'otro', 'otherspecify')
check_unspecified<-module_map[module %in% unspecified_mods | intervention %in% unspecified_mods]
check_unspecified <- check_unspecified[nchar(code)>2] #It's okay to have some general codes with unspecified mods; but should also review these. 
check_unspecified <- unique(check_unspecified)

#EKL commenting this out for now -- until we apply NLP it's okay to have all or na as values. 2/7/19
# if(nrow(check_unspecified)>0 & include_stops == TRUE){
#     print(unique(check_unspecified[, c("module", "intervention", "code"), with = FALSE]))
#     stop(paste0(print(nrow(check_unspecified)), " modules have na or all as values."))
# }

#--------------------------------------------------------------------------------
# 4. Check for close spellings of the same module and intervention  ***EMILY KEEP WORKING HERE 
#--------------------------------------------------------------------------------
#print(sort(unique(module_map$module)))
#print(sort(unique(module_map$intervention)))
#We have some modules in all capital letters showing up here, with spaces. 


#--------------------------------------------------------------------------------
# 5. Make sure that duplicates of module/intervention don't have different mapping codes. ***EMILY KEEP WORKING HERE
#--------------------------------------------------------------------------------

# identify rows with more than one code per module/intervention
map_unique = unique(module_map[,c('module','intervention','code','coefficient')])
dups = duplicated(map_unique[,c('module','intervention')])
bad_combos = map_unique[dups==TRUE & coefficient==1 & !intervention %in% c('all','na'), c('module','intervention')]
map_subset = merge(module_map, bad_combos, by=c('module','intervention'))

# grant management, PBF and TB/HIV are allowed to exist in more than one disease
allowableModules = c('gestiondeprogramas', 'gestiondeprogramme', 'gestiondessubventions', 
                     'performancebasedfinancing', 'programmanagement', 'tbhiv', 'tbhivcollaborativeactivities', 
                     'tbvih', 'tuberculosevih', 'tuberculosisvih', 'hivtbcollaborativeactivities')
map_subset = map_subset[!module %in% allowableModules]

#RSSH modules are allowed to exist in more than one disease 
map_subset[, prefix:=substr(code, 0, 1)]
map_subset = map_subset[!(prefix=='R')]

map_subset = map_subset[order(module, intervention)]

#This is okay to ignore. It's okay for the same module, intervention, and code to be included for more than one disease. EKL 2/7/19 
# if(nrow(map_subset)>0 & include_stops == TRUE){
#   print(unique(map_subset[, c("module", "intervention", "code", "disease"), with = FALSE]))
#   stop(paste0(print(nrow(map_subset)), " duplicates in module/intervention have different mapping codes"))
# }

#--------------------------------------------------------------------------------
# 6. You should only have one phrase in each language mapping to an intervention code.
#     For budget line items that don't match exactly, we should change them line by line 
#     in an R-script so they are documented. ***EMILY KEEP WORKING HERE
#--------------------------------------------------------------------------------

check_multi_mappings <- module_map[, frequency:=1]
check_multi_mappings <- check_multi_mappings[, code_count:=sum(frequency), by = code]
check_multi_mappings <- check_multi_mappings[code_count > 3] #It's okay to have up to 3 languages in the dataset. Should check to verify these are in all 3 languages sometime. 
check_multi_mappings <- check_multi_mappings[order(-code_count)]

#--------------------------------------------------------------------------------
# 7. Make sure we don't have any codes mapped where their prefix 
#     doesn't match their disease. ***EMILY KEEP WORKING HERE
#--------------------------------------------------------------------------------
check_prefix = module_map[, prefix:=substr(code, 0, 1)]
check_prefix$error = ifelse((check_prefix$disease == "hiv" & check_prefix$prefix != "H") |
                              (check_prefix$disease == "tb" & check_prefix$prefix != "T") |
                              (check_prefix$disease == "malaria" & check_prefix$prefix != 'M') | 
                              (check_prefix$disease == "hiv/tb" & (check_prefix$prefix != 'H' & check_prefix$prefix != 'T')) |
                              (check_prefix$disease == "hss" & check_prefix$prefix != "R"), TRUE, FALSE)
check_prefix = check_prefix[error == TRUE] #Emily should check with David how we want to resolve all of these, including RSSH, but for now exclude 'R' from check. 

check_prefix_ltd <- check_prefix[prefix != 'R']
# 
# if(nrow(check_prefix_ltd)>0 & include_stops == TRUE){
#   print(unique(check_prefix_ltd[, c("module", "intervention", 'code', 'disease'), with = FALSE]))
#   stop(paste0(print(nrow(check_prefix_ltd)), " errors in applying code for given disease")) #Check with David here. 
# }


#--------------------------------------------------------------------------------
# 8. Add a check to verify all RSSH modules/interventions look like they belong there.
#     ***EMILY KEEP WORKING HERE. NEED TO ONLY FLAG PROGRAM MANAGEMENT AND PERFORMANCE-BASED FINANCING.
#--------------------------------------------------------------------------------

check_rssh <- module_map[substr(code, 0, 1) == 'R']
check_rssh <- unique(check_rssh, by = 'module')
check_rssh$verified <- FALSE 
verified_prefix <- c('rssh', 'ssrs', 'fss', 'rss')
for(i in 1:length(verified_prefix)){
  check_rssh[, verified:=grepl(verified_prefix[[i]], check_rssh$module)]
  check_rssh = check_rssh[verified == FALSE]
}

#Add modules that should be classified as RSSH to this list 
okay_modules <- c('healthinformationsystemsandme', 'healthmanagementinformationsystemandmonitoringandevaluation', 'healthsystemsstrengthening', 
                  'communitysystemsstrengthening', 'infrastructure', 'infrastructure1', 
                  'systcmesdesanteresiliantsetperennesressourceshumainespourlasanteycomprisagentsdesantecommunautaires', 
                  'systcmesdesanteresiliantsetperennessystcmedegestiondelinformationsanitaireetsuivietevaluation', 'rsssuivievaluation', 'fortalecimientodelossistemascomunitarios', 
                  'hivhealthsystemsstrengthening', 'hivsupportiveenvironment', 'tbhealthsystemsstrengthening', 'malsupportiveenvironment', 'malhealthsystemsstrengthening', 
                  'procurementsupplychainmanagementpscm', 'servicedelivery', 'procurementandsupplymanagement', 'informationsystem', 'enquetes', 'surveys', 
                  'supportiveenvironmentstrengtheningofcivilsocietyandinstitutionalcapacitybuilding', 'rscressourceshumainesrenforcementdescompetencespourlaprestationdeservicesleplaidoyeretleleadership', 
                  'routinereporting', 'renforcementdessystemescommunautaires', 'renforcementdelasocietecivileetdescapacitesinstitutionnelles', 'suivietevaluationenquete', 'suivietevaluation', 
                  'policyandgovernance', 'humanresources', 'informationsystems', 'humanresoursetatrainingpsmcostsinfrastructuremonitoringandevaluation', 'informationsystemoperationalresearch', 
                  'politiqueetgouvernance')

problem_mods <- c('removinglegalbarrierstoaccess', 'programmanagementandadministration', 'otherspecify', 'other', 'otherunidentified', 'malprevention', 'beyondtb', 'tbtreatment', 'maltreatment', 
                  'preventionbccmassmedia', 'preventionbcccommunityoutreach', 'programmanagement', 'gestiondelasubvention', 'gestiondeprogramme', 'gestiondeprogramas', 'performancebasedfinancing', 
                  'financialmanagement', 'supportiveenvironmentprogrammanagementandadministration', 'paymentforresults', 'planningandadministrationcosts', 'palpracticalapproachtolunghealth', 
                  'gestiondeprogrammegestiondesubvention', 'gestiondeprogrammepolitiqueplanificationcoordinationetgestion', 'gestiondessubventions', 'gestionfinancicre')
check_rssh[module %in% okay_modules, verified:=TRUE]
check_rssh = check_rssh[verified == FALSE]

check_rssh = check_rssh[order(module)]

# if(nrow(check_rssh)>0 & include_stops == TRUE){
#   print("Unverified RSSH modules:")
#   print(unique(check_rssh[!(module %in% problem_mods), .(module)]))
# }

#print(unique(check_rssh[module %in% problem_mods, .(module, intervention, code)])) #Need to check with David on these. 
module_map[, prefix:=NULL]
#--------------------------------------------------------------------------------
# 9. Add a check to verify that if observations have the same module, intervention, 
#    and code, they should have the same coefficient. 
#     ***EMILY KEEP WORKING HERE. 
#--------------------------------------------------------------------------------

#--------------------------------------------------------------------------------
# 10. Make sure that program management, performance based financing, and unspecified
#     in 'module' are NOT categorized as RSSH, unless they're with an RSSH grant. 
#   (They should have the disease of the grant they're in)
#--------------------------------------------------------------------------------
hiv_codes <- c('H9', 'H9_1', 'H9_2', 'H9_3', 'H98', 'H99')
tb_codes <- c('T4', 'T4_1', 'T4_2', 'T4_3', 'T98', 'T99')
mal_codes <- c('M4', 'M4_1', 'M4_2', 'M4_3', 'M98', 'M99')
rssh_codes <- c('R8', 'R8_1', 'R8_2', 'R8_3', 'R98', 'R99')

check_rssh_cats <- module_map[code%in%rssh_codes & (disease != 'rssh' & disease != 'hss')]
# if(nrow(check_rssh_cats)>0 & include_stops == TRUE){
#   print(unique(check_rssh_cats[, c("module", "intervention", 'code', 'disease'), with = FALSE]))
#   stop(paste0(print(nrow(check_rssh_cats)), " cases where general modules are incorrectly classified as RSSH")) #Check with David here. 
# }

#--------------------------------------------------------------------------------
# 11. Review cases where you have an unspecified module/intervention and a coefficient < 1
#--------------------------------------------------------------------------------
unspecified = module_map[module=='unspecified' | module=='all' | intervention == 'all' | intervention == 'unspecified' | is.na(module) | is.na(intervention)]
unspecified = unspecified[coefficient!=1][order(module, intervention)]

# if(nrow(unspecified)>0 & include_stops == TRUE){
#   print(unique(unspecified[, c("module", "intervention", 'code', 'disease'), with = FALSE]))
#   stop(paste0(print(nrow(unspecified)), " cases where unspecified module/interventions are split among several interventions")) #Check with David here. 
# }

#--------------------------------------------------------------------------------
# Merge mapped codes to final mappings 
#--------------------------------------------------------------------------------
    module_map = module_map[, .(code, module, intervention, coefficient, disease)]
    all_interventions = all_interventions[, -'disease']
    module_map = merge(module_map, all_interventions, by='code')
    
    stopifnot(nrow(module_map[is.na(gf_module)])==0)
#--------------------------------------------------------------------------------
#Write final mapp and .diff files for comparison
#--------------------------------------------------------------------------------

  write.csv(module_map, paste0(dir, "mapping/multi_country/intervention_categories/gf_mapping.csv"))

  #Write a "diff" file to repository to make comparing changes easier. 
  module_map = module_map[, .(code, module, intervention, coefficient, disease, gf_module, gf_intervention, abbreviated_module)]
  removed_rows = anti_join(original_map, module_map)
  write.csv(removed_rows, paste0(code_loc, "resource_tracking/proposed_deletions_mod_map.csv"))
  
  added_rows = anti_join(module_map, original_map)
  write.csv(added_rows, paste0(code_loc, "resource_tracking/proposed_additions_mod_map.csv"))
  

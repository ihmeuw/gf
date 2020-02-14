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

# Read in most common raw mappings from 2018-2020 files. 
post_2017_map = readRDS(paste0(mapping_dir, "post_2017_map.rds"))
post_2017_map[, module:=as.character(module)]
post_2017_map[, intervention:=as.character(intervention)]
post_2017_map[, disease:=as.character(disease)]
post_2017_map[, loc_name:=as.character(loc_name)]
post_2017_map[, lang:=as.character(lang)]

post_2017_map = post_2017_map[, .(code, module, intervention, coefficient, disease)]

# Merge the 2018-2020 module map onto the module/intervention pairs pulled from raw data. 
# This code never needs to be rerun (unless the framework changes!), just leaving for documentation. Emily Linebarger 2/12/2020 
# map_18_20_hiv = data.table(read_xlsx(paste0(mapping_dir, "2018-2020 Modular Framework.xlsx"), sheet="HIV Interventions"))
# map_18_20_hiv[, disease:="hiv"]
# map_18_20_tb = data.table(read_xlsx(paste0(mapping_dir, "2018-2020 Modular Framework.xlsx"), sheet="TB Interventions"))
# map_18_20_tb[, disease:="tb"]
# map_18_20_malaria = data.table(read_xlsx(paste0(mapping_dir, "2018-2020 Modular Framework.xlsx"), sheet="Malaria Interventions"))
# map_18_20_malaria[, disease:="malaria"]
# map_18_20_rssh = data.table(read_xlsx(paste0(mapping_dir, "2018-2020 Modular Framework.xlsx"), sheet="RSSH Interventions"))
# map_18_20_rssh[, disease:="rssh"]
# 
# map_18_20 = rbindlist(list(map_18_20_hiv, map_18_20_tb, map_18_20_malaria, map_18_20_rssh))
# saveRDS(map_18_20, paste0(mapping_dir, "2018_2020_MF.rds"))

map_18_20 = readRDS(paste0(mapping_dir, "2018_2020_MF.rds"))

# Take the full list of interventions, strip the diacritics and spaces, and add these onto the map as valid raw options. 
all_eng = map_18_20[, .(code, gf_module, gf_intervention, disease)]
setnames(all_eng, old=c('gf_module', 'gf_intervention'), new=c('module', 'intervention'))
all_fr = map_18_20[, .(code, gf_module_fr, gf_intervention_fr, disease)]
setnames(all_fr, old=c('gf_module_fr', 'gf_intervention_fr'), new=c('module', 'intervention'))
all_esp = map_18_20[, .(code, gf_module_esp, gf_intervention_esp, disease)]
setnames(all_esp, old=c('gf_module_esp', 'gf_intervention_esp'), new=c('module', 'intervention'))

all_langs = rbindlist(list(all_eng, all_fr, all_esp))
all_langs[, coefficient:=1]
all_langs = strip_chars(all_langs)
all_langs = all_langs[, -c('orig_module', 'orig_intervention')]

module_map = rbind(post_2017_map, all_langs)
module_map = unique(module_map) # This is your new 'Master' list for 2018-2020 raw extracted module/intervention pairs. 
  
# -------------------------------
#       FORMAT DATA 
#--------------------------------
  
  all_interventions = fread(paste0(mapping_dir, "all_interventions.csv"))
  setnames(all_interventions, old=c('module_eng', 'intervention_eng', 'module_fr', 'intervention_fr', 'abbrev_mod_eng'), 
           new=c('gf_module', 'gf_intervention', 'gf_module_fr', 'gf_intervention_fr', 'abbreviated_module'))

  original_map <- copy(module_map) #Save an original copy for comparison later 
  new_rows <- fread(paste0(mapping_dir, "gf_mapping_additions.csv")) #Add in new rows to previously approved map
  new_rows = new_rows[, .(module, intervention, code, coefficient, disease)]
  module_map = rbind(module_map, new_rows, fill = TRUE)
  
  #-------------------------------------------------------------------------------
  # Expand the map for HIV/TB and RSSH 
  #------------------------------------------------------------------------------
  # If one module/intervention maps to RSSH for one disease (hiv, tb, or malaria)
  #   then it should also map to the same disease for the other two.
  all_rssh = module_map[substr(code, 1, 1)=='R']
  #Remove any program management or unspecified
  all_rssh = all_rssh[!code%in%c('R8', 'R8_1', 'R8_2', 'R8_3', 'R98', 'R99')]
  mal_rssh = copy(all_rssh)
  mal_rssh[, disease:='malaria']
  hiv_rssh = copy(all_rssh)
  hiv_rssh[, disease:='hiv']
  tb_rssh = copy(all_rssh)
  tb_rssh[, disease:='tb']
  hivtb_rssh = copy(all_rssh)
  hivtb_rssh[, disease:='hiv/tb']

  all = list(mal_rssh, hiv_rssh, tb_rssh, hivtb_rssh, module_map)
  module_map = rbindlist(all, use.names=TRUE)

  # All codes that are okay under HIV OR TB will also be okay if they match with the same
  # line item from a specific HIV/TB grant.
  hiv = module_map[substr(code, 1, 1)=='H']
  #Remove program management, PBF, and unspecified
  hiv = hiv[!code%in%c('H13, H13_1', 'H13_2', 'H13_3', 'H98', 'H99')]
  tb = module_map[substr(code, 1, 1)=='T']
  #Remove program management, TB/HIV, PBF, and Unspecified.
  tb = tb[!substr(code, 1, 2)%in%c('T2', 'T4', 'T9')]
  hiv[, disease:='hiv/tb']
  tb[, disease:='hiv/tb']

  hivtb = list(hiv, tb, module_map)
  module_map = rbindlist(hivtb, use.names=TRUE)

  module_map = unique(module_map) #OK to here

# -------------------------------
#
#   CLEAN DATA BEFORE VALIDATING 
#
#--------------------------------
  #These are the variables that are merged onto the raw data, so it's important to check duplicates with these. 
  keyVars = c('module', 'intervention', 'disease')
  
  #Remove whitespace from 'code' column before doing checks below
  module_map[, code:=trimws(code)]
  all_interventions[, code:=trimws(code)]
  
  #Drop out French, Spanish, and abbreviated modules for now.  
  #module_map = module_map[, -c('gf_intervention_fr', 'gf_module_fr', 'module_esp', 'intervention_esp', 'abbreviated_module')]
  
#--------------------------------------------------------------------------------
# CLEANING- Dropping modules/interventions that map to codes that don't make sense. 
#--------------------------------------------------------------------------------
 #These are new changes after the way we've modified the 'general' and 'other' mappings - EKL 5/1/19
  module_map[module=="preventionprogramsforadolescentsandyouthinandoutofschool" & intervention == "unspecified", code:="H8"]
  module_map[module=="tbhiv" & intervention=="keypopulationstbhivothers" & disease=="hiv/tb", code:="H11_5"]

#--------------------------------------------------------------------------------
# CLEANING- Removing typos and close string matches from map  
#--------------------------------------------------------------------------------

#--------------------------------------------------------------------------------
# CLEANING- Replacing modules/interventions that are typos or unspecified. 
#--------------------------------------------------------------------------------
 #Fixing diseases that were unspecified 
  module_map[code=="H99" & disease!='hiv' & disease!='hiv/tb', disease:='hiv']
  module_map[code=="M99", disease:='malaria']
  module_map[code=="T99", disease:="tb"]
  
  module_map[code=="H99", abbreviated_module:="Unspecified"]
  module_map[code=="M99", abbreviated_module:="Unspecified"]
  module_map[code=="T99", abbreviated_module:="Unspecified"] #Ok to here. 
  

#--------------------------------------------------------------------------------
# CLEANING (Checks 1 & 2)- Remove duplicates in module, intervention, and disease 
# with coefficients of 1, then check. 
#--------------------------------------------------------------------------------
module_map = module_map[!(code=="H10_8" & module=="tbhiv" & intervention=="collaborativeactivitieswithotherprogramsandsectorstbhiv")]
module_map = module_map[!(code=="H99" & module=="unspecified" & intervention=="unspecified" & coefficient<1)]
module_map = module_map[!(code=="T99" & module=="unspecified" & intervention=="unspecified" & coefficient<1)]
module_map = module_map[!(code=="M99" & module=="unspecified" & intervention=="unspecified" & coefficient<1)]
module_map = module_map[!(code=="R99" & module=="unspecified" & intervention=="unspecified" & coefficient<1)]

#9/10/2019 EL 
module_map = module_map[!(code=="T1_10" & disease=="hiv/tb" & module=="treatmentcareandsupport" & intervention=="treatmentmonitoringdrugresistancesurveillance")]

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
module_map = unique(module_map) #Ok to here. 

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
redistribution_map[, coeff_sum:=round(sum(coefficient), 3), by=keyVars]

redistribution_error = redistribution_map[coeff_sum!=1.000]
if(nrow(redistribution_error)>0 & include_stops == TRUE){
  print(unique(redistribution_error[, c("module", "intervention", "code"), with = FALSE]))
  stop(paste0(print(nrow(redistribution_error)), " lines have coefficients that don't sum to 1 by key variables."))
}
 #Ok to here. 
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

check_prefix_ltd <- check_prefix[prefix != 'R'] #Ok to here. 
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
module_map[, prefix:=NULL] #Ok to here. 
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
# 
# check_rssh_cats <- module_map[code%in%rssh_codes & (disease != 'rssh' & disease != 'hss')]
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
# 12. Make sure that "other" and "general" are assigned correctly
#--------------------------------------------------------------------------------

#It's an error if the intervention is "unspecified", and it's in the "other" category. 
unspecified = module_map[intervention=="unspecified", .(module, intervention, code)]
error = unspecified[nchar(code)>2]

other_codes = c('H1_8', 'H2_11', 'H3_11', 'H4_12', 'H5_11', 'H6_10', 'H7_5', 'H8_11', 'H9_5', 'H10_8', 'H11_8', 'H12_8', 'H13_3', 
                'T1_10', 'T2_8', 'T3_10', 'T4_3', 
                'M1_7', 'M2_11', 'M3_7', 'M4_3', 
                'R1_5', 'R2_7', 'R3_3', 'R4_6', 'R5_3', 'R6_2', 'R7_5', 'R8_3')

error2 = unspecified[code%in%other_codes] #These were reviewed by hand by EKL 5/1/19


#--------------------------------------------------------------------------------
# Merge mapped codes to final mappings 
#--------------------------------------------------------------------------------
    module_map = module_map[, .(code, module, intervention, coefficient, disease)] #Ok to here. 
    all_interventions = unique(all_interventions[, .(gf_module, gf_intervention, code)]) #Drop out French, Spanish, and the abbreviations for now. 
    module_map = merge(module_map, all_interventions, by='code', all.x = TRUE)
    
    stopifnot(nrow(module_map[is.na(gf_module)])==0) 
#--------------------------------------------------------------------------------
#Write final mapp and .diff files for comparison
#--------------------------------------------------------------------------------
    
    #Final sanity check before saving! 
    nrow(module_map)
    module_map = unique(module_map)
    nrow(module_map)
    duplicates = module_map[duplicated(module_map, by=c('module', 'intervention', 'disease', 'code'))]
    duplicates = duplicates[, .(module, intervention, disease, coefficient, code)]
    duplicates = merge(duplicates, module_map[, .(module, intervention, disease, coefficient, code)], by=c(keyVars, 'coefficient', 'code'))
    duplicates[, coeff_sum:=sum(coefficient), by=keyVars]
    duplicates = duplicates[round(coeff_sum, 3)!=1.000]
    duplicates = duplicates[order(module, intervention, disease)]
    if (nrow(duplicates)!=0){
      print(duplicates)
      stop("There are duplicates in key merge variables - review map before saving.")
    }
    
  write.csv(module_map, paste0(mapping_dir, "gf_mapping.csv"), row.names = FALSE)
  saveRDS(module_map, paste0(mapping_dir, "gf_mapping.rds"))

  #Write a "diff" file to repository to make comparing changes easier. 
  module_map = module_map[, .(code, module, intervention, coefficient, disease, gf_module, gf_intervention)]
  # removed_rows = anti_join(original_map, module_map)
  # write.csv(removed_rows, paste0(code_dir, "proposed_deletions_mod_map.csv"))
  # 
  # added_rows = anti_join(module_map, original_map)
  # write.csv(added_rows, paste0(code_dir, "proposed_additions_mod_map.csv"))
  # 

  print("Step A: Verify module mapping completed.")
# prepare cleaned list of interventions in the new 2020-2022 modular framework
# # 
# # # set up
# rm(list=ls())
# 
# # ----------------------------------------------
# # Initial set up
# # ----------------------------------------------
# user=as.character(Sys.info()[7])
# if (Sys.info()[1]=='Windows'){
#   setwd(paste0("C:/Users/",user,"/Documents/gf/")) #Change to the root of your repository
# } else {
#   setwd(paste0("/ihme/homes/",user,"/gf/"))
# }
# source("./resource_tracking/prep/_common/set_up_r.R", encoding="UTF-8")
# source("./resource_tracking/prep/_common/load_master_list.r", encoding="UTF-8")
# source("./resource_tracking/prep/gf_files_prep_functions/prep_fr_budgets.R", encoding="UTF-8")
# 
# #Source document prep functions
# doc_prep_functions = list.files(paste0(code_dir, "gf_files_prep_functions"), full.names=TRUE)
# for (file in doc_prep_functions){
#   source(file, encoding="UTF-8")
# }
# 
# include_stops <- TRUE

# Merge the 2020-2022 module map onto the module/intervention pairs pulled from raw data. 
# This code never needs to be rerun (unless the framework changes!), just leaving for documentation. Emily Linebarger 2/12/2020 
map_20_22_hiv = data.table(read_xlsx(paste0(mapping_dir, "2020-2022 Modular Framework.xlsx"), sheet="HIV Interventions"))
map_20_22_hiv[, disease:="hiv"]
map_20_22_tb = data.table(read_xlsx(paste0(mapping_dir, "2020-2022 Modular Framework.xlsx"), sheet="TB Interventions"))
map_20_22_tb[, disease:="tb"]
map_20_22_malaria = data.table(read_xlsx(paste0(mapping_dir, "2020-2022 Modular Framework.xlsx"), sheet="Malaria Interventions"))
map_20_22_malaria[, disease:="malaria"]
map_20_22_rssh = data.table(read_xlsx(paste0(mapping_dir, "2020-2022 Modular Framework.xlsx"), sheet="RSSH Interventions"))
map_20_22_rssh[, disease:="rssh"]

map_20_22 = rbindlist(list(map_20_22_hiv, map_20_22_tb, map_20_22_malaria, map_20_22_rssh), fill = TRUE)
saveRDS(map_20_22, paste0(mapping_dir, "2020_2022_MF.rds"))
 
map_20_22 = readRDS(paste0(mapping_dir, "2020_2022_MF.rds"))

# Take the full list of interventions, strip the diacritics and spaces, and add these onto the map as valid raw options. 
all_eng = map_20_22[, .(code, gf_module, gf_intervention, disease, population)]
setnames(all_eng, old=c('gf_module', 'gf_intervention'), new=c('module', 'intervention'))
all_fr = map_20_22[, .(code, gf_module_fr, gf_intervention_fr, disease, population_fr)]
setnames(all_fr, old=c('gf_module_fr', 'gf_intervention_fr', 'population_fr'), new=c('module', 'intervention', 'population'))
all_esp = map_20_22[, .(code, gf_module_esp, gf_intervention_esp, disease, population_esp)]
setnames(all_esp, old=c('gf_module_esp', 'gf_intervention_esp', 'population_esp'), new=c('module', 'intervention', 'population'))

all_langs = rbindlist(list(all_eng, all_fr, all_esp))
all_langs[, coefficient:=1]
all_langs = strip_chars(all_langs)
all_langs = all_langs[, -c('orig_module', 'orig_intervention')]
module_map <- copy(all_langs)
module_map = unique(module_map) # This is your new 'Master' list for 2020-2022 raw extracted module/intervention pairs. 



#-------------------------------
#      SUBSET Modular Framework
#-------------------------------
# Module Map does not currently work because there are duplicates in the key population variables
module_map_nopop <- module_map[!disease%in%c('hiv', 'hiv/tb')]

module_map_hiv <- module_map[disease%in%c('hiv', 'hiv/tb')]
# keep only the unspecified population indicators since that is what we are going to use for the mapping for now
module_hiv_1 <- module_map_hiv[code%in%c('H3_25_NA', 'H3_26_NA', 'H3_27_NA')]
module_hiv_2 <- module_map_hiv[code%in%c('H4_9_NA', 'H4_28_NA', 'H4_29_NA', 'H4_30_NA', 'H4_31_NA', 'H4_32_NA')]
module_hiv_3 <- module_map_hiv[code%in%c('H1_1_NA', 'H1_9_NA', 'H1_2_NA', 'H1_3_NA', 'H1_4_NA', 'H1_5_NA', 'H1_6_NA', 'H1_7_NA','H1_8_NA', 
                                         'H1_57_NA', 'H1_58_NA',
                                         'H1_13_7', 'H1_14_7', 'H1_15_7', 'H1_16_7', 'H1_18_8', 'H1_19_9', 'H1_20_9', 'H1_10_4', 'H1_11_4', 'H1_12_4',
                                         'R2_8', 'R2_9', 'R2_7', 'R2_6', 'R3_12', 'R4_21', 'R4_19', 'R1_2', 'R4_20', 'R8_31', 'R7_26', 'R7_27', 'R7_28',
                                         'T4_15', 'T4_16', 'T4_17', 'T4_18', 'T4_19',
                                         'H7_55_NA', 'H7_56_NA', 'H7_58_NA', 'H7_57_NA')]
module_hiv_4 <- module_map_hiv[code%in%c('H2_21_NA', 'H2_22_NA','H2_23_NA', 'H2_24_NA', 'H2_58_NA', 'H2_57_NA',
                                         'H5_34_NA', 'H5_35_NA', 'H5_36_NA', 'H5_37_NA', 'H5_38_NA', 'H5_39_NA', 'H5_40_NA', 'H5_41_NA', 'H5_42_NA',
                                         'H5_43_NA', 'H5_44_NA', 'H5_45_NA', 'H5_57_NA', 'H6_46_NA', 'H6_47_NA', 'H6_48_NA', 'H6_49_NA',  'H6_50_NA',
                                         'H6_52_NA', 'H6_53_NA', 'H6_54_NA',  'H6_58_NA', 'H6_57_NA',
                                         'T1_1', 'T1_10', 'T1_11', 'T1_2', 'T1_3', 'T1_4', 'T1_5', 'T1_6', 'T1_7', 'T1_8', 'T1_9',
                                         'T3_1', 'T3_10', 'T3_11', 'T3_12',  'T3_2', 'T3_3', 'T3_4', 'T3_6', 'T3_7', 'T3_8', 'T3_9',
                                         'T2_10', 'T2_11', 'T2_2', 'T2_20', 'T2_21', 'T2_3', 'T2_4', 'T2_5', 'T2_6', 'T2_7', 'T2_8', 'T2_9',
                                         'R1_1', 'R1_2', 'R1_4', 'R2_8',
                                         'R1_3', 'R1_5', 'R2_10', 'R2_11', 'R2_6', 'R2_7', 'R2_9', 'R3_12', 'R3_13',
                                         'R3_14',  'R3_15', 'R3_16', 'R3_17', 'R3_18', 'R4_19', 'R4_20', 'R4_21', 'R5_22',  'R5_23',
                                         'R6_24',
                                         'R6_25',
                                         'R7_26', 'R7_27', 'R7_28', 'R7_29', 'R8_30', 'R8_31', 'R8_32', 'R8_33', 'R8_34', 'R9_35', 'R9_36')]

hiv_interventions <- rbind(module_hiv_1, module_hiv_2, module_hiv_3, module_hiv_4, fill=TRUE)


module_map <- rbind(module_map_nopop, hiv_interventions, fill=TRUE)

stopifnot(nrow(module_map[code==""])==0) 

# -------------------------------
#       FORMAT DATA 
#--------------------------------
original_map <- copy(module_map) #Save an original copy for comparison later 
new_rows <- fread(paste0(mapping_dir, "gf_mapping_additions_nfm3.csv")) #Add in new rows to previously approved map
new_rows = new_rows[, .(module, intervention, code, coefficient, disease)]
module_map = rbind(module_map, new_rows, fill = TRUE)

stopifnot(nrow(module_map[is.na(module)])==0) 

# #-------------------------------------------------------------------------------
# # Expand the map for HIV/TB and RSSH 
# #------------------------------------------------------------------------------
# # If one module/intervention maps to RSSH for one disease (hiv, tb, or malaria)
# #   then it should also map to the same disease for the other two.
all_rssh = module_map[substr(code, 1, 1)=='R']
# #Remove any program management [or unspecified if we chose to add that intervention placeholder back in]
all_rssh = all_rssh[!code%in%c('R9_35', 'R9_36')]
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

# # All codes that are okay under HIV OR TB will also be okay if they match with the same
# # line item from a specific HIV/TB grant.
hiv = module_map[substr(code, 1, 1)=='H']
#Remove program management, [and unspecified if we chose to add that intervention placeholder back in]
hiv = hiv[!code%in%c('H7_55_NA', 'H7_56_NA', 'H7_57_NA', 'H7_58_NA')]
tb = module_map[substr(code, 1, 1)=='T']
#Remove program management, TB/HIV, PBF, (and Unspecified if we add that indicator in later)
tb = tb[!substr(code, 1, 2)%in%c('T2', 'T5')]
hiv[, disease:='hiv/tb']
tb[, disease:='hiv/tb']

hivtb = list(hiv, tb, module_map)
module_map = rbindlist(hivtb, use.names=TRUE)

module_map = unique(module_map)

stopifnot(nrow(module_map[code==""])==0) 

# -------------------------------
#
#   CLEAN DATA BEFORE VALIDATING 
#
#--------------------------------
#These are the variables that are merged onto the raw data, so it's important to check duplicates with these. 
keyVars = c('module', 'intervention', 'disease', 'population')

#Remove whitespace from 'code' column before doing checks below
module_map[, code:=trimws(code)]
map_20_22[, code:=trimws(code)]

stopifnot(nrow(module_map[code==""])==0) 

#Drop out French, Spanish, and abbreviated modules for now.  
#module_map = module_map[, -c('gf_intervention_fr', 'gf_module_fr', 'module_esp', 'intervention_esp', 'abbreviated_module')]

#--------------------------------------------------------------------------------
# CLEANING- Dropping modules/interventions that map to codes that don't make sense. 
#--------------------------------------------------------------------------------
#These are new changes after the way we've modified the 'general' and 'other' mappings - EKL 5/1/19
# module_map[module=="preventionprogramsforadolescentsandyouthinandoutofschool" & intervention == "unspecified", code:="H8"]
# module_map[module=="tbhiv" & intervention=="keypopulationstbhivothers" & disease=="hiv/tb", code:="H11_5"]

#--------------------------------------------------------------------------------
# CLEANING- Removing typos and close string matches from map  
#--------------------------------------------------------------------------------

# #--------------------------------------------------------------------------------
# # CLEANING- Replacing modules/interventions that are typos or unspecified. 
# #--------------------------------------------------------------------------------
# #Fixing diseases that were unspecified 
# module_map[code=="H99" & disease!='hiv' & disease!='hiv/tb', disease:='hiv']
# module_map[code=="M99", disease:='malaria']
# module_map[code=="T99", disease:="tb"]
# 
# module_map[code=="H99", abbreviated_module:="Unspecified"]
# module_map[code=="M99", abbreviated_module:="Unspecified"]
# module_map[code=="T99", abbreviated_module:="Unspecified"]  

# #--------------------------------------------------------------------------------
# # CLEANING (Checks 1 & 2)- Remove duplicates in module, intervention, and disease 
# # with coefficients of 1, then check. 
# #--------------------------------------------------------------------------------
# module_map = module_map[!(code=="H10_8" & module=="tbhiv" & intervention=="collaborativeactivitieswithotherprogramsandsectorstbhiv")]
# module_map = module_map[!(code=="H99" & module=="unspecified" & intervention=="unspecified" & coefficient<1)]
# module_map = module_map[!(code=="T99" & module=="unspecified" & intervention=="unspecified" & coefficient<1)]
# module_map = module_map[!(code=="M99" & module=="unspecified" & intervention=="unspecified" & coefficient<1)]
# module_map = module_map[!(code=="R99" & module=="unspecified" & intervention=="unspecified" & coefficient<1)]
# 
# #9/10/2019 EL 
# module_map = module_map[!(code=="T1_10" & disease=="hiv/tb" & module=="treatmentcareandsupport" & intervention=="treatmentmonitoringdrugresistancesurveillance")]

#--------------------------------------------------------------------------------
# CLEANING - remove duplicates created by running checks above.
#--------------------------------------------------------------------------------
module_map = unique(module_map) 

#--------------------------------------------------------------------------------
# 1. Make sure you don't have any coefficients across unique 
#   observations of module, intervention, and disease that sum to 1.
#--------------------------------------------------------------------------------
duplicates_check <- module_map[duplicated(module_map, by = keyVars), ]
duplicates_coeff_one <- duplicates_check[coefficient == 1]
# duplicates_coeff_one <- merge(duplicates_coeff_one, module_map, by = c(keyVars, 'coefficient', 'abbreviated_module')) #Merge back onto module_map because some duplicates don't have coefficients of 1. 
duplicates_coeff_one <- duplicates_coeff_one[order(module, intervention, disease)]

# Check
if (nrow(duplicates_coeff_one[!module%in%c('paymentforresults', 'financiacionbasadaenlosresultados', 'financementbasesurlesresultats'  )]) != 0 & include_stops == TRUE){
  print(duplicates_coeff_one) 
  stop("Module/Intervention/Disease duplicates with coefficients of 1!")
}

stopifnot(nrow(module_map[code==""])==0) 
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
map_unique = unique(module_map[,c('module','intervention','population', 'code','coefficient')])
dups = duplicated(map_unique[,c('module','intervention', 'population')])
bad_combos = map_unique[dups==TRUE & coefficient==1 & !intervention %in% c('all','na'), c('module','intervention', 'population')]
map_subset = merge(module_map, bad_combos, by=c('module','intervention', 'population'))

# grant management, PBF and TB/HIV are allowed to exist in more than one disease
allowableModules = c('gestiondeprogramas', 'gestiondeprogramme', 'gestiondessubventions', 
                     'performancebasedfinancing', 'financementbasesurlesresultats', 'paymentforresults', 'financiacionbasadaenlosresultados', 'programmanagement', 'tbhiv', 'tbhivcollaborativeactivities', 
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
                  'preventionbccmassmedia', 'preventionbcccommunityoutreach', 'programmanagement', 'gestiondelasubvention', 'gestiondeprogramme', 'gestiondeprogramas', 'financementbasesurlesresultats', 'financiacionbasadaenlosresultados', 'performancebasedfinancing', 
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

stopifnot(nrow(module_map[code==""])==0)  
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
# hiv_codes <- c('H9', 'H9_1', 'H9_2', 'H9_3', 'H98', 'H99')
# tb_codes <- c('T4', 'T4_1', 'T4_2', 'T4_3', 'T98', 'T99')
# mal_codes <- c('M4', 'M4_1', 'M4_2', 'M4_3', 'M98', 'M99')
# rssh_codes <- c('R8', 'R8_1', 'R8_2', 'R8_3', 'R98', 'R99')
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
stopifnot(nrow(module_map[code==""])==0) 
#--------------------------------------------------------------------------------
# 12. Make sure that "other" and "general" are assigned correctly
#--------------------------------------------------------------------------------

#It's an error if the intervention is "unspecified", and it's in the "other" category. 
unspecified = module_map[intervention=="unspecified", .(module, intervention, code, population)]
error = unspecified[nchar(code)>2]

# other_codes = c('H1_8', 'H2_11', 'H3_11', 'H4_12', 'H5_11', 'H6_10', 'H7_5', 'H8_11', 'H9_5', 'H10_8', 'H11_8', 'H12_8', 'H13_3', 
#                 'T1_10', 'T2_8', 'T3_10', 'T4_3', 
#                 'M1_7', 'M2_11', 'M3_7', 'M4_3', 
#                 'R1_5', 'R2_7', 'R3_3', 'R4_6', 'R5_3', 'R6_2', 'R7_5', 'R8_3')

# error2 = unspecified[code%in%other_codes] #These were reviewed by hand by EKL 5/1/19

stopifnot(nrow(module_map[code==""])==0) 
#--------------------------------------------------------------------------------
# Merge mapped codes to final mappings 
#--------------------------------------------------------------------------------
module_map = module_map[, .(code, module, intervention, disease, coefficient)]
map_20_22 = unique(map_20_22)
map_20_22$disease <- NULL # Don't need this, because it actually represents the disease of the grant, not the disease of the intervention. 
module_map = merge(module_map, map_20_22, by=c('code'), all.x = TRUE)

stopifnot(nrow(module_map[code==""])==0)  
#--------------------------------------------------------------------------------
#Write final mapp and .diff files for comparison
#--------------------------------------------------------------------------------

# #Final sanity check before saving! 
# nrow(module_map)
# module_map = unique(module_map)
# nrow(module_map)
# duplicates = module_map[duplicated(module_map, by=c('module', 'intervention', 'disease','population', 'code'))]
# duplicates = duplicates[, .(module, intervention, disease, coefficient, population, code)]
# duplicates = merge(duplicates, module_map[, .(module, intervention, disease, coefficient, population, code)], by=c(keyVars,  'coefficient', 'code'))
# duplicates[, coeff_sum:=sum(coefficient), by=keyVars]
# duplicates = duplicates[round(coeff_sum, 3)!=1.000]
# duplicates = duplicates[order(module, intervention, disease, population)]
# if (nrow(duplicates)!=0){
#   print(duplicates)
#   stop("There are duplicates in key merge variables when searching by module, intervention, disease, population, code - review map before saving.")
# }

duplicates = module_map[duplicated(module_map, by=c('module', 'intervention', 'disease'))]
duplicates = duplicates[, .(module, intervention, disease, coefficient)]
duplicates = merge(duplicates, module_map[, .(module, intervention, disease, coefficient)], by=c('module', 'intervention', 'disease',  'coefficient'))
duplicates[, coeff_sum:=sum(coefficient), by=c('module', 'intervention', 'disease')]
duplicates = duplicates[round(coeff_sum, 3)!=1.000]
duplicates = duplicates[order(module, intervention, disease)]
if (nrow(duplicates[!module%in%c('financiacionbasadaenlosresultados', 'financementbasesurlesresultats', 'paymentforresults')])!=0){
  print(duplicates)
  stop("There are duplicates in key merge variables when checking by module, intervention, disease- review map before saving.")
}

write.csv(module_map, paste0(mapping_dir, "gf_mapping_nfm3.csv"), row.names = FALSE)
saveRDS(module_map, paste0(mapping_dir, "gf_mapping_nfm3.rds"))

#Write a "diff" file to repository to make comparing changes easier. 
#module_map = module_map[, .(code, module, intervention, coefficient, disease, gf_module, gf_intervention)]
# removed_rows = anti_join(original_map, module_map)
# write.csv(removed_rows, paste0(code_dir, "proposed_deletions_mod_map.csv"))
# 
# added_rows = anti_join(module_map, original_map)
# write.csv(added_rows, paste0(code_dir, "proposed_additions_mod_map.csv"))
# 

print("NFM3 Mapping completed.")
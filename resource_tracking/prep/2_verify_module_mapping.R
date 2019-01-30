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

prep_map <- function(map){

  original_map <- copy(map) #Save an original copy for comparison later 

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
map = map[!(module == "treatmentcareandsupport" & intervention == "na" & disease == "hss" & (code == "R1_2" | code == "R1_3"))]
map = map[!(module == "tbcareandprevention" & disease == "hiv")] #2 cases of this, for interventions 'all' and 'casedetectionanddiagnosis.' There are also TB entries for these sections. 
map = map[!(module == 'removinglegalbarrierstoaccess' & intervention == 'policyadvocacyonlegalrights' & code == 'R7_2')]
map = map[!(module == 'beyondtb' & intervention == 'all' & disease == 'malaria' & code == "R2")]
#Remove all 'treatment' modules that have an RSSH code 
map = map[!(module %in% c('tb_treatment', 'treatmentdiagnosis', 'treatmentmanagementofmalariainschools', 'treatmentprompteffectiveantimalarialtreatment',
                        'treatmentprompteffectiveantimalarialtreatmentprivatesector') & code %in% c('R4', 'R7_4', 'R1'))]
map = map[!(module=='priseenchargeetpreventiondelatuberculose' & intervention == 'all' & disease == 'hiv')]
map = map[!(module=='tuberculosemultiresistante' & intervention == 'all' & disease == 'hiv')]
map = map[!(module=='hivprevention' & code == 'R7_5' & disease %in% c('malaria', 'tb', 'hss'))]
map = map[!(module=='malprevention' & code == 'R7_3' & disease %in% c('hiv', 'tb', 'hss'))]
map = map[!(module == 'supportiveenvironment' & intervention == 'na' & code == 'H6_8')]
map = map[!(module == 'mdrtb' & substring(code, 0, 1)=='H')]
map = map[!(module == 'hivhealthsystemsstrengthening' & disease%in%c('tb', 'malaria', 'hss'))]
map = map[!(module == 'tbhealthsystemsstrengthening' & disease%in%c('hiv', 'malaria', 'hss'))]
map = map[!(module == 'malhealthsystemsstrengthening' & disease%in%c('tb', 'hiv', 'hss'))]


#--------------------------------------------------------------------------------
# CLEANING- Removing typos and close string matches from map  
#--------------------------------------------------------------------------------
map = map[module == "healthsystemstrengthening", module:='healthsystemsstrengthening']

#--------------------------------------------------------------------------------
# CLEANING- Replacing modules/interventions that are typos or unspecified. 
#--------------------------------------------------------------------------------
#Split modules/interventions - see shared mapping functions for function documentation. 
map = split_mods_interventions(map, "preventionbehavioralchangecommunicationcommunityoutreach", "prevention")
map = split_mods_interventions(map, "preventionbloodsafetyanduniversalprecautions", "prevention")
#map = split_mods_interventions(map, "preventionbehavioralchangecommunicationmassmedia", "prevention")

#Unclear classifications; could be clarified.
map[module=='hivprevention' & intervention=='preventionbcccommunityoutreach' & disease == 'hiv', code:='H1_1']
map[module=='malprevention' & intervention=='preventionbcccommunityoutreach' & disease == 'malaria', code:='M3_5']

#Correcting RSSH modules after it was decided to leave 'rssh' at beginning of string
map = map[module == 'healthmanagementinformationsystemsandme', module:='rsshhealthmanagementinformationsystemsandme']
map = map[module == 'communityresponsesandsystems', module:= 'rsshcommunityresponsesandsystems']
map = map[module == 'nationalhealthstrategies', module:= 'rsshnationalhealthstrategies']
map = map[module == 'integratedservicedeliveryandqualityimprovement', module:= 'rsshintegratedservicedeliveryandqualityimprovement']
map = map[module == 'procurementandsupplychainmanagementsystems', module:= 'rsshprocurementandsupplychainmanagementsystems']
map = map[module == 'humanresourcesforhealthincludingcommunityhealthworkers', module:= 'rsshhumanresourcesforhealthincludingcommunityhealthworkers']
map = map[module == 'financialmanagementsystems', module:= 'rsshfinancialmanagementsystems']

#--------------------------------------------------------------------------------
# CLEANING (Checks 1 & 2)- Remove duplicates in module, intervention, and disease 
# with coefficients of 1, then check. 
#--------------------------------------------------------------------------------
map = map[(module == "healthsystemsstrengthening" & intervention == "servicedelivery"), code:='R4']
map = map[(module == "malhealthsystemsstrengthening" & intervention == "informationsystem"), code:= 'R2']
map = map[(module == "malsupportiveenvironment" & intervention == "leadershipandgovernance"), code:= 'R6_1']
map = map[!(module == "malsupportiveenvironment" & intervention == "leadershipandgovernance" & coefficient == 0.5)] #This should always map to code R6_1. 
map = map[(module == "performancebasedfinancing" & intervention == "performancebasedfinancing"), code:= 'R98']
map = map[(module == "procurementandsupplychainmanagementsystems" & intervention == "nationalproductselectionregistrationandqualitymonitoring"), code:="R1_4"]
map = map[(module == "programmanagement" & intervention == "programmanagement" & disease == "hiv"), code:="H9"]
map = map[(module == "programmanagement" & intervention == "programmanagement" & disease == "tb"), code:="T4"]
map = map[(module == "tbhealthsystemsstrengthening" & intervention == "supportiveenvironmentprogrammanagementandadministration"), code:="R8_1"]
map = map[(module == "tbhivcollaborativeactivities" & intervention == "tbhiv"), code:="T2_1"]
map = map[(module == "treatmentcareandsupport" & intervention == "na" & disease == "hiv"), code:="H6"]
map = map[(module == "treatmentcareandsupport" & intervention == "na" & disease == "malaria"), code:="M2"]
map = map[(module == "treatmentcareandsupport" & intervention == "na" & disease == "tb"), code:="T1"]
map = map[(module == "tbtreatment" & intervention == "mdrtb"), code:="T3_2"]
map = map[!(module == 'rsshprocurementandsupplychainmanagementsystems' & intervention == 'nationalproductselectionregistrationandqualitymonitoring'
            & code == 'R1_3')]
map = map[!(module == 'healthsystemsstrengthening' & intervention == 'humanresources' & (code == "M1_6" | code == "M3_5"))]
map = map[!(module == 'healthsystemsstrengthening' & intervention == 'humanresources' & code == "R3_1")]

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
map[module == 'rsshcommunityresponsesandsystems' & intervention == 'communitybasedmonitoring' & code == "H9_2", code:='R7_1']
map = map[!(module=='mdrtb' & intervention == 'casedetectionanddiagnosismdrtb' & code == 'H7_8')]
map = map[!(module == 'suivietevaluation' & intervention == 'traitementtuberculosemultiresistante' & code == 'H7_8')]
map = map[!(module=='suivietevaluation' & intervention == 'depistageetdiagnosticdesmaladiestuberculosemultiresistante' & code == 'H6_6')]

#--------------------------------------------------------------------------------
# CLEANING (Check 6) Remove duplicate mappings in the same language
#--------------------------------------------------------------------------------

#--------------------------------------------------------------------------------
# CLEANING (Check 7)- Fix cases where module/intervention is mapping
#   to incorrect code by disease. 
#--------------------------------------------------------------------------------
map = map[!(module == 'intervencionesdeprevencionespecificas' & intervention == 'iecccc' & disease == 'hiv' & code == 'M2_3')]

#--------------------------------------------------------------------------------
# CLEANING (Check 8)- Remove RSSH mappings that don't make sense. 
#--------------------------------------------------------------------------------
map[, prefix:=substring(code, 0, 1)]
map = map[!(module == 'otherspecify' & intervention == 'all' & prefix == 'R')]
map = map[!(module == 'other' & intervention == 'all' & prefix == 'R')]
map = map[!(module == 'otherunidentified' & intervention == 'otherunidentified' & prefix == 'R')]
map = map[!(module == 'beyondtb' & intervention == 'all' & prefix=='R')]

#Correcting some RSSH mappings to disease codes
map[module == 'malprevention' & intervention == 'bcccommunityoutreach', code:='M3_5']
#map[module == 'tbtreatment' & 'expandandconsolidatehighqualitydotsservices', code:='T1'] #David 
map[, prefix:=NULL]

#--------------------------------------------------------------------------------
# CLEANING - remove duplicates created by running checks above.
#--------------------------------------------------------------------------------
map = unique(map)

#-------------------------------------------------------------------------------
#
# RUN VALIDATION CHECKS ON MAP, AND CORRECT MAP IN CORRESPONDING SECTIONS ABOVE 
#
#-------------------------------------------------------------------------------

#--------------------------------------------------------------------------------
# 1. Make sure you don't have any coefficients across unique 
#   observations of module, intervention, and disease that sum to 1.
#--------------------------------------------------------------------------------
duplicates_check <- map[duplicated(map, by = keyVars) == TRUE, ]
duplicates_coeff_one <- duplicates_check[coefficient == 1]
duplicates_coeff_one <- merge(duplicates_coeff_one, map, by = keyVars) #Merge back onto map because some duplicates don't have coefficients of 1. 
duplicates_coeff_one <- duplicates_coeff_one[order(module, intervention)]

# Check
if (nrow(duplicates_coeff_one) != 0 & include_stops == TRUE){
  print(duplicates_coeff_one[, c("module", "intervention", "disease", "code.y", "coefficient.y")]) 
  stop("Module/Intervention/Disease duplicates with coefficients of 1!")
}

#--------------------------------------------------------------------------------
# 2. Make sure you don't have any coefficients across unique 
#   observations of module, intervention, and disease that sum to 
#   greater than or less than 1 (budget shrinking or growing) 
#--------------------------------------------------------------------------------
redistribution_map <- map[coefficient!=1]
redistribution_map[, coeff_sum:=round(sum(coefficient), 1), by=keyVars]

redistribution_error = redistribution_map[coeff_sum != 1.0]
#--------------------------------------------------------------------------------
# 3. Remove all cases where "na" or "all" was mapping to a specific code. ***EMILY KEEP WORKING HERE- CHECK IF CODE IS GOING TO 'UNSPECIFIED'. 
#--------------------------------------------------------------------------------

unspecified_mods <- c('all', 'na', 'other', 'unspecified', 'otro', 'otherspecify')
check_unspecified<-map[module %in% unspecified_mods | intervention %in% unspecified_mods]
check_unspecified <- check_unspecified[nchar(code)>2] #It's okay to have some general codes with unspecified mods; but should also review these. 
check_unspecified <- unique(check_unspecified)

if(nrow(check_unspecified)>0 & include_stops == TRUE){
    print(unique(check_unspecified[, c("module", "intervention", "code"), with = FALSE]))
    stop(paste0(print(nrow(check_unspecified)), " modules have na or all as values."))
}

#--------------------------------------------------------------------------------
# 4. Check for close spellings of the same module and intervention  ***EMILY KEEP WORKING HERE 
#--------------------------------------------------------------------------------
#print(sort(unique(map$module)))
#print(sort(unique(map$intervention)))
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
                     'tbvih', 'tuberculosevih', 'tuberculosisvih', 'hivtbcollaborativeactivities')
map_subset = map_subset[!module %in% allowableModules]

#RSSH modules are allowed to exist in more than one disease 
map_subset[, prefix:=substr(code, 0, 1)]
map_subset = map_subset[!(prefix=='R')]

map_subset = map_subset[order(module, intervention)]

if(nrow(map_subset)>0 & include_stops == TRUE){
  print(unique(map_subset[, c("module", "intervention", "code", "disease"), with = FALSE]))
  stop(paste0(print(nrow(map_subset)), " duplicates in module/intervention have different mapping codes"))
}

#--------------------------------------------------------------------------------
# 6. You should only have one phrase in each language mapping to an intervention code.
#     For budget line items that don't match exactly, we should change them line by line 
#     in an R-script so they are documented. ***EMILY KEEP WORKING HERE
#--------------------------------------------------------------------------------

check_multi_mappings <- map[, frequency:=1]
check_multi_mappings <- check_multi_mappings[, code_count:=sum(frequency), by = code]
check_multi_mappings <- check_multi_mappings[code_count > 3] #It's okay to have up to 3 languages in the dataset. Should check to verify these are in all 3 languages sometime. 
check_multi_mappings <- check_multi_mappings[order(-code_count)]

#--------------------------------------------------------------------------------
# 7. Make sure we don't have any codes mapped where their prefix 
#     doesn't match their disease. ***EMILY KEEP WORKING HERE
#--------------------------------------------------------------------------------
check_prefix = map[, prefix:=substr(code, 0, 1)]
check_prefix$error = ifelse((check_prefix$disease == "hiv" & check_prefix$prefix != "H") |
                              (check_prefix$disease == "tb" & check_prefix$prefix != "T") |
                              (check_prefix$disease == "malaria" & check_prefix$prefix != 'M') | 
                              (check_prefix$disease == "hiv/tb" & (check_prefix$prefix != 'H' & check_prefix$prefix != 'T')) |
                              (check_prefix$disease == "hss" & check_prefix$prefix != "R"), TRUE, FALSE)
check_prefix = check_prefix[error == TRUE] #Emily should check with David how we want to resolve all of these, including RSSH, but for now exclude 'R' from check. 

check_prefix_ltd <- check_prefix[prefix != 'R']

if(nrow(check_prefix_ltd)>0 & include_stops == TRUE){
  print(unique(check_prefix_ltd[, c("module", "intervention", 'code', 'disease'), with = FALSE]))
  stop(paste0(print(nrow(check_prefix_ltd)), " errors in applying code for given disease")) #Check with David here. 
}


#--------------------------------------------------------------------------------
# 8. Add a check to verify all RSSH modules/interventions look like they belong there.
#     ***EMILY KEEP WORKING HERE. NEED TO ONLY FLAG PROGRAM MANAGEMENT AND PERFORMANCE-BASED FINANCING.
#--------------------------------------------------------------------------------

check_rssh <- map[substr(code, 0, 1) == 'R']
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

if(nrow(check_rssh)>0 & include_stops == TRUE){
  print(unique(check_rssh[!(module %in% problem_mods), .(module)]))
}

#print(unique(check_rssh[module %in% problem_mods, .(module, intervention, code)])) #Need to check with David on these. 
map[, prefix:=NULL]
#--------------------------------------------------------------------------------
# 9. Add a check to verify that if observations have the same module, intervention, 
#    and code, they should have the same coefficient. 
#     ***EMILY KEEP WORKING HERE. 
#--------------------------------------------------------------------------------
  write.csv(map, "J:/Project/Evaluation/GF/mapping/multi_country/intervention_categories/gf_mapping.csv")

  #Write a "diff" file to repository to make comparing changes easier. 
  removed_rows = anti_join(original_map, map)
  write.csv(removed_rows, paste0(code_loc, "resource_tracking/proposed_deletions_mod_map.csv"))
  
  added_rows = anti_join(map, original_map)
  write.csv(added_rows, paste0(code_loc, "resource_tracking/proposed_additions_mod_map.csv"))
  
  return(map)
}

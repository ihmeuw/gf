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

# -------------------------------
#
#   VERIFICATION STEPS 
#
#--------------------------------
  #These are the variables that are merged onto the raw data, so it's important to check duplicates with these. 
  keyVars = c('module', 'intervention', 'disease')
  
#--------------------------------------------------------------------------------
# CLEANING- Dropping modules/interventions that map to codes that don't make sense. 
#--------------------------------------------------------------------------------
map = map[!(module == "treatmentcareandsupport" & intervention == "na" & disease == "hss" & (code == "R1_2" | code == "R1_3"))]

#--------------------------------------------------------------------------------
# CLEANING- Replacing modules/interventions that are typos or unspecified. 
#--------------------------------------------------------------------------------
#Split modules/interventions - see shared mapping functions for function documentation. 
map = split_mods_interventions(map, "preventionbehavioralchangecommunicationcommunityoutreach", "prevention")
map = split_mods_interventions(map, "preventionbloodsafetyanduniversalprecautions", "prevention")
#map = split_mods_interventions(map, "preventionbehavioralchangecommunicationmassmedia", "prevention")

#Unclear classifications; could be clarified. 

#Correcting RSSH modules after it was decided to leave 'rssh' at beginning of string
map = map[module == 'healthmanagementinformationsystemsandme', module:='rsshhealthmanagementinformationsystemsandme']
map = map[module == 'communityresponsesandsystems', module:= 'rsshcommunityresponsesandsystems']
map = map[module == 'nationalhealthstrategies', module:= 'rsshnationalhealthstrategies']
map = map[module == 'integratedservicedeliveryandqualityimprovement', module:= 'rsshintegratedservicedeliveryandqualityimprovement']
map = map[module == 'procurementandsupplychainmanagementsystems', module:= 'rsshprocurementandsupplychainmanagementsystems']
map = map[module == 'humanresourcesforhealthhrhincludingcommunityhealthworkers', module:= 'rsshhumanresourcesforhealthhrhincludingcommunityhealthworkers']
map = map[module == 'financialmanagementsystems', module:= 'rsshfinancialmanagementsystems']


#--------------------------------------------------------------------------------
# CLEANING- Remove duplicates in module, intervention, and disease 
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

#Remove the duplicates that you created in cleaning through aligning codes. 
map <- unique(map)

#--------------------------------------------------------------------------------
#2. Make sure you don't have any coefficients across unique 
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
#2. Make sure you don't have any coefficients across unique 
#   observations of module, intervention, and disease that sum to 
#   greater than or less than 1 (budget shrinking or growing) ***EMILY KEEP WORKING HERE
#--------------------------------------------------------------------------------

#--------------------------------------------------------------------------------
#3. Remove all cases where "na" or "all" was mapping to a specific code. ***EMILY KEEP WORKING HERE- CHECK IF CODE IS GOING TO 'UNSPECIFIED'. 
#--------------------------------------------------------------------------------

unspecified_mods <- c('all', 'na', 'other', 'unspecified', 'otro', 'otherspecify')
check_unspecified<-map[module %in% unspecified_mods | intervention %in% unspecified_mods]
check_unspecified <- check_unspecified[nchar(code)>2] #It's okay to have some general codes with unspecified mods; but should also review these. 

if(nrow(check_unspecified)>0 & include_stops == TRUE){
    print(unique(check_unspecified[, c("module", "intervention", "code"), with = FALSE]))
    stop(paste0(print(nrow(check_unspecified)), " modules have na or all as values."))
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

if(nrow(map_subset)>0 & include_stops == TRUE){
  print(unique(map_subset[, c("module", "intervention"), with = FALSE]))
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
# CLEANING- Fix cases where module/intervention is mapping to incorrect code by disease. 
#--------------------------------------------------------------------------------

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
#     ***EMILY KEEP WORKING HERE. 
#--------------------------------------------------------------------------------

check_rssh <- map[substr(code, 0, 1) == 'R']

if(nrow(check_rssh)>0 & include_stops == TRUE){
  print(unique(check_rssh[, .(module)]))
}

  return(map)
}

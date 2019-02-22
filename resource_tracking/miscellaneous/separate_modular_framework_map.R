#-----------------------------------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Find all of the rows in the map that are mapping to files after 2017. 
# (modular framework only). Save these in their own file, and remove all RSSH (?)
# Also labels which files apply to the modular framework in each file list. 
# DATE: February 2019
#-----------------------------------------------------------------------

#---------------------------------------------------------------
# To-do list for this code: 
# - Label in file list which file is pre-mf and post-mf
# - Incorporate final GF module and abbrev module in 2017 map. Don't want to have to map twice. 
#---------------------------------------------------------------

rm(list=ls())
library(data.table)

user = "elineb"
j = ifelse(Sys.info()[1]=='Windows','J:','/home/j/')
code_loc = ifelse(Sys.info()[1]=='Windows', 'H:/gf/', paste0('/homes/', user, '/gf/'))

source(paste0(code_loc, "resource_tracking/prep/shared_prep_functions.R"), encoding = "UTF-8")

module_map <- fread(paste0(j, "Project/Evaluation/GF/mapping/multi_country/intervention_categories/gf_mapping.csv"))
module_map = module_map[, -c('V1', 'code_count')]

cleaned_interventions = fread(paste0(j, "Project/Evaluation/GF/mapping/multi_country/intervention_categories/all_interventions.csv"))
#-----------------------------------------------------------------------
#Read in file lists, and keep only the files that are from 2017 or later  `
#-----------------------------------------------------------------------

cod_filelist <- fread(paste0(j, "Project/Evaluation/GF/resource_tracking/cod/grants/cod_budget_filelist.csv"))
cod_filelist[, loc_name:='cod']
post_mf_cod = cod_filelist[grant_period == "2018-2020" & file_name != '10Jul12_Final Budget SSF_ ZAR-H-CORDAID.xlsm' & file_name != 'initial_gf_budgets_2018_2020.csv']
pre_mf_cod = cod_filelist[!(file_name%in%post_mf_cod$file_name)]
cod_filelist[, mod_framework_format:=FALSE]
cod_filelist[file_name%in%post_mf_cod$file_name, mod_framework_format:=TRUE]
write.csv(cod_filelist, paste0(j, "Project/Evaluation/GF/resource_tracking/cod/grants/cod_budget_filelist.csv"), row.names = FALSE)

gtm_filelist <- fread(paste0(j, "Project/Evaluation/GF/resource_tracking/gtm/grants/gtm_budget_filelist.csv"))
#Is this all? How about 2016-2019? 
gtm_filelist[, loc_name:='gtm']
pre_mf_gtm <- gtm_filelist[grant_period != "2019-2022" & grant_period != "2018-2020" & grant_period != "2019-2021" & grant_period != "2018"]
post_mf_gtm <- gtm_filelist[!(file_name%in%pre_mf_gtm$file_name)]
gtm_filelist[, mod_framework_format:=FALSE]
gtm_filelist[file_name%in%post_mf_gtm$file_name, mod_framework_format:=TRUE]
write.csv(gtm_filelist, paste0(j, "Project/Evaluation/GF/resource_tracking/gtm/grants/gtm_budget_filelist.csv"), row.names = FALSE)

uga_filelist <- fread(paste0(j, "Project/Evaluation/GF/resource_tracking/uga/grants/uga_budget_filelist.csv"))
uga_filelist[, loc_name:='gtm']
pre_mf_uga = uga_filelist[grant_period != "2018-2020"]
post_mf_uga = uga_filelist[!(file_name%in%pre_mf_uga$file_name)]
uga_filelist[, mod_framework_format:=FALSE]
uga_filelist[file_name%in%post_mf_uga$file_name, mod_framework_format:=TRUE]
write.csv(uga_filelist, paste0(j, "Project/Evaluation/GF/resource_tracking/uga/grants/uga_budget_filelist.csv"), row.names = FALSE)

pre_mf_files <- list(pre_mf_cod, pre_mf_gtm, pre_mf_uga) 
pre_mf_files <- rbindlist(pre_mf_files, use.names = TRUE, fill = TRUE) #Output this for documentation. 

post_mf_files <- list(post_mf_cod, post_mf_gtm, post_mf_uga)
post_mf_files <- rbindlist(post_mf_files, use.names = TRUE, fill = TRUE)

#-----------------------------------------------------------------------
#Read in raw prepped data and only keep the files that apply
#-----------------------------------------------------------------------

cod_data <- readRDS(paste0(j, "Project/Evaluation/GF/resource_tracking/cod/prepped/raw_bound_gf_files.RDS"))
cod_data[, loc_name:='cod']
cod_data[is.na(lang), lang:='eng'] #Reviewed these visually and they're all English. 
pre_mf_cod_data = cod_data[fileName%in%pre_mf_cod$file_name]
post_mf_cod_data = cod_data[fileName%in%post_mf_cod$file_name]

gtm_data <- readRDS(paste0(j, "Project/Evaluation/GF/resource_tracking/gtm/prepped/raw_bound_gf_files.RDS"))
gtm_data[, loc_name:='gtm']
pre_mf_gtm_data = gtm_data[fileName%in%pre_mf_gtm$file_name]
post_mf_gtm_data = gtm_data[fileName%in%post_mf_gtm$file_name]

uga_data <- readRDS(paste0(j, "Project/Evaluation/GF/resource_tracking/uga/prepped/raw_bound_gf_files.RDS"))
uga_data[, loc_name:='uga']
pre_mf_uga_data = uga_data[fileName%in%pre_mf_uga$file_name]
post_mf_uga_data = uga_data[fileName%in%post_mf_uga$file_name]

data = list(post_mf_cod_data, post_mf_gtm_data, post_mf_uga_data)
mod_framework_data <- rbindlist(data, use.names = TRUE, fill = TRUE) 
mod_framework_data <- strip_chars(mod_framework_data)

#-----------------------------------------------------------------------
#Correct common acronyms in the resource database and the module map. 
#-----------------------------------------------------------------------

mod_framework_data[, module:=replace_acronyms(module)]
mod_framework_data[, intervention:=replace_acronyms(intervention)]

module_map[, module:=replace_acronyms(module)]
module_map[, intervention:=replace_acronyms(intervention)]
mod_framework_data = mod_framework_data[, .(module, intervention, disease, lang, loc_name, fileName, sda_activity)] 
mod_framework_data = unique(mod_framework_data)
mod_framework_data[disease == "tb/hiv", disease:='hiv/tb']

#You should keep all of the rows in the module map that match these interventions and modules. Make sure all are included. 
post_2017_map <- merge(mod_framework_data, module_map, by = c('module', 'intervention', 'disease'), all.x = TRUE) 

#Drop rows that have NA for module and intervention 
post_2017_map = post_2017_map[!(is.na(module) & is.na(intervention))]

post_2017_map = unique(post_2017_map)

#-----------------------------------------------------------------------
#Validate this data, and correct codes. 
#-----------------------------------------------------------------------

check_nas = post_2017_map[is.na(code)]
#These observations need to be filled in by hand! 

check_all = post_2017_map[module == 'all' | intervention == 'all' | module == 'unspecified' | intervention == 'unspecified'] #Review these files to make sure they're actually in modular framework format. 

#Validate all of the rows mapping to RSSH. 
check_rssh = post_2017_map[substring(code, 1, 1)=='R']

#-----------------------------------------------------------------------
# Correct nas that are TB/HIV
#-----------------------------------------------------------------------

tbhiv = check_nas[disease == 'hiv/tb'] #These should all have the same codes as either HIV or TB. Change diseases based on module here, re-merge, and then change disease back. 

#All prevention programs can be categorized under HIV. 
tbhiv[grep("preventionprograms", module), disease:='hiv'] 
tbhiv[grep("programmesdeprevention", module), disease:='hiv'] 
unique(tbhiv[disease == 'hiv', .(module, intervention)]) #Make sure these all look right. 

#All TB/HIV combined modules are categorized under HIV. 
tbhiv[module == 'tbhiv' | module == 'tuberculosevih', disease:='hiv']

#Anything with RSSH should be categorized as rssh. 
tbhiv[grep('rssh', module), disease:='rssh']
tbhiv[grep('systcmesdesanteresiliantsetperennes', module), disease:='rssh']

#Categorize other pairs by module. 
tb_mods = c('mdrtb', 'priseenchargeetpreventiondelatuberculose', 'tuberculosemultiresistante', 'tbcareandprevention')
hiv_mods = c('pmtct', 'preventiondelatransmissiondelamcreylenfantptme', 'programmanagement', 'programstoreducehumanrightsrelatedbarrierstohivservices', 'programmesvisantyreduirelesobstaclesliesauxdroitshumainsquientraventlacccsauxservicesvih' 
             , 'gestiondessubventions', 'traitementpriseenchargeetsoutien', 'treatmentcareandsupport')

tbhiv[module %in% tb_mods, disease:='tb']
tbhiv[module %in% hiv_mods, disease:='hiv'] 

unique(tbhiv[disease == 'hiv/tb', .(module, intervention)])
 
tbhiv = tbhiv[, .(module, intervention, disease, lang, loc_name, fileName, sda_activity)]
tbhiv1 = merge(tbhiv, module_map, by = c('module', 'intervention', 'disease'), all.x = TRUE)
tbhiv1[, disease:='hiv/tb'] 

#Drop all rows with NA for code and disease TB/HIV, and replace with new corrected dataset. 
post_2017_map = post_2017_map[!(is.na(code) & disease == 'hiv/tb')] #Should be 240 rows 
post_2017_map = rbind(post_2017_map, tbhiv1)

#Check NAs again 
check_nas = post_2017_map[is.na(code)]
check_nas = check_nas[, .(module, intervention, disease, code, coefficient, loc_name)]
check_nas = unique(check_nas)

write.csv(check_nas, paste0(j, "Project/Evaluation/GF/mapping/multi_country/intervention_categories/raw_2017_data_to_edit.csv"), row.names = FALSE)
#-----------------------------------------------------------------------
# Correct some codes by hand. These are first done using the old module map and then 
# referencing the full framework if need be, using the csv that's output above. 
#-----------------------------------------------------------------------
hand_edits = read.csv(paste0(j, "Project/Evaluation/GF/mapping/multi_country/intervention_categories/raw_2017_data_edited.csv"))

post_2017_map = post_2017_map[!is.na(code)]
post_2017_map = rbind(post_2017_map, hand_edits, fill = TRUE) #Went from 803 rows to 723 rows, assuming because we dropped some duplicates, but should double check this. 

#Correct all 'tb/hiv' to 'hiv/tb' 
post_2017_map[disease == 'tb/hiv', disease:='hiv/tb']
#-----------------------------------------------------------------------
# Subset the map, and run a few last visual verifications on it. 
#-----------------------------------------------------------------------
post_2017_map = post_2017_map[, .(module, intervention, disease, lang, loc_name, code, coefficient)]
post_2017_map = unique(post_2017_map)

#Make sure you don't have any unexpected NAs
nrow(post_2017_map[is.na(code)])
nrow(post_2017_map[is.na(coefficient)])
nrow(post_2017_map[is.na(loc_name)])
nrow(post_2017_map[is.na(lang)])

#Visually inspect what modules are mapping to each disease. 
unique(post_2017_map$disease)
unique(post_2017_map[, .(substring(code, 1, 1)), by = 'disease'])
unique(post_2017_map[substring(code, 1, 1) == 'R', .(module)])

#Check that coefficients are all 1, we should have no redistribution. 
post_2017_map[coefficient != 1]

#-----------------------------------------------------------------------
# Merge this validated map back onto the post modular framwork one last 
# time just to make sure you caught everything! 
#-----------------------------------------------------------------------

check_merge = merge(post_2017_map, mod_framework_data, by = c('module', 'intervention', 'disease'), all.y = TRUE)
check_merge = check_merge[is.na(code)]
check_merge = check_merge[!(is.na(module) & is.na(intervention))]

write.csv(check_merge, paste0(j, "Project/Evaluation/GF/mapping/multi_country/intervention_categories/last_nas.csv"), row.names = FALSE)

#-----------------------------------------------------------------------
# Map cleaned GF module and intervention, and save data. 
#-----------------------------------------------------------------------
names(cleaned_interventions) = tolower(names(cleaned_interventions))

setnames(cleaned_interventions, old= c('module', 'intervention', 'abbreviated module'), new = c('gf_module', 'gf_intervention', 'abbreviated_module'))
cleaned_interventions = cleaned_interventions[, .(code, gf_module, gf_intervention, abbreviated_module)]

post_2017_map = merge(post_2017_map, cleaned_interventions, by='code', all.x = TRUE)

#See what codes you're missing from each language. Don't necessarily need to fix here but we might review this for NLP. 
fr_codes = post_2017_map[lang == 'fr', .(code)]
esp_codes = post_2017_map[lang == 'esp', .(code)]
eng_codes = post_2017_map[lang == 'eng', .(code)]

setdiff(cleaned_interventions$code, fr_codes$code)
setdiff(cleaned_interventions$code, esp_codes$code)
setdiff(cleaned_interventions$code, eng_codes$code)

setdiff(cleaned_interventions$code, post_2017_map$code)

saveRDS(post_2017_map, paste0(j, "Project/Evaluation/GF/mapping/multi_country/intervention_categories/post_2017_map.rds"))
write.csv(post_2017_map, paste0(j, "Project/Evaluation/GF/mapping/multi_country/intervention_categories/post_2017_map.csv"), row.names = FALSE)


# ----------------------------------------------
# AUTHOR: Emily Linebarger, based on code from Irena Chen 
# PURPOSE: Binds together resource tracking prepped data 
#   into six key datasets.
#   1. Approved budgets from grantmaking 
#   2. Most recent revision of budgets 
#   3. All budget revisions
#   4. Most recently reported semester of absorption 
#   5. Cumulative absorption 
#   6. All absorption data (budget, expenditure, absorption from all PUDRs in each country) **added by FRC on 5/18/2020
#
# These data can then be used to conduct analysis. 
# DATE: Last updated May 2020
# ----------------------------------------------


# Set up filepaths 
cod_prepped = paste0(box, "COD/prepped_data/")
gtm_prepped = paste0(box, "GTM/prepped_data/")
uga_prepped = paste0(box, "UGA/prepped_data/")
sen_prepped = paste0(box, "SEN/prepped_data/")

final_write = paste0(box, "tableau_data/")

# # create topic areas spreadsheet activity level - AB 6/15/20
# dt_all_rev = as.data.table(read.csv(paste0(box, 'tableau_data/all_budget_revisions_activityLevel.csv')))
# dt_unique = unique(dt_all_rev[, .(loc_name, disease, gf_module, gf_intervention, activity_description)])
# setorderv(dt_unique, c('loc_name', 'disease', 'gf_module', 'gf_intervention'))
# dt_unique[, cep_topic_area := FALSE]
# dt_unique[, keyword_topic_area := FALSE]
# dt_unique[, isTopicArea := FALSE]
# dt_unique[, topicAreaDesc := '']
# write.csv(dt_unique, paste0(dir, 'modular_framework_mapping/PCE2020_FocusTopicAreas.csv'))

# load spreadsheet to merge onto data to identify topic areas
topic_areas = as.data.table(read.csv(paste0(dir, 'modular_framework_mapping/PCE2020_FocusTopicAreas_activityLevel_wFRs_FINAL.csv'), na.strings=c("", "NA")))

# checks on topic areas manual entry
if(nrow(topic_areas[isTopicArea == TRUE & is.na(topicAreaDesc) ])!=0) stop('You need to enter a value for topicAreaDesc where isTopicArea = TRUE.')
<<<<<<< HEAD
if(nrow(topic_areas[isTopicArea == FALSE & !is.na(topicAreaDesc) ])!=0) stop('You should not have value for topicAreaDesc where isTopicArea = FALSE.')

# subset
topic_areas_activity = topic_areas[,.(loc_name, gf_module, gf_intervention, activity_description, disease, isTopicArea, topicAreaDesc, isTopicAreaActivity, topicAreaActivityDesc)]
topic_areas_intervention = unique(topic_areas[,.(loc_name, gf_module, gf_intervention, disease, isTopicArea, topicAreaDesc)])

=======
if(nrow(topic_areas[isTopicArea == FALSE & (!is.na(topicAreaDesc) | topicAreaDesc != '')])!=0) stop('You should not have value for topicAreaDesc where isTopicArea = FALSE.')
>>>>>>> 2554ba781bc7b6935da25bc11643703f8f91dde2
#---------------------------------------------
# 1. APPROVED BUDGETS 
#---------------------------------------------
cod1 = fread(paste0(cod_prepped, list.files(cod_prepped, pattern="approved_budgets")))
cod1[, loc_name:="DRC"]
gtm1 = fread(paste0(gtm_prepped, list.files(gtm_prepped, pattern="approved_budgets")))
gtm1[, loc_name:="Guatemala"]
uga1 = fread(paste0(uga_prepped, list.files(uga_prepped, pattern="approved_budgets")))
uga1[, loc_name:="Uganda"]
sen1 = fread(paste0(sen_prepped, list.files(sen_prepped, pattern="approved_budgets")))
sen1[, loc_name:="Senegal"]

approved_budgets = rbindlist(list(cod1, gtm1, uga1, sen1), use.names = TRUE, fill = TRUE)
approved_budgets = merge(approved_budgets, topic_areas_intervention, all.x = TRUE, by = c('loc_name', 'disease', 'gf_module', 'gf_intervention'))

write.csv(approved_budgets, paste0(final_write, "approved_budgets.csv"), row.names=F)
#---------------------------------------------
# 2. MOST RECENT BUDGET REVISION
#---------------------------------------------
cod2 = fread(paste0(cod_prepped, list.files(cod_prepped, pattern="most_recent_budgets")))
cod2[, loc_name:="DRC"]
gtm2 = fread(paste0(gtm_prepped, list.files(gtm_prepped, pattern="most_recent_budgets")))
gtm2[, loc_name:="Guatemala"]
uga2 = fread(paste0(uga_prepped, list.files(uga_prepped, pattern="most_recent_budgets")))
uga2[, loc_name:="Uganda"]
sen2 = fread(paste0(sen_prepped, list.files(sen_prepped, pattern="most_recent_budgets")))
sen2[, loc_name:="Senegal"]

most_recent_budgets = rbindlist(list(cod2, gtm2, uga2, sen2), use.names = TRUE, fill = TRUE)
most_recent_budgets = merge(most_recent_budgets, topic_areas_intervention, all.x = TRUE, by = c('loc_name', 'disease', 'gf_module', 'gf_intervention'))

write.csv(most_recent_budgets, paste0(final_write, "most_recent_budgets.csv"), row.names=F)
#---------------------------------------------
# 3. ALL BUDGET REVISIONS
#---------------------------------------------
cod3 = fread(paste0(cod_prepped, list.files(cod_prepped, pattern="all_budget_revisions_cod")))
cod3[, loc_name:="DRC"]
gtm3 = fread(paste0(gtm_prepped, list.files(gtm_prepped, pattern="all_budget_revisions_gtm")))
gtm3[, loc_name:="Guatemala"]
uga3 = fread(paste0(uga_prepped, list.files(uga_prepped, pattern="all_budget_revisions_uga")))
uga3[, loc_name:="Uganda"]
sen3 = fread(paste0(sen_prepped, list.files(sen_prepped, pattern="all_budget_revisions_sen")))
sen3[, loc_name:="Senegal"]

all_budget_revisions = rbindlist(list(cod3, gtm3, uga3, sen3), use.names = TRUE, fill = TRUE)

# # added this code below in order to have clearer understanding of which modules are added in between revisions *FRC 6/4/2020
# all_budget_revisions_long = all_budget_revisions[,.(grant, grant_period, gf_module, gf_intervention, disease, start_date, budget, budget_version, loc_name)] # subset to key variables
# dcast(all_budget_revisions_long, grant + grant_period + gf_module + gf_intervention + disease + start_date + loc_name ~ budget_version, value.var = "budget" ) # cast wide

all_budget_revisions = merge(all_budget_revisions, topic_areas_intervention, all.x = TRUE, by = c('loc_name', 'disease', 'gf_module', 'gf_intervention'))

write.csv(all_budget_revisions, paste0(final_write, "all_budget_revisions.csv"), row.names=F)

# -------------
# activity level budget revisions
cod3_act = fread(paste0(cod_prepped, list.files(cod_prepped, pattern="all_budget_revisions_act")))
cod3_act[, loc_name:="DRC"]
gtm3_act = fread(paste0(gtm_prepped, list.files(gtm_prepped, pattern="all_budget_revisions_act")))
gtm3_act[, loc_name:="Guatemala"]
uga3_act = fread(paste0(uga_prepped, list.files(uga_prepped, pattern="all_budget_revisions_act")))
uga3_act[, loc_name:="Uganda"]
sen3_act = fread(paste0(sen_prepped, list.files(sen_prepped, pattern="all_budget_revisions_act")))
sen3_act[, loc_name:="Senegal"]

all_budget_revisions_act = rbindlist(list(cod3_act, gtm3_act, uga3_act, sen3_act), use.names = TRUE, fill = TRUE)

all_budget_revisions_act = merge(all_budget_revisions_act, topic_areas_activity, all.x = TRUE, by = c('loc_name', 'disease', 'gf_module', 'gf_intervention', 'activity_description'))

all_budget_revisions_act = add_fr_es_to_dt(all_budget_revisions_act)

write.csv(all_budget_revisions_act, paste0(final_write, "all_budget_revisions_activityLevel.csv"), row.names=F)
#---------------------------------------------
# 4. CURRENT ABSORPTION 
#---------------------------------------------
cod4 = fread(paste0(cod_prepped, list.files(cod_prepped, pattern="most_recent_absorption")))
cod4[, loc_name:="DRC"]
gtm4 = fread(paste0(gtm_prepped, list.files(gtm_prepped, pattern="most_recent_absorption")))
gtm4[, loc_name:="Guatemala"]
uga4 = fread(paste0(uga_prepped, list.files(uga_prepped, pattern="most_recent_absorption")))
uga4[, loc_name:="Uganda"]
sen4 = fread(paste0(sen_prepped, list.files(sen_prepped, pattern="most_recent_absorption")))
sen4[, loc_name:="Senegal"]

most_recent_absorption = rbindlist(list(cod4, gtm4, uga4, sen4))
write.csv(most_recent_absorption, paste0(final_write, "most_recent_absorption.csv"), row.names=F)
#---------------------------------------------
# 5. CUMULATIVE ABSORPTION 
#---------------------------------------------
cod5 = fread(paste0(cod_prepped, list.files(cod_prepped, pattern="cumulative_absorption")))
cod5[, loc_name:="DRC"]
gtm5 = fread(paste0(gtm_prepped, list.files(gtm_prepped, pattern="cumulative_absorption")))
gtm5[, loc_name:="Guatemala"]
uga5 = fread(paste0(uga_prepped, list.files(uga_prepped, pattern="cumulative_absorption")))
uga5[, loc_name:="Uganda"]
sen5 = fread(paste0(sen_prepped, list.files(sen_prepped, pattern="cumulative_absorption")))
sen5[, loc_name:="Senegal"]

cumulative_absorption = rbindlist(list(cod5, gtm5, uga5, sen5))

# add focus topic mapping to the cumulative absorption files FRC
cumulative_absorption = merge(cumulative_absorption, topic_areas_intervention, all.x = TRUE, by = c('loc_name', 'disease', 'gf_module', 'gf_intervention'))

write.csv(cumulative_absorption, paste0(final_write, "cumulative_absorption.csv"), row.names=F)
#---------------------------------------------
# 6. ALL ABSORPTION (historica data from past PUDRs) --Added by FRC on 5/18/2020
#---------------------------------------------
cod6 = fread(paste0(cod_prepped, list.files(cod_prepped, pattern="all_absorption")))
cod6[, loc_name:="DRC"]
gtm6 = fread(paste0(gtm_prepped, list.files(gtm_prepped, pattern="all_absorption")))
gtm6[, loc_name:="Guatemala"]
uga6 = fread(paste0(uga_prepped, list.files(uga_prepped, pattern="all_absorption")))
uga6[, loc_name:="Uganda"]
sen6 = fread(paste0(sen_prepped, list.files(sen_prepped, pattern="all_absorption")))
sen6[, loc_name:="Senegal"]

all_absorption = rbindlist(list(cod6, gtm6, uga6, sen6))
all_absorption = merge(all_absorption, topic_areas_intervention, all.x = TRUE, by = c('loc_name', 'disease', 'gf_module', 'gf_intervention'))

write.csv(all_absorption, paste0(final_write, "all_absorption.csv"), row.names=F)

print("Step E: Aggregate GF files completed. Files saved in combined_prepped folder.")

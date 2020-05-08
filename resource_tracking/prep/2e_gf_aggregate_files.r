# ----------------------------------------------
# AUTHOR: Emily Linebarger, based on code from Irena Chen 
# PURPOSE: Binds together resource tracking prepped data 
#   into five key datasets.
#   1. Approved budgets from grantmaking 
#   2. Most recent revision of budgets 
#   3. All budget revisions
#   4. Most recently reported semester of absorption 
#   5. Cumulative absorption 
#
# These data can then be used to conduct analysis. 
# DATE: Last updated March 2020
# ----------------------------------------------


# Set up filepaths 
cod_prepped = paste0(box, "COD/prepped_data/")
gtm_prepped = paste0(box, "GTM/prepped_data/")
uga_prepped = paste0(box, "UGA/prepped_data/")
sen_prepped = paste0(box, "SEN/prepped_data/")

final_write = paste0(box, "tableau_data/")

# load spreadsheet to merge onto data to identify topic areas
topic_areas = as.data.table(read.xlsx(paste0(dir, 'modular_framework_mapping/identifyTopicAreas_PCE2020_forSubsetting.xlsx')))
# checks on topic areas manual entry
if(nrow(topic_areas[isTopicArea == TRUE & is.na(topicAreaDesc) ])!=0) stop('You need to enter a value for topicAreaDesc where isTopicArea = TRUE.')
if(nrow(topic_areas[isTopicArea == FALSE & !is.na(topicAreaDesc) ])!=0) stop('You should not have value for topicAreaDesc where isTopicArea = FALSE.')
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
approved_budgets = merge(approved_budgets, topic_areas, all.x = TRUE, by = c('loc_name', 'disease', 'gf_module', 'gf_intervention'))

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
most_recent_budgets = merge(most_recent_budgets, topic_areas, all.x = TRUE, by = c('loc_name', 'disease', 'gf_module', 'gf_intervention'))

write.csv(most_recent_budgets, paste0(final_write, "most_recent_budgets.csv"), row.names=F)
#---------------------------------------------
# 3. ALL BUDGET REVISIONS
#---------------------------------------------
cod3 = fread(paste0(cod_prepped, list.files(cod_prepped, pattern="all_budget_revisions")))
cod3[, loc_name:="DRC"]
gtm3 = fread(paste0(gtm_prepped, list.files(gtm_prepped, pattern="all_budget_revisions")))
gtm3[, loc_name:="Guatemala"]
uga3 = fread(paste0(uga_prepped, list.files(uga_prepped, pattern="all_budget_revisions")))
uga3[, loc_name:="Uganda"]
sen3 = fread(paste0(sen_prepped, list.files(sen_prepped, pattern="all_budget_revisions")))
sen3[, loc_name:="Senegal"]

all_budget_revisions = rbindlist(list(cod3, gtm3, uga3, sen3), use.names = TRUE, fill = TRUE)
all_budget_revisions = merge(all_budget_revisions, topic_areas, all.x = TRUE, by = c('loc_name', 'disease', 'gf_module', 'gf_intervention'))

write.csv(all_budget_revisions, paste0(final_write, "all_budget_revisions.csv"), row.names=F)
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
write.csv(cumulative_absorption, paste0(final_write, "cumulative_absorption.csv"), row.names=F)


print("Step E: Aggregate GF files completed. Files saved in combined_prepped folder.")

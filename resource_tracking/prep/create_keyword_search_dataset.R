# -----------------------------------------------
# AUTHOR: Audrey Batzel 
# PURPOSE: Create data set of unique activities for keyword search - this needs to be rerun when the inFile is updated:
# DATE: (Created separate spreadsheet 7/17)

# The current working directory should be the root of this repository
# -----------------------------------------------

# -----------------------------------------------
# set up R
# -----------------------------------------------
user=as.character(Sys.info()[7])

# source files with other functions and common resource tracking filepaths
source("./resource_tracking/prep/_common/set_up_r.R", encoding="UTF-8")
# -----------------------------------------------

# -----------------------------------------------
# Create data set for focus topic keyword search
# -----------------------------------------------
dt = data.table(read.csv(paste0(box, 'tableau_data/budgetRevisions_with_frBudgets_activityLevel.csv')))

dt_unique = unique(dt[, .(loc_name, disease, gf_module, gf_intervention, activity_description)])
setorderv(dt_unique, c('loc_name', 'disease', 'gf_module', 'gf_intervention'))
dt_unique[, keyword_topic_area := FALSE]
dt_unique[, isTopicArea := FALSE]
dt_unique[, topicAreaDesc := '']

dt_merge = data.table(read.csv(paste0(dir, 'modular_framework_mapping/archive/identifyTopicAreas_PCE2020_forSubsetting.csv')))
dt_merge = unique(dt_merge[isTopicArea==TRUE, .(loc_name, disease, gf_module, gf_intervention)])
dt_merge[, cep_topic_area := TRUE]

dt_unique = merge(dt_unique, dt_merge, all.x = TRUE, by = c('loc_name', 'disease', 'gf_module', 'gf_intervention'))
dt_unique[is.na(cep_topic_area), cep_topic_area := FALSE]

write.csv(dt_unique, paste0(dir, 'modular_framework_mapping/PCE2020_FocusTopicAreas_activityLevel_wFRs.csv'))
# -----------------------------------------------
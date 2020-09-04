#------------------------------------------------
# Audrey Batzel 
# 8/25/2020
# prepare data set for finalization of focus topics in UGA
# where the 
#------------------------------------------------

#------------------------------------------------
# set up r
#------------------------------------------------
rm(list = ls())

library(data.table)
box = "C:/Users/abatzel/Box Sync/Global Fund Files/"
dir = "J:/Project/Evaluation/GF/resource_tracking/"

library(stringr)
#------------------------------------------------

#------------------------------------------------
# load data
#------------------------------------------------
# NOTE: I reran the keyword search for UGA on the updated budget file that includes (FR 2017, approved, revisions, FR 2020)
dt = data.table(read.csv(paste0(dir, 'modular_framework_mapping/keyword_search/UGA/test_uganda_focus_topic_search_9_2_2020.csv')))

dt[budget_version == 'funding_request' & grant_period == '2018-2020', budget_version := 'funding_request17']
dt[budget_version == 'funding_request' & grant_period == '2021-2023', budget_version := 'funding_request20']
dt[budget_version == 'approved' , budget_version := 'approved_in_grant_making']

dt = dt[!is.na(budget_version),]
#------------------------------------------------

#------------------------------------------------
# primary intervention level focus topics
#------------------------------------------------
ta_int_level = data.table(read.csv(paste0(dir, 'modular_framework_mapping/archive/identifyTopicAreas_PCE2020_forSubsetting.csv')))
ta_int_level = ta_int_level[loc_name == 'Uganda',]
ta_int_level = ta_int_level[isTopicArea==TRUE, .(loc_name, disease, gf_module, gf_intervention, topicAreaDesc)]
ta_int_level[, primary_intervention_level := TRUE]
#------------------------------------------------

#------------------------------------------------
# subset data to relevant columns
#------------------------------------------------
dt[gf_intervention %in% unique(ta_int_level$gf_intervention), primary_intervention_level := TRUE]
dt[is.na(primary_intervention_level), primary_intervention_level := FALSE]

dt_subset = dt[, .(loc_name, gf_module, gf_intervention, activity_description, cost_category, file_name, budget_version, 
                   grant, disease, grant_disease, fr_disease, pr, primary_intervention_level, keyword_topic_area, topicAreaDesc)]
setnames(dt_subset, 'keyword_topic_area', 'keyword_search_result')
setnames(dt_subset, 'topicAreaDesc', 'keyword_search_topicAreaDesc')
dt_subset[, cep_decision:='']
#------------------------------------------------

#------------------------------------------------
# subset to only identified acitvites to review
#------------------------------------------------
# dt_save = dt_subset[ keyword_search_result == TRUE | budget_version == 'funding_request20', ]

write.csv(dt_subset, paste0(dir, 'modular_framework_mapping/keyword_search/UGA/uganda_final_keyword_search_review.csv'), row.names = FALSE)
#------------------------------------------------

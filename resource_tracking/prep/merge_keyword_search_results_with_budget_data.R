# -----------------------------------------------
# AUTHOR: Francisco Rios Casas and Audrey Batzel
# PURPOSE: Merge budget data with keyword search results
# DATE: moved to a new script 7/17/2020

# The current working directory should be the root of this repository
# -----------------------------------------------

# -----------------------------------------------
# set up r
# -----------------------------------------------
library(data.table)
user=as.character(Sys.info()[7])
# source files with other functions and common resource tracking filepaths
source("./resource_tracking/prep/_common/set_up_r.R", encoding="UTF-8")
# -----------------------------------------------

# -----------------------------------------------
# merge approved and revised budget data to keyword search results for all countries:
# -----------------------------------------------
# update these files with the keyword search runs to use:
drc = as.data.table(read.csv(paste0(dir, 'modular_framework_mapping/keyword_search/DRC/test_drc_focus_topic_search_7_23_2020.csv')))
uga = as.data.table(read.csv(paste0(dir, 'modular_framework_mapping/keyword_search/UGA/test_uganda_focus_topic_search_11_23_2020.csv')))
gtm = as.data.table(read.csv(paste0(dir, 'modular_framework_mapping/keyword_search/GTM/test_guatemala_focus_topic_search_7_23_2020.csv')))
sen = as.data.table(read.csv(paste0(dir, 'modular_framework_mapping/keyword_search/SEN/test_senegal_focus_topic_search_12_18_2020.csv')))
# combine keyword search results
dt = rbindlist(list(drc,uga,gtm,sen), use.names = TRUE, fill = TRUE)

# distinguish between NFM2 and NFM3 approved grants
dt = dt[grant_period!="2021-2023" & budget_version=="approved", budget_version:="approved_nfm2"]

# read in budget revisions data --this data contains values from approved budget files and budget revisions in NFM2 only (not FRs, or approved grant budgets in NFM3)
budget_rev = as.data.table(read.csv(paste0(box, 'tableau_data/all_budget_revisions_activityLevel.csv')))
budget_rev = budget_rev[, -c('isTopicArea', 'topicAreaDesc')]

# distinguish between NFM2 and NFM3 approved grants
budget_rev = budget_rev[grant_period!="2021-2023" & budget_version=="approved", budget_version:="approved_nfm2"]

# keep just revisions and approved budgets
budget_rev = budget_rev[ file_iteration %in% c('revision', 'approved_gm')]

# merge by the unique id's in the keyword search set
budget_dt = merge(dt, budget_rev, all.y = TRUE, by = c("loc_name", "disease", "gf_module", "gf_intervention", "activity_description", "budget_version"))

# # check that there's one file per budget version/grant
# budget_dt[, length(unique(file_name)), by = c('grant', 'budget_version')]

# subset data
budget_dt = budget_dt[, c("loc_name", "disease", "gf_module", "gf_intervention", "activity_description", 'cep_topic_area', 'keyword_topic_area', 'topicAreaDesc', 'grant', 'budget_version', 'budget')]
budget_dt = budget_dt[!is.na(budget_version), ]

# sum over cost categories 
by_cols = names(budget_dt)[!names(budget_dt) %in% 'budget']
budget_dt = budget_dt[, .(activity_budget = sum(budget)), by = by_cols]


# get each activity's percent of intervention budget
budget_dt[, intervention_budget := sum(activity_budget), by = .(loc_name, gf_module, gf_intervention, grant, budget_version)]
budget_dt[, activity_percent_of_intervention := round((activity_budget/intervention_budget)*100, 2)]

# subset data
dt_act_number = budget_dt[, .(loc_name, disease, gf_module, gf_intervention, activity_description, cep_topic_area, keyword_topic_area,
                       topicAreaDesc, grant, budget_version, activity_budget)]
dt_act_percent = budget_dt[, .(loc_name, disease, gf_module, gf_intervention, activity_description, cep_topic_area, keyword_topic_area,
                        topicAreaDesc, grant, budget_version, activity_percent_of_intervention)]



# cast wide so there is a column for each version
dt_wide_number = dcast.data.table(dt_act_number, loc_name + disease + gf_module + gf_intervention + activity_description + cep_topic_area + keyword_topic_area + topicAreaDesc + grant ~ budget_version )
dt_wide_percent = dcast.data.table(dt_act_percent, loc_name + disease + gf_module + gf_intervention + activity_description + cep_topic_area + keyword_topic_area + topicAreaDesc + grant ~ budget_version )

# save data
write.csv(dt_wide_number, paste0(dir, 'modular_framework_mapping/keyword_search/combined_keyword_search_results_activity_budgets.csv'), row.names = FALSE)
write.csv(dt_wide_percent, paste0(dir, 'modular_framework_mapping/keyword_search/combined_keyword_search_results_activity_percent_of_intervention.csv'), row.names = FALSE)
# -----------------------------------------------

# -----------------------------------------------
# merge fr budget data to keyword search results for all countries:
# -----------------------------------------------
# update these files with the keyword search runs to use:
drc = as.data.table(read.csv(paste0(dir, 'modular_framework_mapping/keyword_search/DRC/test_drc_focus_topic_search_7_23_2020.csv')))
uga = as.data.table(read.csv(paste0(dir, 'modular_framework_mapping/keyword_search/UGA/test_uganda_focus_topic_search_11_23_2020.csv')))
gtm = as.data.table(read.csv(paste0(dir, 'modular_framework_mapping/keyword_search/GTM/test_guatemala_focus_topic_search_7_23_2020.csv')))
sen = as.data.table(read.csv(paste0(dir, 'modular_framework_mapping/keyword_search/SEN/test_senegal_focus_topic_search_12_18_2020.csv')))
# combine keyword search results
dt = rbindlist(list(drc,uga,gtm,sen), use.names = TRUE, fill = TRUE)

# distinguish between 2017 FRs and 2020 FRs as well as new approved grant awards
dt <- dt[grant_period%in%c("2021-2023") & budget_version=="approved", budget_version:="approved_nfm3"]
dt <- dt[!grant_period%in%c("2021-2023") & budget_version=="approved", budget_version:="approved_nfm2"]

# dt = dt[, -c('budget_version')]

# read in budget revisions data with FRs (this file contains all NFM2 and NFM3 FRs, and approved budgets, and revisions available for each country)
dt_fr = as.data.table(read.csv(paste0(box, 'tableau_data/budgetRevisions_with_frBudgets_activityLevel.csv')))
dt_fr = dt_fr[, -c('isTopicArea', 'topicAreaDesc')]
dt_fr = dt_fr[ !budget_version %in% c('initial')]
dt_fr = dt_fr[ !is.na(budget_version)]
# dt_fr = dt_fr[ budget_version!="unknown"]

# distinguish between 2017 FRs and 2020 FRs as well as new approved grant awards
# dt_fr <- dt_fr[grant_period=="2021-2023" & budget_version=="funding_request", budget_version:="funding_request_20"]
# dt_fr <- dt_fr[!grant_period%in%c("2021-2023") & budget_version=="funding_request", budget_version:="funding_request_17"]
dt_fr <- dt_fr[!grant_period%in%c("2021-2023") & budget_version=="approved", budget_version:="approved_nfm2"]
dt_fr <- dt_fr[grant_period%in%c("2021-2023") & budget_version=="approved", budget_version:="approved_nfm3"]

# merge by the unique id's in the keyword search set
budget_dt = merge(dt, dt_fr, all.y = TRUE, by = c("loc_name", "disease", "gf_module", "gf_intervention", "activity_description", "budget_version", "grant_period"))

# subset data
budget_dt = budget_dt[, c('loc_name', 'pr', 'fr_disease', 'gf_module', 'gf_intervention', 'activity_description', 'cep_topic_area', 
                          'keyword_topic_area', 'topicAreaDesc', 'budget_version', 'budget')]

# sum over cost categories 
by_cols = names(budget_dt)[!names(budget_dt) %in% 'budget']
budget_dt = budget_dt[, .(activity_budget = sum(budget)), by = by_cols]
budget_dt[, intervention_budget := sum(activity_budget), by = .(loc_name, gf_module, gf_intervention, pr, fr_disease, budget_version)]
budget_dt[, activity_percent_of_intervention := round((activity_budget/intervention_budget)*100, 2)]

# subset data
dt_act_number = budget_dt[, .(loc_name, pr, fr_disease, gf_module, gf_intervention, activity_description, cep_topic_area, keyword_topic_area,
                       topicAreaDesc, budget_version, activity_budget)]
dt_act_percent = budget_dt[, .(loc_name, pr, fr_disease, gf_module, gf_intervention, activity_description, cep_topic_area, keyword_topic_area,
                        topicAreaDesc, budget_version, activity_percent_of_intervention)]

# cast wide so there is a column for each version
dt_wide_number = dcast.data.table(dt_act_number, loc_name + pr + fr_disease + gf_module + gf_intervention + activity_description + cep_topic_area + keyword_topic_area + topicAreaDesc ~ budget_version )
dt_wide_percent = dcast.data.table(dt_act_percent, loc_name + pr + fr_disease + gf_module + gf_intervention + activity_description + cep_topic_area + keyword_topic_area + topicAreaDesc ~ budget_version )

#reorder columns
dt_wide_number = dt_wide_number[, .(loc_name, pr, fr_disease, gf_module, gf_intervention, activity_description, cep_topic_area, keyword_topic_area, topicAreaDesc, 
                                    funding_request17, approved_nfm2, revision1, revision2, revision3, funding_request20, funding_request20_CT, funding_request20_TRP, approved_nfm3, unknown)]
dt_wide_percent = dt_wide_percent[, .(loc_name, pr, fr_disease, gf_module, gf_intervention, activity_description, cep_topic_area, keyword_topic_area, topicAreaDesc, 
                                    funding_request17, approved_nfm2, revision1, revision2, revision3, funding_request20, funding_request20_CT, funding_request20_TRP, approved_nfm3, unknown)]

write.csv(dt_wide_number, paste0(dir, 'modular_framework_mapping/keyword_search/combined_keyword_search_results_activity_budgets_withFRs_byPR.csv'), row.names = FALSE)
write.csv(dt_wide_percent, paste0(dir, 'modular_framework_mapping/keyword_search/combined_keyword_search_results_activity_percent_of_intervention_withFRs_byPR.csv'), row.names = FALSE)
# -----------------------------------------------


# -----------------------------------------------
## OLDER CODE BELOW
# -----------------------------------------------
# merge budget data to DRC data:
# -----------------------------------------------
# read in the budget revisions data set
dt = as.data.table(read.csv(paste0(dir, 'modular_framework_mapping/keyword_search/DRC/test_drc_focus_topic_search_7_8_2020.csv')))
dt = dt[, -c('isTopicArea')]
# read in the budger revisions dataset and subset to just DRC
budget_rev = as.data.table(read.csv(paste0(box, 'tableau_data/all_budget_revisions_activityLevel.csv')))
budget_rev = budget_rev[loc_name == 'DRC', ]
budget_rev = budget_rev[, -c('isTopicArea', 'topicAreaDesc')]

# merge the data sets
ta_budget_dt = merge(dt, budget_rev, all = TRUE, by = c("loc_name", "disease", "gf_module", "gf_intervention", "activity_description"))
dt = ta_budget_dt[, c("loc_name", "disease", "gf_module", "gf_intervention", "activity_description", 'cep_topic_area', 'keyword_topic_area', 'topicAreaDesc', 'grant', 'budget_version', 'budget')]
dt = dt[!is.na(budget_version), ]

# sum over cost categories 
by_cols = names(dt)[!names(dt) %in% 'budget']
dt = dt[, .(activity_budget = sum(budget)), by = by_cols]
dt[, intervention_budget := sum(activity_budget), by = .(loc_name, gf_module, gf_intervention, grant, budget_version)]
dt[, activity_percent_of_intervention := round((activity_budget/intervention_budget)*100, 2)]

dt_act_number = dt[, .(loc_name, disease, gf_module, gf_intervention, activity_description, cep_topic_area, keyword_topic_area,
                       topicAreaDesc, grant, budget_version, activity_budget)]
dt_act_percent = dt[, .(loc_name, disease, gf_module, gf_intervention, activity_description, cep_topic_area, keyword_topic_area,
                        topicAreaDesc, grant, budget_version, activity_percent_of_intervention)]

dt_wide_number = dcast.data.table(dt_act_number, loc_name + disease + gf_module + gf_intervention + activity_description + cep_topic_area + keyword_topic_area + topicAreaDesc + grant ~ budget_version )
dt_wide_percent = dcast.data.table(dt_act_percent, loc_name + disease + gf_module + gf_intervention + activity_description + cep_topic_area + keyword_topic_area + topicAreaDesc + grant ~ budget_version )

# subset_for_writeup_number = dt_wide_number[keyword_topic_area==TRUE,]
# write.csv(subset_for_writeup_number, paste0(dir, 'modular_framework_mapping/keyword_search/DRC/allocations_for_activities_identified_by_keywordsearch.csv'), row.names = FALSE)
# 
# subset_for_writeup_percent = dt_wide_percent[keyword_topic_area==TRUE,]
# write.csv(subset_for_writeup_percent, paste0(dir, 'modular_framework_mapping/keyword_search/DRC/activity_percent_of_intervention.csv'), row.names = FALSE)
# -----------------------------------------------

# -----------------------------------------------
# Compare key word search versions: 
## MAKE THIS A FUNCTION TO COMPARE ANY TWO? 
# -----------------------------------------------
# read in both key word searches
dt1 = as.data.table(read.csv(paste0(dir, 'modular_framework_mapping/keyword_search/DRC/test_drc_focus_topic_search_6_17_2020.csv')))
dt2 = as.data.table(read.csv(paste0(dir, 'modular_framework_mapping/keyword_search/DRC/test_drc_focus_topic_search_7_1_2020.csv')))
dt1 = dt1[, -c('isTopicArea', 'topicAreaDesc')]
dt2 = dt2[, -c('isTopicArea', 'topicAreaDesc')]

# compare the differences: 
setnames(dt1, 'keyword_topic_area', 'keyword_ta_dt1')
setnames(dt2, 'keyword_topic_area', 'keyword_ta_dt2')

dt= merge(dt1, dt2, by = names(dt1)[!names(dt1)%in%c('keyword_ta_dt1')])
now_false = dt[ keyword_ta_dt1 == TRUE & keyword_ta_dt2 == FALSE, ]
now_true = dt[ keyword_ta_dt1 == FALSE & keyword_ta_dt2 == TRUE, ]
# -----------------------------------------------

# -----------------------------------------------
# # merging budget data and troubleshooting:
# -----------------------------------------------
# dt = read_xlsx(paste0(dir, 'modular_framework_mapping/keyword_search/uganda_keyword_search_focus_topic_areas.xlsx'))
# dt = as.data.table(dt)
# nrow(unique(dt[,c("loc_name", "disease", "gf_module", "gf_intervention", "activity_description")]))
# dt = unique(dt[,c("loc_name", "disease", "gf_module", "gf_intervention", "activity_description")])
# setorderv(dt, c("loc_name", "disease", "gf_module", "gf_intervention", "activity_description"))
# dt[, id := .I]
# 
# budget_rev = read.csv(paste0(box, 'tableau_data/all_budget_revisions_activityLevel.csv'))
# budget_rev = as.data.table(budget_rev)
# budget_rev = budget_rev[loc_name == 'Uganda', ]
# budget_rev = budget_rev[, -c('isTopicArea', 'topicAreaDesc')]
# budget_rev = unique(budget_rev[,c("loc_name", "disease", "gf_module", "gf_intervention", "activity_description")])
# budget_rev = budget_rev[, lapply(.SD, as.character), .SDcols = names(budget_rev)]
# budget_rev = budget_rev[, lapply(.SD, trimws), .SDcols = names(budget_rev)]
# budget_rev = budget_rev[, activity_description:=str_replace_all(x, "[\r\n]" , "")]
# 
# setorderv(budget_rev, c("loc_name", "disease", "gf_module", "gf_intervention", "activity_description"))
# budget_rev[, id := .I]
# 
# check = merge(dt, budget_rev, all = TRUE, by = c("loc_name", "disease", "gf_module", "gf_intervention", "activity_description"))
# 
# 
# budget_rev = budget_rev[grant_period == '2018-2020' & file_iteration %in% c('approved_gm', 'revision'),]
# budget_rev = budget_rev[, .(budget = sum(budget)), by = c("loc_name", "disease", "gf_module", "gf_intervention", "activity_description", "grant", "budget_version")]
# 
# budget_rev = dcast.data.table(budget_rev, loc_name + disease + gf_module + gf_intervention + activity_description + grant ~ budget_version)
# -----------------------------------------------

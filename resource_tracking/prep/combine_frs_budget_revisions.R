# -----------------------------------------------
# AUTHOR: Audrey Batzel
# PURPOSE: Combine prepped FR budget data with the budget revisions data set
# DATE: July 2020

# The current working directory should be the root of this repository
# -----------------------------------------------
rm(list=ls())
# -----------------------------------------------
# SET UP R
# -----------------------------------------------
user=as.character(Sys.info()[7])
if (Sys.info()[1]=='Windows'){
  setwd(paste0("C:/Users/",user,"/Documents/gf/")) #Change to the root of your repository
} else {
  setwd(paste0("/ihme/homes/",user,"/gf/"))
}

# source files with other functions and common resource tracking filepaths
source("./resource_tracking/prep/_common/set_up_r.R", encoding="UTF-8")
source("./resource_tracking/prep/_common/load_master_list.r", encoding="UTF-8")
verbose = FALSE
file_list = load_master_list(purpose = 'financial') 
# -----------------------------------------------

# -----------------------------------------------
# read in data
# -----------------------------------------------
budget_rev = as.data.table(read.csv(paste0(box, 'tableau_data/all_budget_revisions_activityLevel.csv')))
fr_budgets = as.data.table(read.csv(paste0(box, "tableau_data/fr_budgets_all.csv"))) # changed this to read in the file with both nfm2 and nfm3 funding requests

names(budget_rev)[!names(budget_rev) %in% names(fr_budgets)]
names(fr_budgets)[!names(fr_budgets) %in% names(budget_rev)]
# -----------------------------------------------

# -----------------------------------------------
# merge the intervention level and activity level topic areas to FRs
# -----------------------------------------------
# load spreadsheet to merge onto data to identify topic areas
topic_areas = as.data.table(read.csv(paste0(dir, 'modular_framework_mapping/PCE2020_FocusTopicAreas_activityLevel_wFRs_FINAL.csv')))
topic_areas[topicAreaDesc == '', topicAreaDesc := NA]
# checks on topic areas manual entry
if(nrow(topic_areas[isTopicArea == TRUE & is.na(topicAreaDesc) ])!=0) stop('You need to enter a value for topicAreaDesc where isTopicArea = TRUE.')
if(nrow(topic_areas[isTopicArea == FALSE & !is.na(topicAreaDesc) ])!=0) stop('You should not have value for topicAreaDesc where isTopicArea = FALSE.')

# topic_areas = topic_areas[isTopicArea == TRUE,]
topic_areas = topic_areas[, -c('disease', 'X')]

fr_budgets = merge(fr_budgets, topic_areas, all.x = TRUE, by = c('loc_name', 'gf_module', 'gf_intervention', 'activity_description'))
# check = merge(fr_budgets[, -c('isTopicArea', 'topicAreaDesc')], topic_areas, all = TRUE, by = c('loc_name', 'gf_module', 'gf_intervention'))
# some of the interventions identified in the spreadsheet are not in the fr_budgets - did a visual check of this and it makes sense.
fr_budgets[is.na(isTopicArea), isTopicArea := FALSE]
# -----------------------------------------------

# -----------------------------------------------
# add a PR column to the budget rev
# -----------------------------------------------
budget_rev[, grant := as.character(grant)]
budget_rev[, pr := unlist(lapply(grant, function(x){str_split_fixed(x, '-', 3)[3]}))]
# add a "grant" disease column
budget_rev[, grant_disease := unlist(lapply(grant, function(x){str_split_fixed(x, '-', 3)[2]}))]
budget_rev[grant_disease == 'T', grant_disease := 'tb']
budget_rev[grant_disease == 'M', grant_disease := 'malaria']
budget_rev[grant_disease == 'H', grant_disease := 'hiv']
budget_rev[grant_disease == 'C', grant_disease := 'hiv/tb']
budget_rev[grant_disease == 'Z', grant_disease := 'tb']
# add which FR it corresponds to? 
budget_rev[loc_name == "DRC" & grant_disease %in% c('hiv/tb', 'hiv', 'tb'), fr_disease := 'hiv/tb']
budget_rev[loc_name == "DRC" & grant_disease %in% c('malaria'), fr_disease := 'malaria']
budget_rev[loc_name == "Uganda" & grant_disease %in% c('hiv/tb', 'hiv', 'tb'), fr_disease := 'hiv/tb']
budget_rev[loc_name == "Uganda" & grant_disease %in% c('malaria'), fr_disease := 'malaria']
budget_rev[loc_name == "Guatemala" & grant_disease %in% c('tb'), fr_disease := 'tb']
budget_rev[loc_name == "Guatemala" & grant_disease %in% c('hiv'), fr_disease := 'hiv']
budget_rev[loc_name == "Guatemala" & grant_disease %in% c('malaria'), fr_disease := 'malaria']
budget_rev[loc_name == "Senegal" & grant_disease %in% c('tb'), fr_disease := 'tb']
budget_rev[loc_name == "Senegal" & grant_disease %in% c('hiv'), fr_disease := 'hiv']
budget_rev[loc_name == "Senegal" & grant_disease %in% c('malaria'), fr_disease := 'malaria']
# -----------------------------------------------

# -----------------------------------------------
# combine FR budgets and budget revisions and save data
# -----------------------------------------------
dt = rbindlist(list(fr_budgets, budget_rev), use.names = TRUE, fill = TRUE)

write.csv(dt,paste0(box, 'tableau_data/budgetRevisions_with_frBudgets_activityLevel.csv'), row.names = FALSE)
# -----------------------------------------------

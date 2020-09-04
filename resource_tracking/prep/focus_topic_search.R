# -----------------------------------------------
# AUTHOR: Francisco Rios Casas and Audrey Batzel
# PURPOSE: Identify budget items that relate to country focus topics
# DATE: June 12, 2020

# The current working directory should be the root of this repository

# pending issues
# add option to specify budget_version?
# -----------------------------------------------

# -----------------------------------------------
# STEP 1: SET UP R
# -----------------------------------------------
user=as.character(Sys.info()[7])
if (Sys.info()[1]=='Windows'){
  setwd(paste0("C:/Users/",user,"/Documents/gf/")) #Change to the root of your repository
} else {
  setwd(paste0("/ihme/homes/",user,"/gf/"))
}

# source files with other functions and common resource tracking filepaths
source("./resource_tracking/prep/_common/set_up_r.R", encoding="UTF-8")
source("./resource_tracking/prep/gf_files_prep_functions/id_focus_topics.R")
# -----------------------------------------------

# -----------------------------------------------
# Run on country data:
# -----------------------------------------------
# activity level data
# inFile = paste0(dir, 'modular_framework_mapping/PCE2020_FocusTopicAreas.csv')
# activity level data including the 2017 FR budgets (eventually include the 2020 FR budgets)
inFile = paste0(dir, 'modular_framework_mapping/PCE2020_FocusTopicAreas_activityLevel_wFRs.csv')

# # just run the function on the budget dataset directly:
# inFile = paste0(box, 'tableau_data/budgetRevisions_with_frBudgets_activityLevel.csv') 
# note: this doesn't work with running the function below bc it doesn't have "cep_topic_area" column 
# (I ran it manually to still generate the results)

id_focus_topics("Senegal", include_module_intervention = TRUE, inFile)
# for guatemala only run certain keywords on the HIV grant
id_focus_topics('Guatemala', include_module_intervention = TRUE, inFile)

id_focus_topics('Uganda', include_module_intervention = TRUE, inFile)
id_focus_topics('DRC', include_module_intervention = TRUE, inFile)
# -----------------------------------------------

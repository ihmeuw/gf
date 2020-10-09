# Update the Focus Topics
# Run this script to source all of the steps that are necessary

# set repo directory as teh root of this file
setwd("~/gf")
rm(list=ls())

# 
# Steps to update the data:
#   
# 1. Update the intervention-level focus topics for approved budgets and budget revisions: "J:\Project\Evaluation\GF\resource_tracking\modular_framework_mapping\archive\identifyTopicAreas_PCE2020_forSubsetting.csv

# 2. Update the intervention-level focus topic selections for FR files in: "J:\Project\Evaluation\GF\resource_tracking\modular_framework_mapping\identifyTopicAreas_PCE2020_forSubsettingFRs.csv"

# 3. Run the file: "create_keyword_search_dataset.R" whenever new budget files are added to the RT database for any country
source("./resource_tracking/prep/create_keyword_search_dataset.R")

# 4. Run the file: "prep_final_focus_topic_mapping.R" which creates the final spreadsheet to map at the intervention level and at the activity level
source("./resource_tracking/prep/prep_final_focus_topic_mapping.R")

# 5. Run the file: "2e_aggregate_files.R" which will remap data to the focus topics

source("./resource_tracking/prep/2e_gf_aggregate_files.R")

# 6. Run: "combine_frs_budget_revisions.R" to map focus topics onto the activity level file called: budgetRevisions_with_frBudgets_activityLevel.csv"
source("./resource_tracking/prep/combine_frs_budget_revisions.R")

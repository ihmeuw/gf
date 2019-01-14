# ---------------------------------------------------------
# AUTHOR: Audrey Batzel and Emily Linebarger
# PURPOSE: Prepare resource tracking data for merge 
#          with activities and outputs.  
# DATE: Last updated January 2019. 
# ----------------------------------------------------------

#------------------------------------
# TO-DO: 
# -Do we want to include any GHE data? 
#------------------------------------

#Read in final budget and expenditure data from RT database, and bind together. 
final_budgets <- readRDS("J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/final_budgets.rds")
#final_expenditures <- readRDS("J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/final_expenditures.rds")

# budget_factors = is.factor(final_budgets)
# 
# for (col in colnames(final_expenditures)){
#   levels(col) = unique(final_expenditures$col)
# }

resource_tracking <- final_budgets #Will want to add expenditures in here eventually. 

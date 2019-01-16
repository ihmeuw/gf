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

#Subset to only the columns we want from resource tracking database and impact evaluation map 
drc_mal_rt = resource_tracking[country == "Congo (Democratic Republic)" & (disease == "malaria" | disease == "hss"), .(budget, start_date, code, loc_name, disease)]

drc_mal_rt = drc_mal_rt[, .(budget = sum(budget)), by = .(start_date, code, loc_name, disease)]

drc_mal_rt <- merge(drc_mal_rt, drc_mal_map, by = c('code'), allow.cartesian = TRUE)

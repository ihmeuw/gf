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
drc_mal_rt = drc_mal_rt[, .(budget = sum(budget, na.rm = TRUE)), by = .(start_date, code, loc_name, disease)]

#Map to intervention and indicator using the map for DRC malaria. 
drc_mal_rt <- merge(drc_mal_rt, drc_mal_map, by = c('code'), allow.cartesian = TRUE)

#Add in quarter variable for merge with outputs and activities, and remove start date. 
#We don't have data disaggregated to the month-level on the activites and outputs side but that's okay. 
drc_mal_rt$quarter <- quarter(drc_mal_rt$start_date)
drc_mal_rt$year <- year(drc_mal_rt$start_date)
drc_mal_rt$start_date <- NULL 
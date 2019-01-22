# ---------------------------------------------------------
# AUTHOR: Audrey Batzel and Emily Linebarger
# PURPOSE: Merge resource tracking, activity, and outputs data. 
# DATE: Last updated January 2019. 
# ----------------------------------------------------------

j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
dir = paste0(j, '/Project/Evaluation/GF/impact_evaluation/cod/prepped_data/')

#Specify the variables that you want to merge by here. 
merge_vars = c('year', 'quarter', 'indicator')

#Read in the previously saved file for outputs/activities in 2b
outputs_activities = paste0(dir, "outputs_activites_for_pilot.RDS") 
outputs_activities <- readRDS(outputs_activities)

#Read in the previously saved files for resource tracking in 2b 
resource_tracking <- paste0(dir, "prepped_resource_tracking.RDS")
resource_tracking <- readRDS(resource_tracking)

#Merge resource tracking and outputs/activites data 
merge_file <- merge(outputs_activities, resource_tracking, by = merge_vars, all = TRUE)

merge_file$loc_name = "cod"
merge_file$disease = "malaria"

merge_file <- setorderv(merge_file, c("year", "quarter"))
#merge_file <- merge_file[, .(year, quarter, code, module, intervention, indicator, indicator_type, value, completeness, budget, loc_name, disease, data_source)] #EKL are we missing some variables?
merge_file <- merge_file[, .(year, quarter, code, module, intervention, indicator, indicator_type, value, completeness, budget, loc_name, disease, other_dah, data_source)]


saveRDS(merge_file, paste0(dir, "pilot_data.RDS"))

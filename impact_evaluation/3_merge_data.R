# ---------------------------------------------------------
# AUTHOR: Audrey Batzel and Emily Linebarger
# PURPOSE: Merge resource tracking, activity, and outputs data. 
# DATE: Last updated January 2019. 
# INSTRUCTIONS: The current working directory should be the root of this repo (set manually by user)
# ----------------------------------------------------------


#Specify the variables that you want to merge by here. 
merge_vars = c('year', 'quarter', 'indicator')

#Read in the previously saved files for resource tracking in 2b 
resource_tracking <- readRDS(outputFile2a)

#Read in the previously saved file for outputs/activities in 2b
outputs_activities <- readRDS(outputFile2b)

#Merge resource tracking and outputs/activites data 
merge_file <- merge(outputs_activities, resource_tracking, by = merge_vars, all = TRUE)

merge_file$loc_name = "cod"
merge_file$disease = "malaria"

merge_file <- setorderv(merge_file, c("year", "quarter", "code", "indicator", "indicator_type"))
#merge_file <- merge_file[, .(year, quarter, code, module, intervention, indicator, indicator_type, value, completeness, budget, loc_name, disease, data_source)] #EKL are we missing some variables?
merge_file <- merge_file[, .(year, quarter, code, module, intervention, indicator, indicator_type, value, completeness, budget, loc_name, disease, other_dah, data_source)]

saveRDS(merge_file, paste0(dir, outputFile3))

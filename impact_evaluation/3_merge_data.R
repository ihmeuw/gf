# ---------------------------------------------------------
# AUTHOR: Audrey Batzel and Emily Linebarger
# PURPOSE: Merge resource tracking, activity, and outputs data. 
# DATE: Last updated January 2019. 
# ----------------------------------------------------------

j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
dir = paste0(j, '/Project/Evaluation/GF/impact_evaluation/cod/prepped_data/')

outputs_activities = paste0(dir, "outputs_activites_for_pilot.RDS")  # produced by code in 2b_prep_activities_outputs

dt <- readRDS(outputs_activities)

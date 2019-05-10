library(data.table)


expenditures = readRDS("J:/Project/Evaluation/GF/resource_tracking/_gf_files_gos/combined_prepped_data/final_expenditures.rds")
gtm_exp = dt[loc_name=='gtm']

budgets = readRDS("J:/Project/Evaluation/GF/resource_tracking/_gf_files_gos/combined_prepped_data/final_budgets.rds")
gtm_budgets = budgets[loc_name=='gtm']

# Analysis 
#What are they spending the majority of their money on? 
gtm_budgets[year > 2016 & disease%in%c('tb', 'rssh'), .(budget=sum(budget, na.rm=T)), by=c('gf_module', 'gf_intervention', 'code')][order(-budget)]

# For top categories, what are the activity descriptions 
head(unique(gtm_budgets[code=="T1_1", .(activity_description)]), 20)
head(unique(gtm_budgets[code=="R2_2", .(activity_description)]), 20)

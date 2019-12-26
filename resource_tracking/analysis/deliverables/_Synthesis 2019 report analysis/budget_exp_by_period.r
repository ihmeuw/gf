# Prep data to analyze budget variance between periods 
# Emily Linebarger, December 26 2019 

library(data.table) 

# Prep PUDR data 
expenditures = readRDS("C:/Users/elineb/Box Sync/Global Fund Files/tableau_data/final_expenditures.rds")

expenditures = expenditures[grant_period=="2018-2020", .(loc_name, grant, grant_period, disease, gf_module, budget, expenditure, start_date)]
expenditures[start_date=="2018-01-01" | start_date=="2018-04-01", semester:="S1"]
expenditures[start_date=="2018-07-01" | start_date=="2018-10-01", semester:="S2"]
expenditures[start_date=="2019-01-01" | start_date=="2019-04-01", semester:="S3"]

expenditures$start_date <- NULL
wide = dcast(expenditures, loc_name+grant+grant_period+disease+gf_module~semester, value.var=c('budget', 'expenditure'), fun.aggregate=sum)

# Prep original budget data, and bind onto PUDR data 
final_budgets = readRDS("C:/Users/elineb/Box Sync/Global Fund Files/tableau_data/final_budgets.rds")
final_budgets = final_budgets[grant_period=="2018-2020", .(original_budget=sum(budget, na.rm=T)), by=c('loc_name', 'grant', 'grant_period', 'disease', 'gf_module', 'start_date')]
final_budgets[start_date=="2018-01-01" | start_date=="2018-04-01", semester:="S1"]
final_budgets[start_date=="2018-07-01" | start_date=="2018-10-01", semester:="S2"]
final_budgets[start_date=="2019-01-01" | start_date=="2019-04-01", semester:="S3"]

budgets_wide = final_budgets[!is.na(semester)]
budgets_wide$start_date <- NULL
budgets_wide = dcast(budgets_wide, loc_name+grant+grant_period+disease+gf_module~semester, value.var='original_budget', fun.aggregate=sum)

setnames(budgets_wide, c('S1', 'S2', 'S3'), c('original_budget_S1', 'original_budget_S2', 'original_budget_S3'))

wide = merge(wide, budgets_wide, by=c('loc_name', 'grant', 'grant_period', 'disease', 'gf_module'), all.x=T)
saveRDS(wide, "J:/Project/Evaluation/GF/resource_tracking/_gf_files_gos/combined_prepped_data/budget_exp_wide.rds")

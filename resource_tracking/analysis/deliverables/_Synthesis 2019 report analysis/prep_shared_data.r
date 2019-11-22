# Prep data for Synthesis 2019-2020 to share with EHG 
# Emily Linebarger, last udpated November 22, 2019 

rm(list=ls()) 
library(data.table) 

#Read in data 
cod = readRDS("C:/Users/elineb/Box Sync/Global Fund Files/COD/prepped_data/absorption_cod.rds")
gtm = readRDS("C:/Users/elineb/Box Sync/Global Fund Files/GTM/prepped_data/absorption_gtm.rds")
sen = readRDS("C:/Users/elineb/Box Sync/Global Fund Files/SEN/prepped_data/absorption_sen.rds")
uga = readRDS("C:/Users/elineb/Box Sync/Global Fund Files/UGA/prepped_data/absorption_uga.rds")

absorption_mi = rbindlist(list(cod, gtm, sen, uga))
absorption_cc = readRDS("C:/Users/elineb/Box Sync/Global Fund Files/tableau_data/all_cost_categories.rds")
budgets = readRDS("C:/Users/elineb/Box Sync/Global Fund Files/tableau_data/final_budgets.rds")

#Calculate module/intervention absorption 
absorption_mi = absorption_mi[, .(budget=sum(budget, na.rm=T), expenditure=sum(expenditure, na.rm=T), 
                                  cumulative_budget=sum(cumulative_budget, na.rm=T), cumulative_expenditure=sum(cumulative_expenditure, na.rm=T)), 
                              by=c('loc_name', 'grant', 'grant_period', 'semester', 'gf_module', 'gf_intervention')]

s1_budget_mi = budgets[start_date>="2018-01-01" & start_date<"2018-07-01", .(original_budget=sum(budget, na.rm=T)), by=c('loc_name', 'grant', 'grant_period', 'gf_module', 'gf_intervention')]
s1_budget_mi[, semester:="Semester 1"]
s12_budget_mi = budgets[start_date>="2018-01-01" & start_date<"2019-01-01", .(original_budget=sum(budget, na.rm=T)), by=c('loc_name', 'grant', 'grant_period', 'gf_module', 'gf_intervention')]
s12_budget_mi[, semester:="Semester 1-2"]
s3_budget_mi = budgets[start_date>="2019-01-01" & start_date<"2019-07-01", .(original_budget=sum(budget, na.rm=T)), by=c('loc_name', 'grant', 'grant_period', 'gf_module', 'gf_intervention')]
s3_budget_mi[, semester:="Semester 3"]

all_original = rbindlist(list(s1_budget_mi, s12_budget_mi, s3_budget_mi))

absorption_mi = merge(absorption_mi, all_original, by=c('loc_name', 'grant', 'grant_period', 'gf_module', 'gf_intervention', 'semester'), all.x=T)

# Calculate absorption
absorption_mi[, absorption:=round((expenditure/budget)*100, 1)]
absorption_mi[, cumulative_absorption:=round((cumulative_expenditure/cumulative_budget)*100, 1)]

absorption_mi = absorption_mi[, .(loc_name, grant, grant_period, semester, gf_module, gf_intervention, absorption, expenditure, budget, 
                                  original_budget, cumulative_budget, cumulative_expenditure, cumulative_absorption)]

absorption_mi[, loc_name:=toupper(loc_name)]
write.csv(absorption_mi, "J:/Project/Evaluation/GF/resource_tracking/visualizations/deliverables/_Synthesis 2019/module_intervention_absorption.csv", row.names=F)
# Calculate cost category absorption 
absorption_cc = absorption_cc[, .(budget=sum(budget, na.rm=TRUE), expenditure=sum(expenditure, na.rm=T), 
                                  cumulative_budget=sum(cumulative_budget, na.rm=T), cumulative_expenditure=sum(cumulative_expenditure, na.rm=T)), 
                              by=c('loc_name', 'grant', 'grant_period', 'pudr_semester_financial', 'cleaned_cost_category')]
setnames(absorption_cc, c("cleaned_cost_category", "pudr_semester_financial"), c("cost_category", "semester"))

absorption_cc[pudr_semester_financial=="1-A", pudr_semester_financial:="Semester 1"]
absorption_cc[pudr_semester_financial=="1-AB", pudr_semester_financial:="Semester 1-2"]
absorption_cc[pudr_semester_financial=="1-B", pudr_semester_financial:="Semester 2"]
absorption_cc[pudr_semester_financial=="2-A", pudr_semester_financial:="Semester 3"]

# Calculate absorption
absorption_cc[, absorption:=round((expenditure/budget)*100, 1)]
absorption_cc[, cumulative_absorption:=round((cumulative_expenditure/cumulative_budget)*100, 1)]

absorption_cc = absorption_cc[, .(loc_name, grant, grant_period, semester, cost_category, absorption, expenditure, budget, 
                                  cumulative_budget, cumulative_expenditure, cumulative_absorption)]

absorption_cc[, loc_name:=toupper(loc_name)]

write.csv(absorption_cc, "J:/Project/Evaluation/GF/resource_tracking/visualizations/deliverables/_Synthesis 2019/cost_category_absorption.csv", row.names=F)


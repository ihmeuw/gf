# Emily Linebarger
# HIV Testing Analyses (budget and absorption) to complement SIGSA-SIGPRO analyses 
# September 2019 
setwd("C:/Users/elineb/Documents/gf") #Set to the root of your repository
library(data.table)
source("./resource_tracking/visualizations/graphing_functions.r")

budgets = readRDS("J:/Project/Evaluation/GF/resource_tracking/_gf_files_gos/combined_prepped_data/final_budgets.rds")
budgets = budgets[(grant=="GTM-H-INCAP" & grant_period=="2018-2020") | (grant=="GTM-H-HIVOS" & grant_period=="2018-2018")]
absorption = readRDS("J:/Project/Evaluation/GF/resource_tracking/_gf_files_gos/gtm/prepped_data/absorption_gtm.rds")
absorption = absorption[(grant=="GTM-H-INCAP" & grant_period=="2018-2020") | (grant=="GTM-H-HIVOS" & grant_period=="2018-2018")]

#How much is budgeted overall? 
budgets[, sum(budget, na.rm=T)]
budgets[, sum(budget, na.rm=T), by='grant']

#Single out HIV testing. 
test_codes = c('H2_7', 'H3_7', 'H4_6', 'H5_7', 'H6_7', 'H7_3', 'H8_5', 'H14', 'H14_1')
budgets1 = budgets[code%in%test_codes]
absorption1 = absorption[code%in%test_codes]

#How much is being spent in the grant on testing overall? 
budgets1[, sum(budget, na.rm=T)]
budgets1[, sum(budget, na.rm=T), by='grant']
#What's the proportion of the total budget? 
(budgets1[, sum(budget, na.rm=T)]/budgets[, sum(budget, na.rm=T)])*100


# What's the breakdown by subpopulation? 
dt1 = budgets1[, .(budget=sum(budget, na.rm=T)), by=c('gf_intervention')]
dt1[, total:=sum(budget)]
dt1[, prop:=round((budget/total)*100, 2)]


# Break down the testing budget by percentage
dt2 = budgets1[, .(budget=sum(budget, na.rm=T)), by=c('grant')]
dt2[, total:=sum(budget)]
dt2[, prop:=round((budget/total)*100, 2)]


# Review Guatemala financial data for 2019-2020 annual report 
# Emily Linebarger 
# February 6, 2020 

library(data.table)

#---------------------------------
# ABSORPTION 
#---------------------------------
dt = readRDS("C:/Users/elineb/Box Sync/Global Fund Files/GTM/prepped_data/absorption_gtm.rds")

# Has the TB absorption ratio improved in the last year of the grant? 
tb = dt[grant_period=="2016-2019" & grant=="GTM-T-MSPAS"]
tb = tb[, .(budget=sum(budget, na.rm=T), expenditure=sum(expenditure, na.rm=T)), by=c('start_date', 'semester')]
tb[, absorption:=round((expenditure/budget)*100, 1)]

# What's the cumulative absorption for TB from the last PUDR? 
tb = dt[grant_period=="2016-2019" & grant=="GTM-T-MSPAS" & start_date == "2018-07-01"]
tb = tb[, .(budget=sum(cumulative_budget, na.rm=T), expenditure=sum(cumulative_expenditure, na.rm=T)), by='semester']
tb[, absorption:=round((expenditure/budget)*100, 1)]

#---------------------------------
# BUDGETS
#---------------------------------
# What was the RSSH allocation for the current grants? 
dt = readRDS("C:/Users/elineb/Box Sync/Global Fund Files/GTM/prepped_data/final_budgets.rds")
dt = dt[disease=="rssh" & ((grant=="GTM-H-INCAP" & grant_period=="2018-2020")| 
                             (grant=="GTM-M-MSPAS" & grant_period=="2019-2021") | 
                             (grant=="GTM-T-MSPAS" & grant_period=="2019-2022"))]

dt = dt[, .(budget=sum(budget, na.rm=T)), by=c('gf_module', 'grant')]
dt[, total:=sum(budget), by='grant']
dt[, pct:=(budget/total)*100]
dt = dt[order(grant, -pct)]

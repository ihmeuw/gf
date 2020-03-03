# Review Guatemala financial data for 2019-2020 annual report 
# Emily Linebarger 
# February 6, 2020 

library(data.table)
library(ggplot2)
library(scales)

source("C:/Users/elineb/Documents/gf/resource_tracking/analysis/graphing_functions.r")

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

# How much has the Global Fund contributed to LLINs from 2015 - present? Only look at non-overlapping PUDRs (most recent). 
malaria = dt[grant=="GTM-M-MSPAS"]

# Also look at GOS
gos = readRDS("J:/Project/Evaluation/GF/resource_tracking/_gf_files_gos/gos/prepped_data/prepped_gos_data.rds")
gos_malaria = gos[disease=="malaria" & loc_name=="gtm"]
gos_malaria[, .(budget=sum(budget, na.rm=T)), by=c('year')][order(year)]

gos_malaria[year>=2015 & year<=2017, .(budget=sum(budget, na.rm=T))]

gos_malaria[gf_intervention%in%c("Long lasting insecticidal nets: Mass campaign", "Long lasting insecticidal nets: Continuous distribution") & year>=2015 & year<=2017, sum(budget, na.rm=T)]

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


# Compare the malaria budget between the last grant and this one. (Make the graph in Excel) 
dt = readRDS("C:/Users/elineb/Box Sync/Global Fund Files/GTM/prepped_data/final_budgets.rds")
malaria = dt[grant=="GTM-M-MSPAS", .(budget=sum(budget, na.rm=T)), by=c('grant_period', 'gf_module')]
write.csv(malaria, "J:/Project/Evaluation/GF/resource_tracking/visualizations/deliverables/_GTM 2019 annual report/compare_malaria_budgets.csv")

# Just check the 2019-2021 grant
malaria = dt[grant=="GTM-M-MSPAS" & grant_period=="2019-2021", .(budget=sum(budget, na.rm=T)), by=c('gf_module')]
malaria[, total:=sum(budget)]
malaria[, pct:=round((budget/total)*100, 1)]
malaria[, budget:=dollar(budget)]

# Are there additional resources for community health workers at the intervention-level? 
malaria = dt[grant=="GTM-M-MSPAS" & grant_period=="2019-2021", .(budget=sum(budget, na.rm=T)), by=c('gf_module', 'gf_intervention')]
malaria[, total:=sum(budget)]
malaria[, pct:=round((budget/total)*100, 1)]
malaria[, budget:=dollar(budget)]

# Check the numbers in the 2019-2022 TB grant breakdown graph.
# We don't have any budget revision files for this grant, so just use the final budget. 
tb = dt[grant=="GTM-T-MSPAS" & grant_period=="2019-2022", .(budget=sum(budget, na.rm=T)), by='gf_module']
tb[, total:=sum(budget)]
tb[, pct:=round((budget/total)*100, 1)]

# Compare the budget for 2016-2019 with 2019-2022. 
tb2 = dt[grant=="GTM-T-MSPAS" & (grant_period%in%c("2016-2019", "2019-2022")), .(budget=sum(budget, na.rm=T)), by=c('gf_module', 'grant_period')]
tb2[, total:=sum(budget)]
tb2[, pct:=round((budget/total)*100, 1)]
p = ggplot(tb2, aes(x=gf_module, y=budget, fill=grant_period, label=dollar(budget))) + 
  geom_bar(stat="identity", position="dodge") + 
  geom_text(position = position_dodge(width=0.6), hjust=0) + 
  coord_flip() + 
  theme_bw() + 
  labs(x="", y="Budget", fill="", title="Comparison of GTM-T-MSPAS grant budgets") + 
  scale_y_continuous(labels=scales::dollar, limits=c(0, 5000000))
  
  
ggsave("J:/Project/Evaluation/GF/resource_tracking/visualizations/deliverables/_GTM 2019 annual report/compare_tb_budgets.png", p, height=8, width=12)

# What was the HMIS + M&E investment as a percentage of the RSSH total in the INCAP grant? 
incap = dt[grant=="GTM-H-INCAP" & grant_period=="2018-2020" & disease=="rssh", .(budget=sum(budget, na.rm=T)), by=c('gf_module')]
incap[, total:=sum(budget)]
incap[, pct:=round((budget/total)*100, 1)]
incap[, budget:=dollar(budget)]

# What's the HMIS percentage of RSSH in each grant? 
current_rssh = dt[current_grant==TRUE & disease=="rssh", .(budget=sum(budget, na.rm=TRUE)), by=c('gf_module', 'grant', 'grant_period')]
current_rssh[, total:=sum(budget), by=c('grant', 'grant_period')]
current_rssh[, pct:=round((budget/total)*100, 1)]
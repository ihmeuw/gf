# Senegal financial analysis, using euros
# Emily Linebarger, 11/19/2019 

rm(list=ls())
library(data.table) 
library(scales)
library(ggplot2)

source("C:/Users/elineb/Documents/gf/resource_tracking/analysis/graphing_functions.r")
user='elineb'

#------------------------------------------
#Read in data 
#------------------------------------------
absorption = readRDS("C:/Users/elineb/Box Sync/Global Fund Files/SEN/prepped_data/absorption_sen_euro.rds")

all_mods = readRDS("J:/Project/Evaluation/GF/resource_tracking/modular_framework_mapping/all_interventions.rds")
setnames(all_mods, c('module_eng', 'intervention_eng', 'abbrev_mod_eng', 'abbrev_int_eng'), c('gf_module', 'gf_intervention', 'abbrev_mod', 'abbrev_int'))
all_mods = unique(all_mods[, .(gf_module, gf_intervention, disease, abbrev_mod, abbrev_int)])
absorption = merge(absorption, all_mods, by=c('gf_module', 'gf_intervention', 'disease'), allow.cartesian=TRUE)

#make sure this merge worked correctly. 
stopifnot(nrow(absorption[is.na(abbrev_int)])==0)

#Get cumulative absorption dataset
cumulative_absorption = get_cumulative_absorption(byVars=c('abbrev_mod', 'grant'), countrySubset=c("SEN"), currency=c("EUR"))

#------------------------------------------
# PLOT DATA 
#------------------------------------------

# 1. Line plot that shows absorption in each period. 
by_grant = absorption[grant_period=="2018-2020", .(budget=sum(budget, na.rm=T), expenditure=sum(expenditure, na.rm=T)), by=c('grant', 'grant_disease', 'start_date', 'semester')]
by_grant[, absorption:=(expenditure/budget)*100]
by_grant[, label1:=paste0(round(absorption, 1), "%")]
by_grant[absorption>200, absorption:=200] # Cap absorption at 200%
by_grant[, label2:=paste0(grant_disease)]

p1 = ggplot(by_grant, aes(x=semester, y=absorption, color=grant, group=grant)) + 
  geom_point() + 
  geom_line() + 
  theme_bw(base_size=16) + 
  labs(title="Absorption by grant for 2018-2020 grant period", x="Grant semester", y="Absorption (%)", color="Grant")

# 2. Bar graph that shows 18-month cumulative absorption by grant. 
cumulative_by_grant = cumulative_absorption[, .(budget=sum(budget, na.rm=T), expenditure=sum(expenditure, na.rm=T)), 
                                 by=c('grant')]
cumulative_by_grant[, absorption:=round((expenditure/budget)*100, 1)]
cumulative_by_grant = melt(cumulative_by_grant, id.vars=c('grant', 'absorption'), value.name="amount")

cumulative_by_grant[variable=="budget", label:=""] #Don't display the expenditure amount on the budget bar. 
cumulative_by_grant[variable=="expenditure", label:=paste0(dollar(amount), " (", absorption, "%)")]
cumulative_by_grant[variable=="budget", variable:="Budget"]
cumulative_by_grant[variable=="expenditure", variable:="Expenditure"]


p2 = ggplot(cumulative_by_grant, aes(x=grant, y=amount, fill=variable, label=label)) + 
  geom_bar(stat="identity", position="identity") + 
  geom_text(hjust=0) + 
  theme_bw(base_size=14) + 
  coord_flip() + 
  scale_y_continuous(labels=scales::dollar) + 
  labs(title="Absorption by grant", subtitle="January 2018-June 2019", x="Grant", 
       y="Absorption (%)", fill="", caption="*Labels show expenditure amounts and absorption percentages")

# 3. Cumulative absorption by GF module, but adding new module categories per Allison's request
kvp_mods = c("Prevention programs for MSM", "Prevention programs for PWID", "Prevention programs for CSW & clients",
             "Prevention programs for other KVP", "Prevention programs for youth/adol.")
hiv_care = c("PMTCT", "Treatment, care & support", "Unspecified")
malaria_case_management = "Case management"
rssh=c("Community systems", "Info systems & M&E", "HR & health workers", "Service delivery", 
       "Nat. health strategies", "PSM")
vector_control = c("Specific prev. interventions", "Vector control")
tb_care = c("MDR-TB", "Care & prevention")
pm = c("Program mgmt")

plot_data = copy(cumulative_absorption)
plot_data[abbrev_mod%in%kvp_mods, module_cat:="Prevention programs for KVP"]
plot_data[abbrev_mod%in%hiv_care, module_cat:="HIV care and prevention"]
plot_data[abbrev_mod%in%pm, module_cat:="Program management"]
plot_data[abbrev_mod%in%rssh, module_cat:="RSSH"]
plot_data[abbrev_mod%in%malaria_case_management, module_cat:="Malaria case management"]
plot_data[abbrev_mod%in%vector_control, module_cat:="Malaria prevention and vector control"]
plot_data[abbrev_mod%in%tb_care, module_cat:="TB care and prevention"]
plot_data[abbrev_mod=="TB/HIV", module_cat:="TB/HIV co-infection"]

plot_data[is.na(module_cat), module_cat:=abbrev_mod]

# Plot data 
plot_data = plot_data[, .(budget=sum(budget, na.rm=T), expenditure=sum(expenditure, na.rm=T)), 
                                            by=c('module_cat')]
plot_data[, absorption:=round((expenditure/budget)*100, 1)]
plot_data = melt(plot_data, id.vars=c('module_cat', 'absorption'), value.name="amount")

plot_data[variable=="budget", label:=""] #Don't display the expenditure amount on the budget bar. 
plot_data[variable=="expenditure", label:=paste0(dollar(amount), " (", absorption, "%)")]
plot_data[variable=="budget", variable:="Budget"]
plot_data[variable=="expenditure", variable:="Expenditure"]

p3 = ggplot(plot_data, aes(x=module_cat, y=amount, fill=variable, label=label)) + 
  geom_bar(stat="identity", position="identity") + 
  geom_text(hjust=0) + 
  theme_bw(base_size=14) + 
  coord_flip() + 
  scale_y_continuous(labels=scales::dollar) + 
  labs(title="Absorption by module category", subtitle="January 2018-June 2019", x="", 
       y="Absorption (%)", fill="", caption="*Labels show expenditure amounts and absorption percentages")

pdf("J:/Project/Evaluation/GF/resource_tracking/visualizations/deliverables/_SEN 2019 annual report/report_graphs.pdf", height=8, width=11)
p1 
p2
p3
dev.off() 

#-----------------------------------------------------------------
# Two requests after first submission: 
# 1. Show budget/expenditure for 2015-2017 reporting period 
# 2. show funding landscape for malaria
# Emily Linebarger 1/13
# 
# 
# budgets = readRDS("C:/Users/elineb/Box Sync/Global Fund Files/tableau_data/final_budgets_sen.rds")
# expenditures = readRDS("C:/Users/elineb/Box Sync/Global Fund Files/tableau_data/final_expenditures_sen.rds")

# What are the budgets/expenditures for each year in the 2015-2017 range? 
# This will probably be better to do with absorption. 
absorption = readRDS("J:/Project/Evaluation/GF/resource_tracking/_gf_files_gos/gos/prepped_data/prepped_gos_data.rds")
absorption = absorption[loc_name=="sen"]
completeness_check = unique(absorption[start_date>="2015-01-01" & start_date < "2018-01-01", 
                                       .(grant, grant_period, start_date)][order(grant, grant_period, start_date)])
completeness_check[, id:=.GRP, by=c('grant', 'grant_period')]
ggplot(completeness_check, aes(x=start_date, y=id, color=paste0(grant, "_", grant_period))) + 
  geom_line() + 
  geom_point() 
# Yes, this data looks complete, and the GOS data is at the quarter-level, so no need to worry about overlaps. 
absorption[, year:=year(start_date)]
output = absorption[start_date>="2015-01-01" & start_date<"2018-01-01", .(budget=sum(budget), expenditure=sum(expenditure)), 
           by=c('disease', 'year')][order(year, disease)]
output = dcast(output, disease~year, value.var=c('budget', 'expenditure'))
output = output[, .(disease, budget_2015, expenditure_2015, budget_2016, expenditure_2016, budget_2017, expenditure_2017)]
write.csv(output, "J:/Project/Evaluation/GF/resource_tracking/visualizations/deliverables/_SEN 2019 annual report/budget_expenditure.csv", row.names=F)

#Funding landscape
source("C:/Users/elineb/Documents/gf/resource_tracking/analysis/graphing_functions.r")
landscape = funding_landscape('ribbon', 'sen', 'malaria', 2010, 2017, includeGHE = TRUE)
ggsave("J:/Project/Evaluation/GF/resource_tracking/visualizations/deliverables/_SEN 2019 annual report/funding_landscape.png", landscape, height=8, width=12 )

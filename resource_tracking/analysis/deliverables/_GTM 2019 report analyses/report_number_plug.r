# Number plug for Guatemala report 2019-2020 
# Emily Linebarger, December 2 2019 

#-----------------------
# PREP DATA 
#-----------------------
rm(list=ls())
library(data.table) 
source("C:/Users/elineb/Documents/gf/resource_tracking/analysis/graphing_functions.r")

save_loc = "J:/Project/Evaluation/GF/resource_tracking/visualizations/deliverables/_GTM 2019 annual report/"

absorption = readRDS("C:/Users/elineb/Box Sync/Global Fund Files/GTM/prepped_data/absorption_gtm.rds")
gos = readRDS("J:/Project/Evaluation/GF/resource_tracking/_gf_files_gos/gos/prepped_data/prepped_gos_data.rds")
cost_categories = readRDS("C:/Users/elineb/Box Sync/Global Fund Files/tableau_data/all_cost_categories.rds")
all_files = readRDS("C:/Users/elineb/Box Sync/Global Fund Files/GTM/prepped_data/budget_pudr_iterations.rds")

revised_budgets = all_files[file_name%in%c("Copia de GTM-H-INCAP_DB_11.10. Final_For PR.XLSX", "DB-GTM-M-MSPAS_28.10.19.xlsx", "1d_GTM-T-MSPAS Detailed budget_FINAL_01062016.xlsx", 
                                           "GTM_T_Full_Budget_9Sept2018.xlsx")]
#--------------------------
# GENERAL 
#--------------------------

# What's the average absorption rate, by disease, from 2013-2018? 
# First, remove overlap in semesters from the PUDRs and bind together with the GOS. 
gos = gos[loc_name=="gtm" & start_date>="2013-01-01"]

# Limit absorption so you don't re-cover dates from GOS 
gos_coverage = unique(gos[, .(grant, grant_period, start_date)][order(grant, grant_period, start_date)])
for (i in 1:nrow(gos_coverage)) { 
  absorption = absorption[grant!=gos_coverage$grant[i] & grant_period!=gos_coverage$grant_period[i] & start_date!=gos_coverage$start_date[i]]
}
# Review the remaining data visually 
unique(absorption[, .(grant, grant_period, start_date)][order(grant, grant_period, start_date)])

# Combine this data together. 
plot_data = rbind(gos, absorption, use.names=T, fill=T)
range(plot_data$start_date) # Should be 2013-2018 
#--------------------------
# RSSH 
#--------------------------
# What was the total investment in HMIS in the grants? What is this as a percentage of the total HMIS budget, and total RSSH budget? 
hmis = revised_budgets[disease=="rssh", .(budget=sum(budget, na.rm=T)), by=c('gf_module', 'grant', 'grant_period')]
hmis[, total:=sum(budget), by=c('grant', 'grant_period')]
hmis[, mod_pct:=round((budget/total)*100, 1)]

#----------------
# TB
#----------------
tb = revised_budgets[grant=="GTM-T-MSPAS" & grant_period=="2019-2022", .(budget=sum(budget)), by='gf_module']
tb[, total:=sum(budget)]
tb[, mod_pct:=round((budget/total)*100, 1)]
#---------------------------------
# Analyses 
#----------------------------------

# How much was budgeted and spent for malaria from 2013-2018? (Figure 14)
figure_14=plot_data[disease=="malaria", .(budget=sum(budget, na.rm=T), expenditure=sum(expenditure, na.rm=T)), by=c('year')]
figure_14[, absorption:=round((expenditure/budget)*100, 1)]
figure_14 = melt(figure_14, id.vars=c('year', 'absorption'))
p = ggplot(figure_14, aes(x=year, y=value, fill=variable)) + 
  geom_bar(stat="identity", position="dodge") + 
  theme_bw(base_size=16) + 
  scale_y_continuous(labels=scales::dollar) + 
  labs(title="Global Fund budget for malaria", subtitle="2013-2018", caption="*Does not include RSSH", x="Year", y="", fill="")
ggsave(paste0(save_loc, "figure_14_malaria.png"), p, height=8, width=11)

# What's the average absorption rate by disease over 2013-2018? 
figure_15 = plot_data[, .(budget=sum(budget, na.rm=T), expenditure=sum(expenditure, na.rm=T)), by=c('disease')]
figure_15[, absorption:=round((expenditure/budget)*100, 1)]

# How about by grant disease, to collapse RSSH in? 
figure_15_2 = plot_data[, .(budget=sum(budget, na.rm=T), expenditure=sum(expenditure, na.rm=T)), by=c('grant_disease')]
figure_15_2[, absorption:=round((expenditure/budget)*100, 1)] # It's practically the same. 

# What's the average absorption rate, by disease, on 'personal services' and 'materials' from 2013-2018? 
# We will get as close as we can using cost category data. 
# Graphs for Guatemala report 
# Emily Linebarger, November 11, 2019 

# Set up R
rm(list=ls())
library(data.table) 
library(wesanderson)
library(ggplot2)
library(scales)

save_loc = "J:/Project/Evaluation/GF/resource_tracking/visualizations/deliverables/_GTM 2019 annual report/"
#Source graphing functions
source("C:/Users/elineb/Documents/gf/resource_tracking/analysis/graphing_functions.r")

#Read in absorption data 
gtm = readRDS("C:/Users/elineb/Box Sync/Global Fund Files/GTM/prepped_data/absorption_gtm.rds")
gtm = gtm[grant_period%in%c('2018-2018', '2018-2020', '2019-2021') | (grant=="GTM-T-MSPAS" & grant_period=="2016-2019" & semester=="Semester 5")]

#Paste the grant period on each grant name for viewing purposes 
gtm[, grant:=paste0(grant, " (", grant_period, ")")]

#Assign each grant a distinct color from the "Darjeeling1" palette. 
grants = unique(gtm$grant) 
for (i in 1:length(grants)){
  g = grants[i]
  gtm[grant==g, grant_color:=wes_palette("Darjeeling1")[i]]
}

#Improve the semester label 
gtm[, date_range:=paste0(month(start_date), "/", year(start_date), "-", 
                         month(end_date), "/", year(end_date))]

#Add abbreviated module names. 
all_mods = readRDS("J:/Project/Evaluation/GF/resource_tracking/modular_framework_mapping/all_interventions.rds")
setnames(all_mods, c('module_eng', 'intervention_eng', 'abbrev_mod_eng', 'abbrev_int_eng'), c('gf_module', 'gf_intervention', 'abbrev_mod', 'abbrev_int'))
all_mods = unique(all_mods[, .(gf_module, gf_intervention, disease, abbrev_mod, abbrev_int)])
gtm = merge(gtm, all_mods, by=c('gf_module', 'gf_intervention', 'disease'))


#-------------------------------------
# SHOW RSSH ABSORPTION BY GRANT 
# ------------------------------------
rssh_mods = c('Community systems', 'Financial systems', "Info systems & M&E", "HR & health workers", "Service delivery", "Nat. health strategies")

rssh_by_int = gtm[abbrev_mod%in%rssh_mods, .(budget=sum(budget, na.rm=TRUE), expenditure=sum(expenditure, na.rm=TRUE)), by=c('abbrev_mod', 'abbrev_int', 'grant')]
rssh_by_int[, absorption:=(expenditure/budget)*100]
rssh_by_int[, label1:=paste0(round(absorption, 1), "%")]
rssh_by_int[absorption>200, absorption:=200] # Cap absorption at 200%

#Shorten a few intervention names. 
rssh_by_int[abbrev_int=="Supportive policy and programmatic environment", abbrev_int:="Supportive policy environment"]

#Sort the graph by module 
rssh_by_int[, module_num:=.GRP, by='abbrev_mod']
rssh_by_int[, int_num:=.GRP, by=c('abbrev_mod', 'abbrev_int')]
rssh_by_int[, rssh_factor:=paste0(module_num, int_num)]

rssh_by_int$int_factor = factor(rssh_by_int$rssh_factor, unique(rssh_by_int$rssh_factor), unique(rssh_by_int$abbrev_int))

for (g in unique(rssh_by_int$grant)){
  subset = rssh_by_int[grant==g]
  p1 = ggplot(subset, aes(x=int_factor, y=absorption, fill=abbrev_mod, label=label1))+ 
    geom_bar(stat="identity", position="dodge") + 
    geom_text(hjust=-0.5, size=4) + 
    theme_bw(base_size=14) + 
    scale_fill_manual(values=wes_palette(6, name="Rushmore1", type='continuous')) + 
    coord_flip() + 
    scale_y_continuous(limits=c(0, 220)) + 
    labs(title=paste0("2018-2019 RSSH absorption by intervention\nfor ", g), subtitle="Interventions grouped by module", x="Intervention", y="Absorption (%)",   fill="Module", caption="*Max bar height set at 200%")
 
  rssh_melt = subset[, .(int_factor, label1, budget, expenditure)]
  rssh_melt = melt(rssh_melt, id.vars=c('int_factor', 'label1'), variable.name='variable', value.name='amount')
  rssh_melt[, label1:=dollar(amount)]
  rssh_melt[variable=="budget", label1:=""] #Don't display the expenditure amount on the budget bar. 
  
  #Fix budget and expenditure so they display nicely 
  rssh_melt[variable=="budget", variable:="Budget"]
  rssh_melt[variable=="expenditure", variable:="Expenditure"]
  
  p2 = ggplot(rssh_melt, aes(x=int_factor, y=amount, fill=variable))+ 
    geom_bar(stat="identity", position="identity") + 
    geom_text(aes(label=label1), hjust=0, size=4) + 
    theme_bw(base_size=16) + 
    coord_flip() + 
    scale_y_continuous(labels = scales::dollar, limits=c(0, 350000)) + 
    labs(title=paste0("RSSH budget and expenditure by intervention \nfor ", g), x="Intervention", y="Amount ($)", fill="", 
         subtitle="Labels show expenditure amounts")
  
  ggsave(paste0(save_loc, paste0("rssh_", g, ".png")), grid.arrange(p1, p2, ncol=1), height=15, width=10)
} 

#------------------------------------------------------
# SHOW RSSH BUDGET BY MODULE USING MOST RECENT REVISION
#------------------------------------------------------
dt = readRDS("C:/Users/elineb/Box Sync/Global Fund Files/GTM/prepped_data/budget_pudr_iterations.rds") #Change to your Box Sync folder
dt = dt[file_name%in%c("Copia de GTM-H-INCAP_DB_11.10. Final_For PR.XLSX", "DB-GTM-M-MSPAS_28.10.19.xlsx", "GTM_T_Full_Budget_9Sept2018.xlsx")]
dt = merge(dt, all_mods, by=c('gf_module', 'gf_intervention', 'disease'), all.x=T)

rssh_mods = c('Community systems', 'Financial systems', "Info systems & M&E", "HR & health workers", "Service delivery", "Nat. health strategies")

plot_data1 = dt[abbrev_mod%in%rssh_mods, .(budget=sum(budget, na.rm=T)), by=c('abbrev_mod', 'abbrev_int')]
plot_data1[, module_num:=.GRP, by='abbrev_mod']
plot_data1[, int_num:=.GRP, by=c('abbrev_mod', 'abbrev_int')]
plot_data1[, rssh_factor:=paste0(module_num, int_num)]

plot_data1$int_factor = factor(plot_data1$rssh_factor, unique(plot_data1$rssh_factor), unique(plot_data1$abbrev_int))

p1 = ggplot(plot_data1, aes(x=int_factor, y=budget, label=dollar(budget), fill=abbrev_mod))+ 
  geom_bar(stat="identity") + 
  geom_text(hjust=0) + 
  theme_bw(base_size=14) + 
  scale_fill_manual(values=wes_palette(6, name="Rushmore1", type='continuous')) + 
  coord_flip() + 
  scale_y_continuous(labels=scales::dollar, limits=c(0, 1750000)) + 
  labs(title=paste0("RSSH interventions for most recent budgets, pooled"), subtitle="Interventions grouped by module", x="Intervention", y="Budget (USD)",   fill="Module", caption="*Max bar height set at 200%")

ggsave(paste0(save_loc, "revised_budget_rssh_pooled.png"), p1, height=8, width=11)

plot_data2 = dt[abbrev_mod%in%rssh_mods, .(budget=sum(budget, na.rm=T)), by=c('abbrev_mod', 'abbrev_int', 'grant')]
plot_data2[, module_num:=.GRP, by='abbrev_mod']
plot_data2[, int_num:=.GRP, by=c('abbrev_mod', 'abbrev_int')]
plot_data2[, rssh_factor:=paste0(module_num, int_num)]

plot_data2$int_factor = factor(plot_data2$rssh_factor, unique(plot_data2$rssh_factor), unique(plot_data2$abbrev_int))

p2 = ggplot(plot_data2[grant=="GTM-H-INCAP"], aes(x=int_factor, y=budget, label=dollar(budget), fill=abbrev_mod))+ 
  geom_bar(stat="identity") + 
  geom_text(hjust=0) + 
  theme_bw(base_size=14) +
  scale_y_continuous(labels=scales::dollar, limits=c(0, 1000000)) + 
  scale_fill_manual(values=wes_palette(6, name="Rushmore1", type='continuous')) + 
  coord_flip() + 
  labs(title=paste0("RSSH interventions for most recent GTM-H-INCAP budget"), subtitle="Interventions grouped by module", x="Intervention", y="Budget (USD)",   fill="Module", caption="*Max bar height set at 200%")

ggsave(paste0(save_loc, paste0("revised_budget_rssh_GTM-H-INCAP.png")), p2, height=8, width=11)

p2 = ggplot(plot_data2[grant=="GTM-M-MSPAS"], aes(x=int_factor, y=budget, label=dollar(budget), fill=abbrev_mod))+ 
  geom_bar(stat="identity") + 
  geom_text(hjust=0) + 
  theme_bw(base_size=14) +
  scale_y_continuous(labels=scales::dollar, limits=c(0, 800000)) + 
  scale_fill_manual(values=wes_palette(6, name="Rushmore1", type='continuous')) + 
  coord_flip() + 
  labs(title=paste0("RSSH interventions for most recent GTM-M-MSPAS budget"), subtitle="Interventions grouped by module", x="Intervention", y="Budget (USD)",   fill="Module", caption="*Max bar height set at 200%")

ggsave(paste0(save_loc, paste0("revised_budget_rssh_GTM-M-MSPAS.png")), p2, height=8, width=11)

p2 = ggplot(plot_data2[grant=="GTM-T-MSPAS"], aes(x=int_factor, y=budget, label=dollar(budget), fill=abbrev_mod))+ 
  geom_bar(stat="identity") + 
  geom_text(hjust=0) + 
  theme_bw(base_size=14) +
  scale_y_continuous(labels=scales::dollar, limits=c(0, 200000)) + 
  scale_fill_manual(values=wes_palette(6, name="Rushmore1", type='continuous')) + 
  coord_flip() + 
  labs(title=paste0("RSSH interventions for most recent GTM-T-MSPAS budget"), subtitle="Interventions grouped by module", x="Intervention", y="Budget (USD)",   fill="Module", caption="*Max bar height set at 200%")

ggsave(paste0(save_loc, paste0("revised_budget_rssh_GTM-T-MSPAS.png")), p2, height=8, width=11)



#-------------------------------------------------------
# SHOW BUDGET BY MODULE FOR LATEST GTM-H-INCAP REVISION
# ------------------------------------------------------
revisions = readRDS("C:/Users/elineb/Box Sync/Global Fund Files/GTM/prepped_data/budget_pudr_iterations.rds") #Change to your Box Sync folder
revisions = revisions[file_name%in%c("Copia de GTM-H-INCAP_DB_11.10. Final_For PR.XLSX")]

pudr_period = revisions[start_date<="2019-04-01", .(budget=sum(budget, na.rm=T)), by=c('gf_module')]
pudr_period[, label:=dollar(budget)]
full_budget_period = revisions[, .(budget=sum(budget, na.rm=T)), by=c('gf_module')]
full_budget_period[, label:=dollar(budget)]

all_mods2= unique(all_mods[, .(gf_module, abbrev_mod)])

pudr_period = merge(pudr_period, all_mods2, by=c('gf_module'), all.x=T)
full_budget_period = merge(full_budget_period, all_mods2, by=c('gf_module'), all.x=T)

p1 = ggplot(pudr_period[!is.na(abbrev_mod)], aes(x=abbrev_mod, y=budget, label=label)) + 
  geom_bar(stat="identity", fill="chocolate3") +
  geom_text(hjust=0) + 
  theme_bw(base_size=16) + 
  coord_flip() + 
  scale_y_continuous(labels=scales::dollar, limits=c(0, 1000000)) + 
  labs(title="Most recent budget for GTM-H-INCAP", subtitle="October 2018-June 2019", x="Module", y="Budget (USD)")

p2 = ggplot(full_budget_period[!is.na(abbrev_mod)], aes(x=abbrev_mod, y=budget, label=label)) + 
  geom_bar(stat="identity", fill="blue1") +
  geom_text(hjust=0) + 
  theme_bw(base_size=16) + 
  coord_flip() + 
  scale_y_continuous(labels=scales::dollar, limits=c(0, 6000000)) + 
  labs(title="Most recent budget for GTM-H-INCAP", subtitle="October 2018-October 2020", x="Module", y="Budget (USD)")

ggsave(paste0(save_loc, "INCAP_budget_revision1.png"), p1, height=8, width=11)
ggsave(paste0(save_loc, "INCAP_budget_revision2.png"), p2, height=8, width=11)


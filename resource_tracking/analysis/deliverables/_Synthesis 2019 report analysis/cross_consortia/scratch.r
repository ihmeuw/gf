#--------------------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Scratch area for synthesis 
# (graphs might not be used in report)
# DATE: Last updated November 25, 2019 
#---------------------------------------------------------

rm(list=ls())
library(data.table) 
library(ggplot2) 
library(scales) 
library(readxl)

modules = readRDS("J:/Project/Evaluation/GF/resource_tracking/_other_data_sources/multi_country/2019-2020_synthesis/all_modules.rds")
cost_categories = readRDS("J:/Project/Evaluation/GF/resource_tracking/_other_data_sources/multi_country/2019-2020_synthesis/all_cost_categories.rds")
source("C:/Users/elineb/Documents/gf/resource_tracking/analysis/graphing_functions.r")
save_loc = "J:/Project/Evaluation/GF/resource_tracking/visualizations/deliverables/_Synthesis 2019/"
#------------------------------
# Analyses 
#------------------------------

#-----------------------------------------------------------------------------------
# Show how RSSH has just been divided proportionally among all quarters in a budget
ihme_budgets = readRDS("C:/Users/elineb/Box Sync/Global Fund Files/tableau_data/final_budgets.rds")
plot_data = ihme_budgets[grant_period%in%c('2018-2020'), .(budget=sum(budget, na.rm=T)), by=c('grant', 'grant_period', 'disease', 'start_date', 'gf_module')]
plot_data[, date_col:=paste0("q", quarter(start_date), "-", year(start_date))]
plot_data$start_date<- NULL
plot_data = dcast(plot_data, grant+grant_period+disease+gf_module~date_col, value.var='budget')

# Show how budget has changed between quarters 
quarters = c('q1-2018', 'q2-2018', 'q3-2018', 'q4-2018', 'q1-2019', 'q2-2019', 'q3-2019', 'q4-2019', 'q1-2020', 'q2-2020',
             'q3-2020', 'q4-2020')
for (i in 1:(length(quarters)-1)){
  plot_data[, paste0(quarters[i+1], "-change"):=(get(quarters[i+1])-get(quarters[i]))]
}
keep_cols = names(plot_data)[grepl("-change", names(plot_data))]
keep_cols = c(keep_cols, 'grant', 'grant_period', 'disease', 'gf_module')
plot_data = plot_data[, keep_cols, with=FALSE]

# Melt this data back 
plot_data = melt(plot_data, id.vars=c('grant', 'grant_period', 'disease', 'gf_module'))
plot_data[, quarter:=tstrsplit(variable, "-", keep=1)]
plot_data[, quarter:=gsub("q", "", quarter)]
plot_data[, quarter:=as.numeric(quarter)]
plot_data[, year:=tstrsplit(variable, "-", keep=2)]
plot_data[, year:=as.numeric(year)]

plot_data[, date:=year+((quarter/4)-0.25)]

# How many times is there zero-variance between quarters for a module? 
zero_variance = plot_data[value==0]
plot_data2 = zero_variance[, .(num_quarters=.N), by=c('disease', 'date')]

plot_data2[, disease:=toupper(disease)]
plot_data2[disease=="MALARIA", disease:="Malaria"]

p1 = ggplot(plot_data2, aes(x=date, y=num_quarters, fill=disease, label=num_quarters)) + 
  geom_bar(stat="identity", position="dodge") + 
  geom_text(vjust=0, position=position_dodge(width=0.2)) + 
  theme_bw(base_size=16) + 
  labs(title="Number of grants whose budget did not change from one quarter to the next", x="Budget quarter",
       y="Number of grants with zero variance between budget quarters", fill="")

ggsave(paste0(save_loc, "zero_variance.png"), p1, height=8, width=15)

#-----------------------------------------------------------------------------------
# Is there a movement of funds away from RSSH activities towards disease-specific activities through grant revision? 
plot_data = modules[, .(loc_name, grant, abbrev_mod, cumulative_budget, original_budget)]
plot_data[, diff:=cumulative_budget-original_budget]
plot_data[diff>0 & !is.na(diff), change:="Additional funds allocated"]
plot_data[diff<0 & !is.na(diff), change:="Funds removed"]

plot_data = plot_data[, .(num_grants=.N), by=c('abbrev_mod', 'change')]
plot_data = plot_data[!is.na(change)] #Remove NAs at this point; you don't have to have every module in every grant. 
plot_data[, label:=paste0(num_grants)]
plot_data[change=="Funds removed", num_grants:=-num_grants]

#Get the absolute count of what's changed to order the graph. 
metadata = plot_data[, .(abbrev_mod, change, num_grants)]
metadata = dcast(plot_data, abbrev_mod~change, value.var=c('num_grants'))
metadata[is.na(`Additional funds allocated`), `Additional funds allocated`:=0]
metadata[is.na(`Funds removed`), `Funds removed`:=0]
metadata[, absolute_change:=`Additional funds allocated` + `Funds removed`]
metadata = metadata[, .(abbrev_mod, absolute_change)]

plot_data = merge(plot_data, metadata, by=c('abbrev_mod'), all.x=T)

p2 = ggplot(plot_data, aes(x=reorder(abbrev_mod, absolute_change), y=num_grants, fill=change, label=label)) + 
  geom_bar(stat="identity") + 
  geom_text(hjust=0) + 
  theme_bw(base_size=16) + 
  coord_flip() + 
  labs(title="PUDR cumulative budget compared to original budget", x="", 
       y="Number of grants with funds allocated or removed", fill="")

#-----------------------------------------------------------------------------------
# Program management - explore activities/cost categories and see what kind of things are happening under this module. 

# This will be difficult to do across consortia with the data we have. 

#-----------------------------------------------------------------------------------
# Has higher program management absorption led to higher overall grant absorption? 

# Just do two bars, one with program management absorption for each grant and one with overall absorption. 
pm = modules[abbrev_mod=="Program mgmt", .(cumulative_budget=sum(cumulative_budget), 
                                           cumulative_expenditure=sum(cumulative_expenditure), 
                                           original_budget_pm=sum(original_budget)), 
             by=c('loc_name', 'grant')]
pm[, pm_absorption:=round((cumulative_expenditure/cumulative_budget)*100, 1)]

all = modules[, .(cumulative_budget=sum(cumulative_budget), 
                  cumulative_expenditure=sum(cumulative_expenditure), 
                  original_budget_all=sum(original_budget)), 
             by=c('loc_name', 'grant')]
all[, overall_absorption:=round((cumulative_expenditure/cumulative_budget)*100, 1)]

plot_data = merge(pm, all, by=c('loc_name', 'grant'), all=T)
plot_data[, pm_pct_of_budget:=round((original_budget_pm/original_budget_all)*100, 1)]

p4 = ggplot(plot_data, aes(x=pm_absorption, y=overall_absorption)) + 
  geom_point() + 
  geom_smooth() + 
  theme_bw(base_size=16) + 
  labs(title="Does high program management absorption \ncorrelate with higher overall absorption?", x="Program management absorption (%)", 
       y="Overall grant absorption (%)", subtitle="Absorption calculated over the period Jan. 2018-June 2019", 
       caption="*Each point represents one grant", size="Percentage of budget that is \nprogram management")

ggsave("J:/Project/Evaluation/GF/resource_tracking/visualizations/deliverables/_Synthesis 2019/program_mgmt_vs_absorption.png", p4, height=8, width=11)


# Review if a higher percentage of budget devoted to PM correlates to higher absorption overall. 
p5 = ggplot(plot_data, aes(x=pm_pct_of_budget, y=overall_absorption)) + 
  geom_point() + 
  geom_smooth() + 
  theme_bw(base_size=16) + 
  labs(title="Does a higher budget percentage for program management\ncorrelate with higher overall absorption?",
       x="Percentage of budget given to program management (%)", 
       y="Overall grant absorption (%)", subtitle="Absorption calculated over the period Jan. 2018-June 2019", 
       caption="*Each point represents one grant", size="Percentage of budget that is \nprogram management")

pdf("J:/Project/Evaluation/GF/resource_tracking/visualizations/deliverables/_Synthesis 2019/extra_synthesis_analyses.pdf", height=8, width=11)
p1
p2
p4
p5

dev.off() 

#--------------------------------
# How commoditized are the grants, and do more commoditized grants have higher absorption? 

#Map the parent categories 
parent_categories = data.table(read_xlsx("J:/Project/Evaluation/GF/resource_tracking/modular_framework_mapping/cost_category_mapping.xlsx"))
plot_data = copy(cost_categories)
plot_data[, category_code:=tstrsplit(cost_category, " ", keep=1)][, category_code:=as.numeric(category_code)]

parent_categories = parent_categories[, .(category_code, parent_category)]
plot_data = merge(plot_data, parent_categories, all.x=T)
stopifnot(nrow(plot_data[is.na(parent_category)])==0)

# What is the commoditization levels in the grants? 
plot_data[parent_category%in%c("Health Products - Pharmaceutical Products (HPPP)", "Health Products - Non-Pharmaceuticals (HPNP)"), 
          type:="Commodities"]
plot_data[is.na(type), type:="Non-commodities"]

plot_data1 = plot_data[, .(budget=sum(cumulative_budget), expenditure=sum(cumulative_expenditure)), by=c('loc_name', 'grant', 'type')]
plot_data1[, grant:=paste0(loc_name, ", ", grant)]
plot_data1[, total_budget:=sum(budget), by=c('grant')]
plot_data1[, commodity_pct:=round((budget/total_budget)*100, 1)]
plot_data1 = plot_data1[type=="Commodities"]

p = ggplot(plot_data1, aes(x=grant, y=commodity_pct, label=paste0(commodity_pct, "%"))) + 
  geom_bar(stat="identity", fill="blue") + 
  geom_text(hjust=0) + 
  theme_bw() + 
  coord_flip() + 
  labs(title="Percentage of each grant that is commodities", subtitle="Showing budget for January 2018-June 2019", 
       caption="*Commodities defined as cost categories 4 & 5", x="", y="Budget Percentage that is Commodities")

# What is the absorption level for each grant? 
plot_data1 = plot_data[, .(budget=sum(cumulative_budget), expenditure=sum(cumulative_expenditure)), by=c('loc_name', 'grant', 'type')]
plot_data1[, grant:=paste0(loc_name, ", ", grant)]
plot_data1[, absorption:=round((expenditure/budget)*100, 1)]
plot_data1 = plot_data1[, .(grant, budget, expenditure)]
p2 = budget_exp_bar(plot_data1, xVar='grant', altTitle="Absorption by grant")

pdf(paste0(save_loc, "commodity_absorption.pdf"), height=8, width=13)
p
p2
dev.off() 

# How many activities are in each of our 2018-2020 grant budgets? 
budgets = readRDS("C:/Users/elineb/Box Sync/Global Fund Files/tableau_data/final_budgets.rds")
budgets = budgets[current_grant==TRUE]

acts_by_grant = unique(budgets[, .(grant, grant_period, activity_description)])
acts_by_grant = acts_by_grant[, .N, by=c('grant', 'grant_period')]
acts_by_disease = unique(budgets[, .(grant, grant_period, activity_description, disease)])
acts_by_disease = acts_by_disease[, .N, by=c('grant', 'grant_period', 'disease')]

# Can you plot the breakdown by disease graphically? 
plot_data = copy(acts_by_disease) 
plot_data[disease=="rssh", category:="RSSH"]
plot_data[is.na(category), category:="Not RSSH"]
plot_data = plot_data[, .(count=sum(N)), by=c('grant', 'grant_period', 'category')]

# merge_data = expand.grid(grant=unique(plot_data$grant), category=c('Not RSSH', 'RSSH'))
# plot_data = merge(plot_data, merge_data, by=c('grant', 'category'), all=T)
p = ggplot(plot_data, aes(x=paste0(grant, ", ", grant_period), y=count, fill=category)) + 
  geom_bar(stat="identity", position='dodge') + 
  theme_bw(base_size=16) + 
  coord_flip() + 
  labs(title="Count of activity descriptions by grant", x="", y="Number of activities", fill="")

ggsave(paste0(save_loc, "count_of_activities.png"), p, height=8, width=11)
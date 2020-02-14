rm(list=ls())
library(data.table) 
library(ggplot2) 
library(scales) 
library(readxl)

modules = readRDS("J:/Project/Evaluation/GF/resource_tracking/_other_data_sources/multi_country/2019-2020_synthesis/all_modules.rds")
cost_categories = readRDS("J:/Project/Evaluation/GF/resource_tracking/_other_data_sources/multi_country/2019-2020_synthesis/all_cost_categories.rds")
source("./gf/resource_tracking/analysis/graphing_functions.r") #Change to your repo location
save_loc = "J:/Project/Evaluation/GF/resource_tracking/visualizations/deliverables/_Synthesis 2019/"

#--------------------------------
# How commoditized are the grants, and do more commoditized grants have higher absorption? 

# Map the parent categories 
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

# scatterplot of absorption vs. percent of commodities by grant
plot_data1 = plot_data[, .(budget=sum(cumulative_budget), expenditure=sum(cumulative_expenditure)), by=c('loc_name', 'grant', 'type')]
plot_data1[, grant:=paste0(loc_name, ", ", grant)]
plot_data1[, total_budget:=sum(budget), by=c('grant')]
plot_data1[, commodity_pct:=round((budget/total_budget)*100, 1)]
plot_data1 = plot_data1[type=="Commodities"]
plot_data1[, absorption:=round((expenditure/budget)*100, 1)]

p3 = ggplot(plot_data1, aes(x=commodity_pct, y=absorption)) + 
  geom_point(size = 3, color="purple") +
  theme_bw() + 
  labs(title="Percentage of grant that is commodities vs. absorption", subtitle="January 2018-June 2019", 
       caption="*Commodities defined as cost categories 4 & 5\n*Each point represents one grant", 
       x="Budget Percentage that is Commodities", y="Absorption by Grant") +
  coord_fixed() +
  ylim(0, 100) + xlim(0, 100) + theme(text = element_text(size = 18)) 

# Show what the absorption is for one commodity-based module, vector control, by country. 
plot_data = modules[abbrev_mod=="Vector control", .(budget=sum(cumulative_budget, na.rm=T), expenditure=sum(cumulative_expenditure, na.rm=T)), by=c('loc_name')]
p4 = budget_exp_bar(plot_data, xVar='loc_name', altTitle="Absorption for vector control interventions, by country", 
               altSubtitle="January 2018-June 2019")


pdf(paste0(save_loc, "commodity_absorption.pdf"), height=8, width=13)
p
p2
p3
p4
dev.off() 
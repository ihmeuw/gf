#--------------------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Print final cross-consortia synthesis report graphs
# DATE: Last updated November 25, 2019 
#---------------------------------------------------------

rm(list=ls()) 

library(data.table)
library(ggplot2)
library(scales)

#-------------
#Read in data 
#-------------
all_modules = readRDS("J:/Project/Evaluation/GF/resource_tracking/_other_data_sources/multi_country/2019-2020_synthesis/all_modules.rds")
source("C:/Users/elineb/Documents/gf/resource_tracking/analysis/graphing_functions.r")
#--------------------
# SO1 analyses
#--------------------
save_loc = "J:/Project/Evaluation/GF/resource_tracking/visualizations/deliverables/_Synthesis 2019/SO1/"

# Make a general graph of cumulative country-level absorption 
plot_data = all_modules[, .(budget=sum(cumulative_budget, na.rm=T), expenditure=sum(cumulative_expenditure, na.rm=T)), by='loc_name']
plot_data[, label:=paste0(dollar(expenditure), " (", round((expenditure/budget)*100, 1), "%)")]
plot_data = melt(plot_data, id.vars=c('loc_name', 'label'))
plot_data[variable=="budget", label:=""]
plot_data[variable=="budget", variable:="Budget"]
plot_data[variable=="expenditure", variable:="Expenditure"]

p = ggplot(plot_data, aes(x=loc_name, y=value, fill=variable, label=label)) + 
  geom_bar(stat="identity", position="identity") + 
  geom_text(hjust=0) + 
  theme_bw(base_size=16) + 
  coord_flip() + 
  scale_y_continuous(labels=scales::dollar) + 
  labs(title="Absorption for PCE countries", subtitle="January 2018-June 2019", x="Country", y="Budget/Expenditure (USD)", fill="")
ggsave(paste0(save_loc, "absorption_summary.png"), p, height=8, width=11)

# Want a module-level visual for all of the countries combined. 
# Try this two ways. One that shows simply how they're performing in terms of absorptive capacity. 
# Then, one that scales to the average absorption level of the grant. 
plot_data = all_modules[, .(budget=sum(cumulative_budget), expenditure=sum(cumulative_expenditure)), by=c('abbrev_mod', 'loc_name')]
# Make disease label. 
all_mods = readRDS("J:/Project/Evaluation/GF/resource_tracking/modular_framework_mapping/all_interventions.rds")
setnames(all_mods, c('abbrev_mod_eng'), c('abbrev_mod'))
all_mods = unique(all_mods[, .(abbrev_mod, disease)])
all_mods = all_mods[!abbrev_mod%in%c('Program mgmt', 'Unspecified', 'TB/HIV')] # Drop these three so as not to expand dataset. 
plot_data = merge(plot_data, all_mods, by=c('abbrev_mod'), all.x=T)

# Fix diseases so that there's just one disease per module. 
plot_data[abbrev_mod=="Program mgmt", disease:="all"]
plot_data[abbrev_mod=="Unspecified", disease:="all"]
plot_data[abbrev_mod=="TB/HIV", disease:="hiv"]

# Make sure that there's just one module per disease. 
check = unique(plot_data[, .(abbrev_mod, disease)])
check[, dup:=.N, by='abbrev_mod']
check = check[dup!=1]
stopifnot(nrow(check)==0)
stopifnot(nrow(plot_data[is.na(disease)])==0)

#Collapse data 
plot_data[, absorption:=round((expenditure/budget)*100, 1)]
plot_data[absorption>=75.0, performance:="Excellent (>75%)"]
plot_data[absorption>=50.0 & absorption<75.0, performance:="Average (50-75%)"]
plot_data[absorption<50.0, performance:="Poor (<50%)"]
print(plot_data[is.na(performance)]) #Visual review. 
plot_data[is.na(performance), performance:="Data Unavailable"] # Verify that this is always true. 
plot_data[, num_countries_per_category:=.N, by=c('abbrev_mod', 'disease', 'performance')]
plot_data = unique(plot_data[, .(abbrev_mod, performance, num_countries_per_category, disease)])

# ***** DROPPING AREAS WHERE DATA IS UNAVAILABLE FOR NOW 
plot_data = plot_data[performance!="Data Unavailable"]

# Calculate the absolute difference to use as a sorting variable. 
# merge_data = dcast(plot_data, abbrev_mod+disease~performance, value.var='num__per_category')
# merge_data[is.na(`At or above target`), `At or above target`:=0]
# merge_data[is.na(`Below target`), `Below target`:=0]
# 
# merge_data[, difference:=`At or above target`-`Below target`]
# merge_data = merge_data[, .(abbrev_mod, disease, difference)]
# plot_data = merge(plot_data, merge_data, by=c('abbrev_mod', 'disease'))
# 
# # Count the total # of countries reporting, and make 'below target' values negative. 
# plot_data[, total_countries_reporting:=sum(num_mods_per_category), by=c('abbrev_mod')]
# plot_data[, label:=num_mods_per_category]
# plot_data[performance=="Below target", num_mods_per_category:=-num_mods_per_category]
# 
# # When you sort by difference below, account for disease in the calculation as well. 
# plot_data[disease=="hiv", difference:=difference+1000]
# plot_data[disease=="tb", difference:=difference+500]
# plot_data[disease=="malaria", difference:=difference+100]

# Make it easier to see where the categories are
plot_data[disease=="hiv", abbrev_mod:=paste0("HIV: ", abbrev_mod)]
plot_data[disease=="tb", abbrev_mod:=paste0("TB: ", abbrev_mod)]
plot_data[disease=="malaria", abbrev_mod:=paste0("Malaria: ", abbrev_mod)]
plot_data[disease=="rssh", abbrev_mod:=paste0("RSSH: ", abbrev_mod)]

plot_data[, total_countries_reporting:=sum(num_countries_per_category), by='abbrev_mod']
plot_data[, y_proportion:=num_countries_per_category/total_countries_reporting]
plot_data[, label:=as.character(num_countries_per_category)]

plot_data$performance <- factor(plot_data$performance, levels=c("Excellent (>75%)", "Average (50-75%)", "Poor (<50%)"))

# Try a bimodal distribution. 
p = ggplot(plot_data, aes(x=abbrev_mod, y=y_proportion, fill=performance, label=label)) + 
  geom_bar(stat="identity", position="stack") + 
  geom_text(size=4, position = position_stack(vjust=0.5)) + 
  theme_bw(base_size=16) + 
  coord_flip() + 
  scale_fill_brewer(palette=c('RdYlGn'), direction=-1) + 
  theme(axis.text.x=element_blank()) + 
  labs(title="Absorption by module for PCE countries", subtitle="January 2018-June 2019", x="Module",
       y="Number of countries with this performance rating", fill="Meeting absorptive\ncapacity target (75%)?", 
       caption="Numbers shown out of 8 possible countries,\nnot including Guatemala data and\nincluding CAM/MYN regional grant")

ggsave(paste0(save_loc, "absorption_by_mod1.png"), p, height=8, width=11)

  
  
#--------------------
# SO3 analyses
#--------------------
save_loc = "J:/Project/Evaluation/GF/resource_tracking/visualizations/deliverables/_Synthesis 2019/SO3/"

# Figure of achievement on HIV/TB performance indicators related to KVPs/gender/HR

# Figure of achievement on malaria performance indicators related to pregnant women, and any other available KPs

# Remake percent of HIV budgets devoted to AGYW, HR, and KP modules - distinguish the three. 
kp_mods = c("Prevention programs for men who have sex with men",  "Prevention programs for sex workers and their clients", 
            "Prevention programs for people who inject drugs and their partners", "Prevention programs for transgender people", 
            "Comprehensive programs for people in prisons and other closed settings", "Prevention programs for other vulnerable populations")

plot_data = all_modules[grant_disease%in%c('hiv/tb', 'hiv')]
plot_data[gf_module%in%kp_mods, category:="Key populations"]
plot_data[gf_module=="Programs to reduce human rights-related barriers to HIV services", category:="Human rights"]
plot_data[gf_module=="Prevention programs for adolescents and youth, in and out of school", category:="AGYW"]
plot_data[is.na(category), category:="Other"]

plot_data = plot_data[, .(budget=sum(cumulative_budget, na.rm=T), expenditure=sum(cumulative_expenditure, na.rm=T)), by=c('category', 'loc_name')]
plot_data[, label:=paste0(dollar(expenditure), " (", round((expenditure/budget)*100, 1), "%)")] #Add label that shows expenditure amount and absorption percentage. 
plot_data = melt(plot_data, id.vars=c('category', 'loc_name', 'label'))
plot_data[variable=="budget", label:=""]
plot_data[variable=="budget", variable:="Budget"]
plot_data[variable=="expenditure", variable:="Expenditure"]

p = ggplot(plot_data, aes(x=loc_name, y=value, fill=variable, label=label)) + 
  geom_bar(stat="identity", position="identity") + 
  geom_text(hjust=0) + 
  theme_bw(base_size=16) + 
  coord_flip() + 
  facet_wrap(~category) + 
  theme(axis.text.x=element_text(angle=20, vjust=0.5)) + 
  scale_y_continuous(labels=scales::dollar) + 
  labs(title="Absorption for KP, HR, and AGYW", subtitle="January 2018-June 2019", x="Country", y="Budget/Expenditure (USD)", fill="")
ggsave(paste0(save_loc, "absorption_kp_hr_agyw.png"), p, height=8, width=11)

# Remake percent of TB budgets devoted to KP modules - distinguish the three. 
plot_data = copy(all_modules) 
plot_data[grepl("Key populations", gf_intervention), tb_kp:=TRUE]
# Fill in interventions table 
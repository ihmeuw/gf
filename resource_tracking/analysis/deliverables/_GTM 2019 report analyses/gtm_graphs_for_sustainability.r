#------------------------------------------
# Specific graphs for Sustainability synthesis presentation - Guatemala 
# Updated by Emily Linebarger November 2019 
#------------------------------------------

rm(list=ls()) 

library(data.table) 
library(ggplot2) 
library(RColorBrewer) 
library(scales)

options(scipen=100)

source("C:/Users/elineb/Documents/gf/resource_tracking/analysis/graphing_functions.r")

absorption = readRDS("C:/Users/elineb/Box Sync/Global Fund Files/GTM/prepped_data/absorption_gtm.rds")
save_loc = "J:/Project/Evaluation/GF/resource_tracking/visualizations/deliverables/_GTM 2019 annual report/"

current_periods = c('2018-2018', '2019-2021', '2018-2020')
absorption = absorption[grant_period%in%current_periods]

all_mods = readRDS("J:/Project/Evaluation/GF/resource_tracking/modular_framework_mapping/all_interventions.rds")
setnames(all_mods, c('module_eng', 'intervention_eng', 'abbrev_mod_eng', 'abbrev_int_eng'), c('gf_module', 'gf_intervention', 'abbrev_mod', 'abbrev_int'))
all_mods = unique(all_mods[, .(gf_module, gf_intervention, disease, abbrev_mod, abbrev_int)])
absorption = merge(absorption, all_mods, by=c('gf_module', 'gf_intervention', 'disease'), allow.cartesian=TRUE)

# Show RSSH 
plot_data = absorption[disease=="rssh", .(budget=sum(budget, na.rm=T), expenditure=sum(expenditure, na.rm=T)), 
                                          by=c('grant', 'grant_period', 'semester', 'abbrev_mod')]
plot_data[, absorption:=round((expenditure/budget)*100, 1)]
plot_data[, concat:=paste0(grant, ", ", grant_period, ", ", semester)]
plot_data = plot_data[!(grant=="GTM-M-MSPAS" & grant_period=="2018-2018")]

melt = plot_data[, .(concat, grant, abbrev_mod, absorption, budget, expenditure)]
melt = melt(melt, id.vars=c('concat', 'grant', 'abbrev_mod', 'absorption'))
melt[variable=="budget", label:=""] #Don't display the expenditure amount on the budget bar. 
melt[variable=="expenditure", label:=paste0(dollar(value), " (", absorption, "%)")]
melt[variable=="budget", variable:="Budget"]
melt[variable=="expenditure", variable:="Expenditure"]


p = ggplot(melt, aes(x=abbrev_mod, y=value, fill=variable, label=label)) + 
  geom_bar(stat="identity") + 
  geom_text(hjust=0) + 
  theme_bw(base_size=16) + 
  facet_wrap(~grant) + 
  coord_flip() + 
  theme(axis.text.x=element_text(angle=30, vjust=0.5)) + 
  scale_y_continuous(labels=scales::dollar) + 
  labs(title=paste0("RSSH absorption for most recent PUDRs\nin Guatemala"),
       x="Module", y="Absorption (%)", fill="")

ggsave(paste0(save_loc, "RSSH_by_pudr.png"), p, height=8, width=11)


gtm_tb = funding_landscape("ribbon", "gtm", "tb", 2010, 2017, includeGHE=TRUE)
ggsave(paste0(save_loc, "tb_funding_landscape.png"), gtm_tb, height=8, width=11)
gtm_hiv = funding_landscape("ribbon", "gtm", "hiv", 2010, 2017, includeGHE=TRUE)
ggsave(paste0(save_loc, "hiv_funding_landscape.png"), gtm_hiv, height=8, width=11)
gtm_mal = funding_landscape("ribbon", "gtm", "malaria", 2010, 2017, includeGHE=TRUE)
ggsave(paste0(save_loc, "malaria_funding_landscape.png"), gtm_mal, height=8, width=11)


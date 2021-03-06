# Graphs for Uganda report 
# Emily Linebarger 11/19/2019 
# updated by Emily Linebarger 3/20/2020 

rm(list=ls())
library(data.table) 
library(gridExtra)
library(ggplot2)

source("C:/Users/elineb/Documents/gf/resource_tracking/analysis/graphing_functions.r")
save_loc = "J:/Project/Evaluation/GF/resource_tracking/visualizations/deliverables/_UGA 2019 annual report/"
#absorption = readRDS("C:/Users/elineb/Box Sync/Global Fund Files/UGA/prepped_data/absorption_uga.rds")
absorption = fread("C:/Users/elineb/Box Sync/Global Fund Files/UGA/prepped_data/cumulative_absorption_uga.csv")
all_mods = readRDS("J:/Project/Evaluation/GF/resource_tracking/modular_framework_mapping/archive/all_interventions.rds")
all_mods = unique(all_mods[, .(module_eng, abbrev_mod_eng)])
setnames(all_mods, c('module_eng', 'abbrev_mod_eng'), c('gf_module', 'abbrev_mod'))
absorption = merge(absorption, all_mods, all.x=T, by='gf_module')
stopifnot(nrow(absorption[is.na(abbrev_mod)])==0)

# Malaria
mofped_cumulative = absorption[grant%in%c('UGA-M-MoFPED', 'UGA-M-TASO'), .(budget=sum(cumulative_budget, na.rm=T), expenditure=sum(cumulative_expenditure, na.rm=T)), 
                               by=c('abbrev_mod', 'grant')]
p1 = budget_exp_bar(mofped_cumulative, xVar='abbrev_mod', facetVar='grant', trimAbsorption=TRUE, angleText=TRUE, 
                    altTitle="Absorption by module\nUGA-M-MoFPED and UGA-M-TASO", altSubtitle="January 2018-June 2019", baseSize=18, labelSize=6)

ggsave(paste0(save_loc, "malaria_absorption.png"), p1, height=8, width=14)

# What was absorption for AGYW interventions in S1 2019, facet-wrapped by grant? 
p = absorption[gf_module=="Prevention programs for adolescents and youth, in and out of school" & grant_period=="2018-2020" & start_date=="2019-01-01", 
               .(budget=sum(budget), expenditure=sum(expenditure)), by=c('grant', 'gf_intervention')]

#HIV 
mofped_cumulative = get_cumulative_absorption(byVars = c('abbrev_mod', 'grant'), grantSubset=c("UGA-H-MoFPED", "UGA-C-TASO"), currency="USD")
p1 = budget_exp_bar(mofped_cumulative, xVar='abbrev_mod', facetVar='grant', trimAbsorption=TRUE, 
                    altTitle="Absorption by module\nUGA-H-MoFPED and UGA-C-TASO", altSubtitle="January 2018-June 2019")

ggsave(paste0(save_loc, "hiv_absorption.png"), p1, height=8, width=14)

# What was absorption for AGYW interventions in S1 2019, facet-wrapped by grant? 
plot_data = absorption[gf_module=="Prevention programs for adolescents and youth, in and out of school" & grant_period=="2018-2020" & start_date=="2019-01-01", 
               .(budget=sum(budget), expenditure=sum(expenditure)), by=c('grant', 'gf_intervention')]
plot_data[, absorption:=round((expenditure/budget)*100, 1)]

write.csv(plot_data, "C:/Users/elineb/Desktop/uga_agyw_ints.csv", row.names=F)
p = budget_exp_bar(plot_data, xVar=c('gf_intervention'), facetVar='grant')

# TB 
mofped_cumulative = get_cumulative_absorption(byVars = c('abbrev_mod', 'grant'), grantSubset=c("UGA-T-MoFPED"), 
                                              currency="USD")
p1 = budget_exp_bar(mofped_cumulative, xVar='abbrev_mod', trimAbsorption=TRUE, yScaleMax=15000000,
                    altTitle="Absorption by module for UGA-T-MoFPED", altSubtitle="January 2018-June 2019")

ggsave(paste0(save_loc, "tb_absorption.png"), p1, height=8, width=11)

# Cumulative RSSH absorption 
# cumulative_rssh = get_cumulative_absorption(byVars=c('abbrev_mod', 'abbrev_int'), countrySubset="UGA", diseaseSubset="rssh", currency="USD")
# cumulative_rssh[, absorption:=(expenditure/budget)*100]
# cumulative_rssh[, label1:=paste0(round(absorption, 1), "%")]
# cumulative_rssh[absorption>200, absorption:=200] # Cap absorption at 200%
# cumulative_rssh[, label2:=paste0(abbrev_mod)]
# 
# #Shorten a few intervention names. 
# cumulative_rssh[abbrev_int=="Supportive policy and programmatic environment", abbrev_int:="Supportive policy environment"]
# 
# #Sort the graph by module 
# cumulative_rssh = cumulative_rssh[order(abbrev_mod, abbrev_int)]
# cumulative_rssh[, module_num:=.GRP, by='abbrev_mod']
# cumulative_rssh[, int_num:=.GRP, by=c('abbrev_mod', 'abbrev_int')]
# cumulative_rssh[, rssh_factor:=paste0(module_num, int_num)]
# cumulative_rssh[, rssh_factor:=as.numeric(rssh_factor)]
# 
# cumulative_rssh$int_factor = factor(unique(cumulative_rssh$rssh_factor), levels=unique(cumulative_rssh$rssh_factor), labels=unique(cumulative_rssh$abbrev_int))

# p = ggplot(cumulative_rssh, aes(x=int_factor, y=absorption, fill=abbrev_mod, label=label1))+ 
#   geom_bar(stat="identity", position="dodge") + 
#   geom_text(hjust=-0.5, size=4) + 
#   theme_bw(base_size=14) + 
#   guides(fill=guide_legend(reverse=TRUE)) + 
#   scale_fill_brewer(palette="Set2") + 
#   coord_flip() + 
#   scale_y_continuous(limits=c(0, 220)) + 
#   labs(title="Cumulative RSSH absorption by intervention, all grants", subtitle="Jan. 2018-June 2019\nInterventions grouped by module", x="Intervention", y="Absorption (%)",   fill="Module", caption="*Max bar height set at 200%")
# ggsave(paste0(save_loc, "cumulative_rssh.png"), p, height=8, width=11)

#cumulative_rssh = get_cumulative_absorption(byVars=c('abbrev_mod'), countrySubset="UGA", diseaseSubset="rssh", currency="USD")
cumulative_rssh = absorption[disease=="rssh", .(budget=sum(cumulative_budget, na.rm=T), expenditure=sum(cumulative_expenditure, na.rm=T)), by=c('abbrev_mod')]
p = budget_exp_bar(dt=cumulative_rssh, xVar='abbrev_mod', altTitle="Absorption for RSSH modules", altSubtitle="January 2018-June 2019", baseSize=20)
ggsave(paste0(save_loc, "cumulative_rssh.png"), p, height=8, width=11)
#------------------------------
# GENERAL 
# ------------------------------

# Show cumulative absorption by grant 
all_absorption =  absorption[, .(budget=sum(cumulative_budget, na.rm=T), expenditure=sum(cumulative_expenditure, na.rm=T)), by=c('grant')]
p = budget_exp_bar(dt = all_absorption, xVar='grant', altTitle="Absorption for 2018-2020 grants", altSubtitle="January 2018-June 2019", baseSize=20)
ggsave(paste0(save_loc, "absorption_overview.png"), p, height=8, width=11)
# Are there any broad patterns in absorption by grant, PR type, or commoditization level? 

cost_categories = readRDS("C:/Users/elineb/Box Sync/Global Fund Files/tableau_data/all_cost_categories.rds")
# Add in a 'parent' cost category - move this into general prep code. 
cost_categories[, parent_code:=tstrsplit(cleaned_cost_category, "\\.", keep=1)]
parent_categories = fread("C:/Users/elineb/Desktop/parent_cost_categories.csv")
cost_categories = merge(cost_categories, parent_categories, by='parent_code', all.x=T)
stopifnot(nrow(cost_categories[is.na(parent_category)])==0)

# Collapse data to plot 
collapse = cost_categories[loc_name=="uga" & grant_period=="2018-2020", 
                           .(cumulative_budget=sum(cumulative_budget), cumulative_expenditure=sum(cumulative_expenditure)), by=c('grant', 'parent_category', 'parent_code')]
collapse[, label:=paste0(dollar(cumulative_expenditure), " (", round((cumulative_expenditure/cumulative_budget)*100, 1), "%)")]
collapse[parent_category=="Unspecified", parent_code:='14']
collapse[, parent_code:=as.numeric(parent_code)]

collapse = melt(collapse, id.vars=c('grant', 'parent_category', 'parent_code', 'label'))
collapse[variable=="cumulative_budget", label:=""]
pdf("C:/Users/elineb/Desktop/uga_commodity_absorption.pdf", height=8, width=12)
for (g in unique(collapse$grant)){
  subset = collapse[grant==g]
  p = ggplot(subset, aes(x=reorder(parent_category, -parent_code), y=value, fill=variable, label=label)) + 
    geom_bar(stat="identity", position="identity") + 
    geom_text(hjust=0) + 
    theme_bw(base_size=16) + 
    theme(axis.text.x=element_text(size=12)) + 
    coord_flip() + 
    scale_y_continuous(labels=scales::dollar) + 
    labs(title=paste0("Cost category absorption for ", g), x="", y="Budget/Expenditure (USD", fill="")
  print(p) 
}
dev.off() 


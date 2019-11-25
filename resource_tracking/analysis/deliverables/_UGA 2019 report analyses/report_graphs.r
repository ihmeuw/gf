# Graphs for Uganda report 
# Emily Linebarger 11/19/2019 

rm(list=ls())
library(data.table) 
library(gridExtra)
library(ggplot2)

source("C:/Users/elineb/Documents/gf/resource_tracking/analysis/graphing_functions.r")
save_loc = "J:/Project/Evaluation/GF/resource_tracking/visualizations/deliverables/_UGA 2019 annual report/"

# Malaria
mofped_cumulative = get_cumulative_absorption(byVars = c('abbrev_mod'), grantSubset="UGA-M-MoFPED", currency="USD")
p1 = budget_exp_bar(mofped_cumulative, trimAbsorption=TRUE, yScaleMax=200, angleText=TRUE, 
                    altTitle="Absorption by module\nfor UGA-M-MoFPED", altSubtitle="January 2018-June 2019")

taso_cumulative = get_cumulative_absorption(byVars = c('abbrev_mod'), grantSubset="UGA-M-TASO", currency="USD")
p2 = budget_exp_bar(taso_cumulative, trimAbsorption=TRUE, yScaleMax=200, angleText=TRUE, 
                    altTitle="Absorption by module\nfor UGA-M-TASO", altSubtitle="January 2018-June 2019")

ggsave(paste0(save_loc, "malaria_absorption.png"), grid.arrange(p1, p2, ncol=2, nrow=1), height=8, width=15)

#HIV 
mofped_cumulative = get_cumulative_absorption(byVars = c('abbrev_mod'), grantSubset="UGA-H-MoFPED", currency="USD")
p1 = budget_exp_bar(mofped_cumulative, trimAbsorption=TRUE, yScaleMax=200, angleText=TRUE, 
                    altTitle="Absorption by module\nfor UGA-H-MoFPED", altSubtitle="January 2018-June 2019")

taso_cumulative = get_cumulative_absorption(byVars = c('abbrev_mod'), grantSubset="UGA-C-TASO", currency="USD")
p2 = budget_exp_bar(taso_cumulative, trimAbsorption=TRUE, yScaleMax=200, angleText=TRUE, 
                    altTitle="Absorption by module\nfor UGA-C-TASO", altSubtitle="January 2018-June 2019")

ggsave(paste0(save_loc, "hiv_absorption.png"), grid.arrange(p1, p2, ncol=2, nrow=1), height=8, width=15)

# TB 
mofped_cumulative = get_cumulative_absorption(byVars = c('abbrev_mod'), grantSubset="UGA-T-MoFPED", currency="USD")
p1 = budget_exp_bar(mofped_cumulative, trimAbsorption=TRUE, yScaleMax=200, angleText=TRUE, 
                    altTitle="Absorption by module\nfor UGA-T-MoFPED", altSubtitle="January 2018-June 2019")

taso_cumulative = get_cumulative_absorption(byVars = c('abbrev_mod'), grantSubset="UGA-C-TASO", currency="USD")
p2 = budget_exp_bar(taso_cumulative, trimAbsorption=TRUE, yScaleMax=200, angleText=TRUE, 
                    altTitle="Absorption by module\nfor UGA-C-TASO", altSubtitle="January 2018-June 2019")

ggsave(paste0(save_loc, "tb_absorption.png"), grid.arrange(p1, p2, ncol=2, nrow=1), height=8, width=15)

# Cumulative RSSH absorption 
cumulative_rssh = get_cumulative_absorption(byVars=c('abbrev_mod', 'abbrev_int'), countrySubset="UGA", diseaseSubset="rssh", currency="USD")
cumulative_rssh[, absorption:=(expenditure/budget)*100]
cumulative_rssh[, label1:=paste0(round(absorption, 1), "%")]
cumulative_rssh[absorption>200, absorption:=200] # Cap absorption at 200%
cumulative_rssh[, label2:=paste0(abbrev_mod)]

#Shorten a few intervention names. 
cumulative_rssh[abbrev_int=="Supportive policy and programmatic environment", abbrev_int:="Supportive policy environment"]

#Sort the graph by module 
cumulative_rssh = cumulative_rssh[order(abbrev_mod, abbrev_int)]
cumulative_rssh[, module_num:=.GRP, by='abbrev_mod']
cumulative_rssh[, int_num:=.GRP, by=c('abbrev_mod', 'abbrev_int')]
cumulative_rssh[, rssh_factor:=paste0(module_num, int_num)]
cumulative_rssh[, rssh_factor:=as.numeric(rssh_factor)]

cumulative_rssh$int_factor = factor(unique(cumulative_rssh$rssh_factor), levels=unique(cumulative_rssh$rssh_factor), labels=unique(cumulative_rssh$abbrev_int))

p = ggplot(cumulative_rssh, aes(x=int_factor, y=absorption, fill=abbrev_mod, label=label1))+ 
  geom_bar(stat="identity", position="dodge") + 
  geom_text(hjust=-0.5, size=4) + 
  theme_bw(base_size=14) + 
  guides(fill=guide_legend(reverse=TRUE)) + 
  scale_fill_brewer(palette="Set2") + 
  coord_flip() + 
  scale_y_continuous(limits=c(0, 220)) + 
  labs(title="Cumulative RSSH absorption by intervention, all grants", subtitle="Jan. 2018-June 2019\nInterventions grouped by module", x="Intervention", y="Absorption (%)",   fill="Module", caption="*Max bar height set at 200%")
ggsave(paste0(save_loc, "cumulative_rssh.png"), p, height=8, width=11)

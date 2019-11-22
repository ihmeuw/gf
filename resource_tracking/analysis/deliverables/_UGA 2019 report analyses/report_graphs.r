# Graphs for Uganda report 
# Emily Linebarger 11/19/2019 

rm(list=ls())
library(data.table) 
library(gridExtra)

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
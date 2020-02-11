# Graphs for Uganda February TERG slides  
# Emily Linebarger 1/23/2020

rm(list=ls())
library(data.table) 
library(gridExtra)
library(ggplot2)

source("C:/Users/elineb/Documents/gf/resource_tracking/analysis/graphing_functions.r")
save_loc = "J:/Project/Evaluation/GF/resource_tracking/visualizations/deliverables/_UGA 2019 annual report/"
absorption = readRDS("C:/Users/elineb/Box Sync/Global Fund Files/UGA/prepped_data/absorption_uga.rds")

# Absorption by grant 
cumul_abs_grant = get_cumulative_absorption(byVars='grant', countrySubset='UGA')
p1 = budget_exp_bar(cumul_abs_grant, xVar='grant', altTitle="Cumulative from start of grant", altSubtitle="January 2018-June 2019", baseSize=18, angleText=TRUE)

recent_abs_grant = absorption[semester=="Semester 3" & grant_period=="2018-2020", .(budget=sum(budget, na.rm=T), expenditure=sum(expenditure, na.rm=T)), 
                              by=c('grant')]
p2 = budget_exp_bar(recent_abs_grant, xVar='grant', altTitle="Most recent semester reporting", altSubtitle="January 2019-June 2019", baseSize=18, angleText=TRUE)
ggsave(paste0(save_loc, "recent_absorption_by_grant.png"), grid.arrange(p2, p1, nrow=1, ncol=2), height=8, width=18)

# We're seeing that the absorption rate has dropped for M-MoFPED  - is this right? 
# Compare absorption rates across semesters. 
abs_semesters = absorption[grant_period=="2018-2020", .(budget=sum(budget, na.rm=T), expenditure=sum(expenditure, na.rm=T)), 
                           by=c('grant', 'semester')][order(grant, semester)]
abs_semesters[, absorption:=round((expenditure/budget)*100, 1)]

# Compare the average absorption rate across all grants cumulatively vs. S3. 
cumul_abs = get_cumulative_absorption(byVars='loc_name', countrySubset='UGA')
recent_abs = absorption[semester=="Semester 3" & grant_period=="2018-2020", .(budget=sum(budget, na.rm=T), expenditure=sum(expenditure, na.rm=T))]
recent_abs[, absorption:=round((expenditure/budget)*100, 1)]

#-------------------
# RSSH absorption
#-------------------
cumul_rssh_abs = get_cumulative_absorption(byVars='abbrev_mod', diseaseSubset='rssh', countrySubset='UGA')
rssh = budget_exp_bar(cumul_rssh_abs, baseSize=18)
ggsave(paste0(save_loc, "rssh_by_mod.png"), rssh, height=8, width=11)

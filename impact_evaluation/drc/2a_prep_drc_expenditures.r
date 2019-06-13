#------------------------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Creating a time-series of DRC expenditure data with the 
#   fewest reporting gaps possible. 
# DATE: June 13, 2019
#------------------------------------------------------------

#PROCESS: 
# 1. Take expenditures from final expenditures file. 
# 2. Fill gaps where necessary for COD-M-MOH (Q3-Q4 2017), COD-M-SANRU (Q1-Q4 2017), and COD-M-PSI (Q1-Q4 2017) by
#   taking budget data, and applying the last available semester's absorption rate to it. 
#   This will only be possible for COD-M-SANRU and COD-M-PSI, because we don't have budget data for the missing period for COD-M-MOH. 
#   All methodological decisions made by David Phillips 6/13/19.


#Read in data 
final_expenditures = readRDS("J:/Project/Evaluation/GF/resource_tracking/_gf_files_gos/combined_prepped_data/final_expenditures.rds")
final_budgets = readRDS("J:/Project/Evaluation/GF/resource_tracking/_gf_files_gos/combined_prepped_data/final_budgets.rds")
gos_data = readRDS("J:/Project/Evaluation/GF/resource_tracking/_gf_files_gos/gos/prepped_data/prepped_gos_data.rds")

#Subset to the data you want - just malaria grants
cod_expenditures = final_expenditures[loc_name=="cod" & grant_disease=="malaria"]
unique(cod_expenditures[start_date>="2014-01-01", .(grant, start_date)])

#Calculate absorption rate for the quarters you're going to fill - just the last semester of 2016 for both grants. 
#Do one by module where possible
absorption_by_int = gos_data[grant%in%c("COD-M-PSI", "COD-M-SANRU") & quarter%in%c(3, 4) & year==2016, 
                              .(budget=sum(budget, na.rm=T), expenditure=sum(expenditure, na.rm=T)), by=c('grant', 'gf_module', 'gf_intervention')]
absorption_by_int[, s2_int_absorption_2016:=(expenditure/budget)] #FLAG THIS HIGH absorption_by_int RATE FOR DAVID. EMILY. 
absorption_by_int = absorption_by_int[, .(grant, gf_module, gf_intervention, s2_int_absorption_2016)]

#And one in general by module where interventions are not represented in old data. 
absorption_by_mod = gos_data[grant%in%c("COD-M-PSI", "COD-M-SANRU") & quarter%in%c(3, 4) & year==2016, 
                             .(budget=sum(budget, na.rm=T), expenditure=sum(expenditure, na.rm=T)), by=c('grant', 'gf_module')]
absorption_by_mod[, s2_mod_absorption_2016:=(expenditure/budget)] #FLAG THIS HIGH absorption_by_mod RATE FOR DAVID. EMILY. 
absorption_by_mod = absorption_by_mod[, .(grant, gf_module, s2_mod_absorption_2016)]


#Now, pull the budget data for these quarters and multiply by absorption_by_int. 
budget_fill = final_budgets[grant%in%c("COD-M-PSI", "COD-M-SANRU") & year==2017, 
                      .(grant, disease, grant_period, data_source, loc_name, start_date, code, gf_module, gf_intervention, budget)]


#Merge the budget fill and absorption_by_int datasets together, and multiply budget by absorption_by_int to get "expected" expenditure. 
budget_fill = merge(budget_fill, absorption_by_int, by=c('grant', 'gf_module', 'gf_intervention'), all.x=T)
budget_fill = merge(budget_fill, absorption_by_mod, by=c('grant', 'gf_module'), all.x=T)
stopifnot(nrow(budget_fill[is.na(s2_mod_absorption_2016)])==0) #Do you have at least one absorption value you can multiply by? 


#Where possible, adjust budget at the intervention level, and if that's not possible adjust by the module level. 
budget_fill[!is.na(s2_int_absorption_2016), expenditure:=budget*s2_int_absorption_2016]
budget_fill[is.na(s2_int_absorption_2016), expenditure:=budget*s2_mod_absorption_2016]

budget_fill = budget_fill[, -c('budget', 's2_int_absorption_2016', 's2_mod_absorption_2016')]
#Bind these files together to create final product. 
cod_expenditures = rbind(cod_expenditures, budget_fill, use.names=T, fill=T) 
#Visualize time series 
unique(cod_expenditures[start_date>="2015-01-01", .(grant, start_date)])

#SAVE DATA 
saveRDS(cod_expenditures, "J:/Project/Evaluation/GF/impact_evaluation/cod/prepped_data/cod_expenditures.rds")

#Make some graphs of the new data you have. 
ggplot(cod_expenditures[start_date>="2015-01-01"], aes(x=start_date, y=expenditure, color=gf_module))+
  geom_point()+
  theme_bw()+
  facet_wrap(~grant)

by_grant = cod_expenditures[, .(expenditure=sum(expenditure, na.rm=T)), by=c('grant', 'start_date')]
ggplot(by_grant[start_date>="2015-01-01"], aes(x=start_date, y=expenditure))+
  geom_point()+ geom_line() + 
  theme_bw()+
  facet_wrap(~grant)

toView = cod_expenditures[start_date>="2015-01-01", .(expenditure=sum(expenditure, na.rm=T)), 
                                                             by=c('grant', 'start_date')][order(grant, start_date)]
View(toView)

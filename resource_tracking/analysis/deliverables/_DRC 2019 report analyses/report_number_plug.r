#---------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Plug numbers into DRC 2019 annual report 
# Last updated 11/26/2019 
#---------------------------------------

rm(list=ls())
library(data.table)
source("C:/Users/elineb/Documents/gf/resource_tracking/analysis/graphing_functions.r")

absorption = readRDS("C:/Users/elineb/Box Sync/Global Fund Files/COD/prepped_data/absorption_cod.rds")
budgets=readRDS("C:/Users/elineb/Box Sync/Global Fund Files/COD/prepped_data/final_budgets.rds")
# We changed the final CORDAID budget version, so will need to re-run this data! 

# -------------------------------
# MALARIA 
#--------------------------------
# How much was allocated for community-based case management or community responses and systems (for both malaria grants?)
budgets[grant_disease=="malaria" & grant_period=="2018-2020" & (gf_module=="Community responses and systems" | 
                                                                  gf_intervention=="Integrated community case management (iCCM)"), 
        .(budget=sum(budget, na.rm=T)), by='file_name']

budgets[grant_disease=="malaria" & grant_period=="2018-2020" & (gf_module=="Community responses and systems" | 
                                                                  gf_intervention=="Integrated community case management (iCCM)"), 
        .(budget=dollar(sum(budget, na.rm=T))), by='year']

# What's been the overall absorption for S1 2019 for the two malaria grants? 
absorption[grant_period=="2018-2020" & grant_disease=="malaria" & semester=="Semester 3", .(b=sum(budget, na.rm=T), e=sum(expenditure, na.rm=T))]

# What's been the absorption for community based care in 2018? 
absorption[grant_period=="2018-2020" & grant_disease=="malaria" & semester=="Semester 1-2" & (gf_intervention=="Integrated community case management (iCCM)"),
           .(b=sum(budget, na.rm=T), e=sum(expenditure, na.rm=T))]

# What's the total budgeted for facility-based treatment, and integrated service delivery? 
budgets[grant_period=="2018-2020" & grant_disease=="malaria" & gf_intervention=="Facility-based treatment", .(b=sum(budget, na.rm=T))]
budgets[grant_period=="2018-2020" & grant_disease=="malaria" & gf_module=="Integrated service delivery and quality improvement", .(b=dollar(sum(budget, na.rm=T)))]
budgets[grant_period=="2018-2020" & gf_module=="Integrated service delivery and quality improvement", .(b=dollar(sum(budget, na.rm=T)))]


#----------------------------------
# RSSH
#----------------------------------

cumulative_rssh = get_cumulative_absorption(byVars=c('abbrev_mod'), diseaseSubset='rssh', countrySubset="COD")

cumulative_rssh1 = get_cumulative_absorption(byVars=c('abbrev_int'), moduleSubset="Health management information system and monitoring and evaluation", countrySubset="COD")


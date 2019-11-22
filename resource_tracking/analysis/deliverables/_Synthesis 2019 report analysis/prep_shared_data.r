# Prep data for Synthesis 2019-2020 to share with EHG 
# Emily Linebarger, last udpated November 22, 2019 


rm(list=ls()) 
library(data.table) 

absorption_mi = readRDS("C:/Users/elineb/Box Sync/Global Fund Files/tableau_data/absorption.rds")
absorption_cc = readRDS("C:/Users/elineb/Box Sync/Global Fund Files/tableau_data/all_cost_categories.rds")
budgets = readRDS("C:/Users/elineb/Box Sync/Global Fund Files/tableau_data/final_budgets.rds")

absorption_mi = absorption_mi[, .(budget=sum(budget, na.rm))]
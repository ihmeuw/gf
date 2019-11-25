#--------------------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Print final cross-consortia synthesis report graphs
# DATE: Last updated November 25, 2019 
#---------------------------------------------------------

rm(list=ls()) 

library(data.table)
library(ggplot2)
library(readxl)

#Read in data 
ehg_data = data.table(read_xlsx("J:/Project/Evaluation/GF/resource_tracking/_other_data_sources/multi_country/ehg_2019_absorption_synthesis.xlsx"))

#Format data to be in the same format as ours 
names(ehg_data) = c('loc_name', 'grant', 'grant_period', 'semester', 'module_cost_category', 'intervention', 'cumulative_absorption', 
                    'cumulative_expenditure', 'cumulative_budget', 'original_approved_budget', 
                    'expenditure_incl_commitments', 'x1', 'x2')

ehg_data[grant_period=="2018-20", grant_period:="2018-2020"]

# Split out cost category columns 
ehg_data[grepl("[:digit:]", module_cost_category), is_cc:=TRUE]

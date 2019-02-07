#---------------------------------------------------------------
#AUTHOR: Emily Linebarger 
#DATE: November 2018 
#PURPOSE: Calculate total spent on Health Information Systems 
#         and RSSH for current DRC grants. 
#---------------------------------------------------------------

# ------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2)
# ------------------

# -----------------------------------------------------------------
# Files and directories
# -----------------------------------------------------------------
# root directory for input/average_absorptionput
dir = 'J:/Project/Evaluation/GF/resource_tracking/multi_country/'

# input data
inFile = paste0(dir, 'mapping/total_resource_tracking_data.csv')

#-------------------------------------------------------------------
# Subset to only the grants we want (DRC, 2018-2020 active only) 
#------------------------------------------------------------------
allData = fread(inFile)
cod = allData[country %in% "Congo (Democratic Republic)"]
cod = cod[grant_period == "2018-2020" & data_source == "fpm"]

print(unique(cod[, "grant_number"])) #Verify that these are the grants you want, you should have 5.
stopifnot(length(unique(cod$fileName))==5)

total_drc_budget <- sum(cod$budget)
print(total_drc_budget)

print(unique(cod[, module]))
#Generate a total budget number 
total_drc_budget <- sum(cod$budget)
#--------------------------
# Calculate HMIS Spending 
#--------------------------
hmis = cod[gf_module %in% "Health management information system and monitoring and evaluation"]
stopifnot(length(unique(hmis$module))==1) #verify gf_module and module are the same here

#Sum the budget for this module across the grants. 
hmis <- hmis[, .('budget' = sum(budget, na.rm = T)), by = c("gf_module", "grant_number")]
total_budget_hmis <- sum(hmis$budget)
print(total_budget_hmis)
hmis_percent <- total_budget_hmis/total_drc_budget

#--------------------------
# Calculate RSSH Spending 
#--------------------------
cod$short_code <- substring(cod$code, 1, 1)
rssh = cod[short_code == 'R']
print(unique(rssh[, module])) #Verify that these modules belong here. 
print(unique(rssh[, gf_module]))

#stopifnot(length(unique(rssh$module))==1) #verify gf_module and module are the same here

#Sum the budget for this module across the grants. 
rssh <- rssh[, .('budget' = sum(budget, na.rm = T)), by = c("module", "grant_number")]
total_budget_rssh <- sum(rssh$budget)
print(total_budget_rssh)
rssh_percent <- total_budget_rssh/total_drc_budget


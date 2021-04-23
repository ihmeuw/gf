#########################################################
# Audrey Batzel
# troubleshoot discrepancies in the 2S figures for DRC 

# clear
rm(list=ls())

# Set up
library(ggplot2)
library(data.table)
library(readxl)
library(scales)
library(grid)
library(lattice)
library(RColorBrewer)

# -------------------------------------------------------------------
# Files and directories
setwd('C:/local/gf/')

user=as.character(Sys.info()[7])

# input file
box = paste0("C:/Users/", user, "/Box Sync/Global Fund Files/")
inFile = paste0(box, 'tableau_data/budgetRevisions_with_frBudgets_activityLevel.csv')
inFile_2s_coded = paste0(box, '2s_data/prepped_2s_data_all_countries.csv')
# -------------------------------------------------------------------

# -------------------------------------------------------------------
# read in and sum to appropriate levels
# -------------------------------------------------------------------
prepped_dt = as.data.table(read.csv(paste0(box, '2s_data/prepped_2s_data_all_countries.csv')))
setnames(prepped_dt, 'budget', 'budget_2s')
check_2s_coded = prepped_dt[loc_name == 'DRC',.(budget_2s = sum(budget_2s, na.rm = TRUE)), by = .(loc_name, module, intervention, activity_description, version, cycle, grant_period)]
check_2s_coded_intervention = prepped_dt[loc_name == 'DRC',.(budget_2s = sum(budget_2s, na.rm = TRUE)), by = .(loc_name, module, intervention, version, cycle, grant_period)]
check_2s_coded_module = prepped_dt[loc_name == 'DRC',.(budget_2s = sum(budget_2s, na.rm = TRUE)), by = .(loc_name, module, version, cycle, grant_period)]

data = as.data.table(read.csv(inFile))
check_data = data[loc_name == 'DRC' & rssh==TRUE & budget_version %in% c('funding_request20', 'funding_request20_CT', 'approved'), .(budget=sum(budget, na.rm=TRUE)), by = .(loc_name, gf_module, gf_intervention, activity_description, grant_period, budget_version )]
check_data[ grant_period == '2018-2020', cycle := 'NFM2']
check_data[ grant_period == '2021-2023', cycle := 'NFM3']
check_data[ budget_version == 'approved', version := 'approved_budget']
check_data[ budget_version == 'funding_request20', version := 'funding_request']
check_data[ budget_version == 'funding_request20_CT', version := 'funding_request_CT']
setnames(check_data, 'budget', 'correct_budget')
setnames(check_data, 'gf_module', 'module')
setnames(check_data, 'gf_intervention', 'intervention')
check_data_module = check_data[, .(correct_budget=sum(correct_budget, na.rm=TRUE)), by = .(loc_name, module, version, cycle )]
check_data_intervention = check_data[, .(correct_budget=sum(correct_budget, na.rm=TRUE)), by = .(loc_name, module, intervention, version, cycle )]

x = merge(check_data_module, check_2s_coded_module, all = TRUE, by = c('loc_name', 'module', 'version', 'cycle'))
x[, discrepancy := round(correct_budget) - round(budget_2s)]
x[discrepancy > 5 | discrepancy < -5, check := 'flag']

y = merge(check_data_intervention, check_2s_coded_intervention, all = TRUE, by = c('loc_name', 'module', 'intervention', 'version', 'cycle'))
y[, discrepancy := round(correct_budget) - round(budget_2s)]
y[discrepancy > 5 | discrepancy < -5, check := 'flag']

check_2s_coded[, activity_description := trimws(activity_description)]
check_2s_coded[, activity_description := gsub('  ', ' ', activity_description)]
check_data[, activity_description := trimws(activity_description)]
check_data[, activity_description :=  gsub('  ', ' ', activity_description)]

z = merge(check_2s_coded, check_data, all = TRUE, by = c('loc_name', 'module', 'intervention', 'activity_description', 'version', 'cycle'))
z = z[intervention == 'Surveys']
z = z[, -c('grant_period.x', 'grant_period.y')]
z[, discrepancy := round(correct_budget) - round(budget_2s)]
z[discrepancy > 5 | discrepancy < -5, check := 'flag']

# -------------------------------------------------------------------


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
data_2s = as.data.table(read.csv(paste0(box, '2s_data/prepped_2s_data_all_countries.csv')))
setnames(data_2s, 'budget', 'budget_2s')
data_2s[loc_name == 'UGA', loc_name := 'Uganda']
data_2s[loc_name == 'SEN', loc_name := 'Senegal']
data_2s[loc_name == 'GTM', loc_name := 'Guatemala']
check_2s_coded = data_2s[,.(budget_2s = sum(budget_2s, na.rm = TRUE)), by = .(loc_name, module, intervention, activity_description, version, cycle, grant_period)]
# check_2s_coded_intervention = data_2s[,.(budget_2s = sum(budget_2s, na.rm = TRUE)), by = .(loc_name, module, intervention, version, cycle, grant_period)]
# check_2s_coded_module = data_2s[,.(budget_2s = sum(budget_2s, na.rm = TRUE)), by = .(loc_name, module, version, cycle, grant_period)]

data = as.data.table(read.csv(inFile))
data_tableau = data[rssh==TRUE & budget_version %in% c('funding_request20', 'funding_request20_CT', 'approved'), .(budget=sum(budget, na.rm=TRUE)), by = .(loc_name, gf_module, gf_intervention, activity_description, grant_period, budget_version )]
data_tableau[ grant_period == '2018-2020', cycle := 'NFM2']
data_tableau[ grant_period == '2021-2023', cycle := 'NFM3']
data_tableau[ budget_version == 'approved', version := 'approved_budget']
data_tableau[ budget_version == 'funding_request20', version := 'funding_request']
data_tableau[ budget_version == 'funding_request20_CT', version := 'funding_request_CT']
setnames(data_tableau, 'budget', 'correct_budget')
setnames(data_tableau, 'gf_module', 'module')
setnames(data_tableau, 'gf_intervention', 'intervention')
# data_tableau_module = data_tableau[, .(correct_budget=sum(correct_budget, na.rm=TRUE)), by = .(loc_name, module, version, cycle )]
# data_tableau_intervention = data_tableau[, .(correct_budget=sum(correct_budget, na.rm=TRUE)), by = .(loc_name, module, intervention, version, cycle )]

# x = merge(data_tableau_module, check_2s_coded_module, all = TRUE, by = c('loc_name', 'module', 'version', 'cycle'))
# x[, discrepancy := round(correct_budget) - round(budget_2s)]
# x[discrepancy > 5 | discrepancy < -5, check := 'flag']
# 
# y = merge(data_tableau_intervention, check_2s_coded_intervention, all = TRUE, by = c('loc_name', 'module', 'intervention', 'version', 'cycle'))
# y[, discrepancy := round(correct_budget) - round(budget_2s)]
# y[discrepancy > 5 | discrepancy < -5, check := 'flag']

check_2s_coded[, activity_description := trimws(activity_description)]
check_2s_coded[, activity_description := gsub('  ', ' ', activity_description)]
data_tableau[, activity_description := trimws(activity_description)]
data_tableau[, activity_description :=  gsub('  ', ' ', activity_description)]

z = merge(check_2s_coded, data_tableau, all = TRUE, by = c('loc_name', 'module', 'intervention', 'activity_description', 'version', 'cycle'))

# z = z[intervention == 'Surveys']
# z = z[, -c('grant_period.x', 'grant_period.y')]
# z[, discrepancy := round(correct_budget) - round(budget_2s)]
# z[discrepancy > 5 | discrepancy < -5, check := 'flag']

# -------------------------------------------------------------------


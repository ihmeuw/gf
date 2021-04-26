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
library(stringr)

# -------------------------------------------------------------------
# Files and directories
setwd('C:/local/gf/')

user=as.character(Sys.info()[7])

# input file
box = paste0("C:/Users/", user, "/Box Sync/Global Fund Files/")
inFile = paste0(box, 'tableau_data/budgetRevisions_with_frBudgets_activityLevel.csv')
inFile_2s_coded = paste0(box, '2s_data/prepped_2s_data_all_countries.csv')
outFile = paste0(box, '2s_data/corrected_DRC_2s_data.csv')
outFile_all_countries = paste0(box, '2s_data/CORRECTED_prepped_2s_data_all_countries.csv')
# -------------------------------------------------------------------

# -------------------------------------------------------------------
# read in and sum to appropriate levels
# -------------------------------------------------------------------
data_2s = as.data.table(read.csv(paste0(box, '2s_data/prepped_2s_data_all_countries.csv')))
setnames(data_2s, 'budget', 'budget_2s')
data_2s[loc_name == 'UGA', loc_name := 'Uganda']
data_2s[loc_name == 'SEN', loc_name := 'Senegal']
data_2s[loc_name == 'GTM', loc_name := 'Guatemala']
# check_2s_coded = data_2s[,.(budget_2s = sum(budget_2s, na.rm = TRUE)), by = .(loc_name, module, intervention, activity_description, version, cycle, grant_period)]
# check_2s_coded_intervention = data_2s[,.(budget_2s = sum(budget_2s, na.rm = TRUE)), by = .(loc_name, module, intervention, version, cycle, grant_period)]
# check_2s_coded_module = data_2s[,.(budget_2s = sum(budget_2s, na.rm = TRUE)), by = .(loc_name, module, version, cycle, grant_period)]

data = as.data.table(read.csv(inFile))
data_tableau = data[rssh==TRUE & budget_version %in% c('funding_request20', 'approved'), .(budget=sum(budget, na.rm=TRUE)), by = .(loc_name, gf_module, gf_intervention, activity_description, cost_category, grant, grant_period, budget_version )]
data_tableau[ grant_period %in% c('2018-2020', '2019-2021', '2019-2022'), cycle := 'NFM2']
data_tableau[ grant_period == '2021-2023', cycle := 'NFM3']
data_tableau[ budget_version == 'approved', version := 'approved_budget']
data_tableau[ budget_version == 'funding_request20', version := 'funding_request']
# data_tableau[ budget_version == 'funding_request20_CT', version := 'funding_request_CT']
data_tableau[, budget_version := NULL]

setnames(data_tableau, 'gf_module', 'module')
setnames(data_tableau, 'gf_intervention', 'intervention')
setnames(data_tableau, 'cost_category', 'cost_input')
# setnames(data_tableau, 'budget_version', 'version')
# data_tableau_module = data_tableau[, .(correct_budget=sum(correct_budget, na.rm=TRUE)), by = .(loc_name, module, version, cycle )]
# data_tableau_intervention = data_tableau[, .(correct_budget=sum(correct_budget, na.rm=TRUE)), by = .(loc_name, module, intervention, version, cycle )]

# x = merge(data_tableau_module, check_2s_coded_module, all = TRUE, by = c('loc_name', 'module', 'version', 'cycle'))
# x[, discrepancy := round(correct_budget) - round(budget_2s)]
# x[discrepancy > 5 | discrepancy < -5, check := 'flag']
# 
# y = merge(data_tableau_intervention, check_2s_coded_intervention, all = TRUE, by = c('loc_name', 'module', 'intervention', 'version', 'cycle'))
# y[, discrepancy := round(correct_budget) - round(budget_2s)]
# y[discrepancy > 5 | discrepancy < -5, check := 'flag']

# removing whitespace from beginning and end of all merging variables just in case
cols_trim <- c("activity_description","module","intervention","cost_input")
data_2s[,(cols_trim) :=lapply(.SD,trimws),.SDcols = cols_trim]
data_tableau[,(cols_trim) := lapply(.SD, trimws),.SDcols = cols_trim]

# this should trim any spaces greater than 2
data_2s[, activity_description := gsub('\\s+', ' ', activity_description)]
data_tableau[, activity_description := gsub('\\s+', ' ', activity_description)]

data_2s[, grant:=gsub('_', '-', grant)]

all_2s_data_cols = names(data_2s)[!names(data_2s)%in%c('budget_2s')]
data_2s = data_2s[, .(budget_2s = sum(budget_2s, na.rm = TRUE)), by = all_2s_data_cols]
data_tableau = data_tableau[, .(budget = sum(budget, na.rm = TRUE)), by = c('loc_name', 'module', 'intervention', 'activity_description', 'cost_input', 'version', 'cycle', 'grant', 'grant_period')]
data_2s[version == 'funding_request', grant:=NA]

# merge together
z = merge(data_tableau[loc_name == 'DRC',], data_2s[loc_name == 'DRC',], all = TRUE, by = c('loc_name', 'module', 'intervention', 'activity_description', 'cost_input', 'version', 'cycle', 'grant', 'grant_period'))

# z = z[intervention == 'Surveys']
# z = z[, -c('grant_period.x', 'grant_period.y')]
z[, discrepancy := round(budget) - round(budget_2s)]
z[discrepancy > 5 | discrepancy < -5, check := 'flag']
z[is.na(budget) & !is.na(budget_2s), check := 'flag']
z[budget!=0 & is.na(budget_2s), check := 'flag']

# save corrected DRC 2S data
z[, c('budget_2s', 'discrepancy', 'check'):=NULL]
z=z[!is.na(budget)]
write.csv(z, outFile, row.names = FALSE)

# save corrected data all countries
data_2s = as.data.table(read.csv(paste0(box, '2s_data/prepped_2s_data_all_countries.csv')))
data_2s = data_2s[loc_name != 'DRC']
z = z[budget != 0, ]
corrected_data = rbindlist(list(data_2s,z), use.names = TRUE, fill = TRUE)
write.csv(corrected_data, outFile_all_countries, row.names = FALSE)
# -------------------------------------------------------------------


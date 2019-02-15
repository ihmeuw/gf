# ----------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Compares current RT files with archived version 
#   from Fall 2018. 
# DATE: Last updated February 2019
# ----------------------------------------------

rm(list=ls())
library(data.table)
library(dplyr)

options(scipen=100)

#----------------------------------
# Read in files 
# ---------------------------------
archive_rt <- fread("J:/Project/Evaluation/GF/resource_tracking/archive/cleaned_total_data_bc_archive.csv")
current_rt <- readRDS("J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/budget_pudr_iterations.rds")
final_budgets <- readRDS("J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/final_budgets.rds")
final_expenditures <- readRDS("J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/final_expenditures.rds")

final_files <- rbind(final_budgets, final_expenditures, fill = TRUE)
#Subset archived file 
archive_rt = archive_rt[data_source != 'sicoin' & data_source != 'fgh']

#-----------------------------------------------------------------------
#Compare which files are marked 'final' in our data vs the archived data
#-----------------------------------------------------------------------
archive_final_files <- unique(archive_rt[data_source%in%c('fpm', 'pudr', 'gos'), .(fileName)])
current_final_files <- unique(current_rt[current_grant == TRUE, .(fileName)])

current_rt_only <- anti_join(current_final_files, archive_final_files)
archived_rt_only <- anti_join(archive_final_files, current_final_files)

#Here, we really only want to review the ones that aren't marked 'final' in the current data, because we know we're dropping a lot 
# of files from the GOS reprioritization. 
if (nrow(current_rt_only)!=0){
  print("Error: some files are now classified as 'final' that weren't in the archived data.")
}

#-----------------------------------------------------------------------
#Calculate total spend for RSSH for each file. 
#-----------------------------------------------------------------------
rssh_current = current_rt[substring(code, 1, 1) == 'R']
rssh_archive = archive_rt[substring(code, 1, 1) == 'R']

current_rssh_by_file <- rssh_current[, lapply(.SD, sum), .SDcols = c('budget', 'expenditure'), by=c('fileName')]
colnames(current_rssh_by_file) <- c('fileName', 'budget_current', 'exp_current')
archive_rssh_by_file <- rssh_archive[, lapply(.SD, sum), .SDcols = c('budget', 'expenditure'), by=c('fileName')]
colnames(archive_rssh_by_file) <- c('fileName', 'budget_archive', 'exp_archive')

#Compare these numbers 
spend_compare_rssh <- merge(current_rssh_by_file, archive_rssh_by_file, by=c('fileName'))
spend_compare_rssh[, budget_diff:=budget_archive-budget_current]
spend_compare_rssh[, exp_diff:=exp_archive-exp_current]

if (nrow(spend_compare_rssh[round(budget_current)!=round(budget_archive)])!=0){
  print("RSSH discrepancies between current and archived files")
  print(spend_compare_rssh[round(budget_current)!=round(budget_archive), .(fileName, budget_current, budget_archive, budget_diff)][order(-abs(budget_diff))])
}

if (nrow(spend_compare_rssh[round(exp_current)!=round(exp_archive)])!=0){
  print("Expenditure discrepancies between current and archived files")
  print(spend_compare_rssh[round(exp_current)!=round(exp_archive), .(fileName, exp_current, exp_archive, exp_diff)][order(-abs(exp_diff))])
}

#-----------------------------------------------------------------------
#Calculate total spend for each file.
#-----------------------------------------------------------------------
current_total_by_file <- current_rt[, lapply(.SD, sum), .SDcols = c('budget', 'expenditure'), by=c('fileName')]
colnames(current_total_by_file) <- c('fileName', 'budget_current', 'exp_current')
archive_total_by_file <- archive_rt[, lapply(.SD, sum), .SDcols = c('budget', 'expenditure'), by=c('fileName')]
colnames(archive_total_by_file) <- c('fileName', 'budget_archive', 'exp_archive')

#Compare these numbers 
spend_compare_total <- merge(current_total_by_file, archive_total_by_file, by=c('fileName'))
spend_compare_total[, budget_diff:=budget_archive-budget_current]
spend_compare_total[, exp_diff:=exp_archive-exp_current]

#EKL investigated these 2/6/19 - all of the budget differences >$50 are correct in the current files. 
if (nrow(spend_compare_total[round(budget_current)!=round(budget_archive)])!=0){
  print("Budget discrepancies between current and archived files")
  print(spend_compare_total[round(budget_current)!=round(budget_archive), .(fileName, budget_current, budget_archive, budget_diff)][order(-abs(budget_diff))])
}

if (nrow(spend_compare_total[round(exp_current)!=round(exp_archive)])!=0){
  print("Expenditure discrepancies between current and archived files")
  print(spend_compare_total[round(exp_current)!=round(exp_archive), .(fileName, exp_current, exp_archive, exp_diff)][order(-abs(exp_diff))])
}

#-----------------------------------------------------------------------
#Calculate total spend by grant/year/disease (to compare difference between GOS and our budgets)
#-----------------------------------------------------------------------
current_total_by_grant <- final_files[, lapply(.SD, sum), .SDcols = c('budget', 'expenditure'), by=c('grant_number', 'year')]
colnames(current_total_by_grant) <- c('grant_number', 'year', 'budget_current', 'exp_current')
archive_total_by_grant <- archive_rt[, lapply(.SD, sum), .SDcols = c('budget', 'expenditure'), by=c('grant_number', 'year')]
colnames(archive_total_by_grant) <- c('grant_number', 'year', 'budget_archive', 'exp_archive')

#Compare these numbers 
spend_compare_grant <- merge(current_total_by_grant, archive_total_by_grant, by=c('grant_number', 'year'))
spend_compare_grant[, budget_diff:=budget_archive-budget_current]
spend_compare_grant[, exp_diff:=exp_archive-exp_current]
spend_compare_grant = spend_compare_grant[budget_diff!=0 & exp_diff!=0]

if (nrow(spend_compare_grant[round(budget_current)!=round(budget_archive)])!=0){
  print("Budget discrepancies between current and archived files")
  print(spend_compare_grant[round(budget_current)!=round(budget_archive), .(grant_number, year, budget_current, budget_archive, budget_diff)][order(-year, grant_number, -abs(budget_diff))])
}

if (nrow(spend_compare_grant[round(exp_current)!=round(exp_archive)])!=0){
  print("Expenditure discrepancies between current and archived files")
  print(spend_compare_grant[round(exp_current)!=round(exp_archive), .(grant_number, year, exp_current, exp_archive, exp_diff)][order(-year, grant_number, -abs(exp_diff))])
}

#-----------------------------------------------------------------------
#Compare which line items (module, intervention, disease) have different codes in the two files. #EKL keep working here! 
#-----------------------------------------------------------------------
current_mods = current_rt[, .(gf_module, gf_intervention, module, intervention, code, sda_activity, fileName)]
archive_mods = archive_rt[, .(gf_module, gf_intervention, module, intervention, code, sda_activity, fileName)]

mods_current_only <- anti_join(current_mods, archive_mods, by=c('gf_module', 'gf_intervention', 'code', 'sda_activity', 'fileName')) #Returns all rows in x where there are not rows in y
mods_archive_only <- anti_join(archive_mods, current_mods, by=c('gf_module', 'gf_intervention', 'code', 'sda_activity', 'fileName')) #Returns all rows in x where there are not rows in y



#Make sure to distinguish old and new grants. 

#-----------------------------------------------------------------------
# Write an output file to review  
#-----------------------------------------------------------------------
setwd("J:/Project/Evaluation/GF/resource_tracking/rt_database_updates")
sink("compare_archived_rtdb.txt")

print("")
print("\#-------------------------------\#")
print("ERROR: These files are marked final in the archived database, but NOT in the current database.")
print(current_rt_only)

print("")
print("#-------------------------------#")
print("ERROR: Budget discrepancies between current and archived files, by file name")
print(spend_compare_total[round(budget_current)!=round(budget_archive), .(fileName, budget_current, budget_archive, budget_diff)][order(-abs(budget_diff))])
print("") 
print("ERROR: Expenditure discrepancies between current and archived files, by file name")
print(spend_compare_total[round(exp_current)!=round(exp_archive), .(fileName, exp_current, exp_archive, exp_diff)][order(-abs(exp_diff))])

print("")
print("#-------------------------------#")
print("ERROR: RSSH budget discrepancies between current and archived files")
print(spend_compare_rssh[round(budget_current)!=round(budget_archive), .(fileName, budget_current, budget_archive, budget_diff)][order(-abs(budget_diff))])
print("") 
print("ERROR: RSSH expenditure discrepancies between current and archived files")
print(spend_compare_rssh[round(exp_current)!=round(exp_archive), .(fileName, exp_current, exp_archive, exp_diff)][order(-abs(exp_diff))])

print("")
print("#-------------------------------#")
print("ERROR: Budget discrepancies between current and archived files, by grant and year")
print(spend_compare_grant[round(budget_current)!=round(budget_archive), .(grant_number, year, budget_current, budget_archive, budget_diff)][order(-year, grant_number, -abs(budget_diff))])
print("") 
print("ERROR: Expenditure discrepancies between current and archived files, by grant and year")
print(spend_compare_grant[round(exp_current)!=round(exp_archive), .(grant_number, year, exp_current, exp_archive, exp_diff)][order(-year, grant_number, -abs(exp_diff))])


sink()
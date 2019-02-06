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

#-----------------------------------------------------------------------
#Calculate total spend for each file.
#-----------------------------------------------------------------------
current_total_by_file <- current_rt[, lapply(.SD, sum), .SDcols = c('budget', 'expenditure'), by=c('fileName')]
colnames(current_total_by_file) <- c('fileName', 'budget_current', 'exp_current')
archive_total_by_file <- archive_rt[, lapply(.SD, sum), .SDcols = c('budget', 'expenditure'), by=c('fileName')]
colnames(archive_total_by_file) <- c('fileName', 'budget_archive', 'exp_archive')

#Compare these numbers 
spend_compare <- merge(current_total_by_file, archive_total_by_file, by=c('fileName'))
spend_compare[, budget_diff:=budget_archive-budget_current]
spend_compare[, exp_diff:=exp_archive-exp_current]

if (spend_compare$budget_current != spend_compare$budget_archive){
  print("Budget discrepancies between current and archived files")
  print(spend_compare[round(budget_current)!=round(budget_archive), .(fileName, budget_current, budget_archive, budget_diff)][order(abs(budget_diff))])
}

if (spend_compare$exp_current != spend_compare$exp_archive){
  print("Expenditure discrepancies between current and archived files")
  print(spend_compare[round(exp_current)!=round(exp_archive), .(fileName, exp_current, exp_archive, exp_diff)][order(abs(exp_diff))])
}

#-----------------------------------------------------------------------
#Calculate total spend by grant/year/disease (to compare difference between GOS and our budgets)
#-----------------------------------------------------------------------

#-----------------------------------------------------------------------
#Compare which line items (module, intervention, disease) have different codes in the two files.
#-----------------------------------------------------------------------


#Make sure to distinguish old and new grants. 
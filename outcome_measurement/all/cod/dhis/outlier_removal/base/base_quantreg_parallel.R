# Prep & remove outliers from the COD DHIS2 PNLS Viral Load data 
# Impute missing data in DHIS2 SIGL data
# ----------------------------------------------
# Caitlin O'Brien-Carelli / Audrey Batzel (3-7-19)
#
# 10/1/2018
# The current working directory should be the same as this script
# This code must be run on the cluster

# ----------------------------------------------

# --------------------
# Set up R
# --------------------
rm(list=ls())
library(data.table)
library(quantreg)
library(fst) # to save data tables as .fst for faster read/write and full random access

user_name = 'ccarelli'
# --------------------

# --------------------
# Manual set up on the cluster
#---------------------

# --------------------
# make sure qr_results exists
# cd /ihme/scratch/users/ccarelli/

# copy over the base services data from j - to source from scracth
# cp '/home/j/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/outliers/base_to_screen.rds' '/ihme/scratch/users/ccarelli/'

# to delete files in the directory
# rm base_results/*
# rm base_output/*

# set the working directory in the qlogin by navigating to it
# cd /ihme/code/ccarelli/gf/outcome_measurement/all/cod/dhis/outlier_removal/base

# print the contents
# ls

# once you navigate to the directory, git pull R
# make sure you have pushed from github desktop

# then call R

# then source this script (located in your working directory)
# source('base_quantreg_parallel.r')

#------------------------------------

#------------------------------------
# set directories, switchs, arguments  
#------------------------------------
# detect if operating on windows or on the cluster 
root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set the directory for input and output
dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/')

# output file
outFile = paste0(dir, 'outliers/base_quantreg_results.rds')

# whether or not to resubmit jobs that have completed already
resubmitAll = TRUE

# whether or not to delete all files from parallel runs at the end
cleanup = TRUE

#------------------------------------

#------------------------------------
# read in and set up the data
#------------------------------------
# data set with equality constraints checked and an entry for both tests/undetectable
dt = readRDS('/ihme/scratch/users/ccarelli/base_to_screen.rds')
dt = data.table(dt)

# sort dt so indexing works correctly when retrieving data using fst
dt = setorder(dt, org_unit_id)

# make array table to set up for submitting an array job
array_table = data.table(expand.grid(unique(dt$org_unit_id)))
setnames(array_table, "Var1", "org_unit_id")

# subset for testing:
array_table = array_table[1:5,]

# save the array table and the data with IDs to /ihme/scratch/
write.csv(array_table, paste0('/ihme/scratch/users/', user_name, '/array_table_for_qr.csv'))
write.fst(dt, paste0('/ihme/scratch/users/', user_name, '/data_for_qr.fst'))

#------------------------------------

#------------------------------------
# run quantregScript.r as separate qsubs for each subset of date, org_unit, element, and variable.
#------------------------------------
# array job
N = nrow(array_table)
PATH = paste0('/ihme/scratch/users/', user_name, '/base_output')
system(paste0('qsub -e ', PATH, ' -o ', PATH,' -N all_quantreg_jobs -cwd -t 1:', N, ' ./core/r_shell.sh ./outcome_measurement/all/cod/dhis/outlier_removal/quantregScript_base2.r'))

# NOTE: file paths now relative to the root of the repo

# # loop over elements and org units, run quantreg once per each
# i=1
# for (v in unique(dt$variable_id)) {
#   for (e in unique(dt$element_id)) { 
#     for(o in unique(dt$org_unit_id)) { 
#       # skip if this job has already run and resubmitAll is FALSE
#       if (resubmitAll==FALSE & file.exists(paste0('/ihme/scratch/users/', user_name, '/qr_results/quantreg_output', i, '.rds'))) { 
#          i=i+1
#          next
#       } else {
#         # run the quantile regression and list the residuals
#         system(paste0('qsub -o /ihme/scratch/users/', user_name, '/quantreg_output -e /ihme/scratch/users/', user_name, '/quantreg_output -cwd -N quantreg_output_', i, ' ../../../../../core/r_shell.sh ./quantregScript.r ', e, ' ', o, ' ', i, ' ', inFile, ' ', impute, ' ', v ))
#         i=i+1
#       }
#     }
#   }
# }
#------------------------------------

#------------------------------------
# wait for files to be done
#------------------------------------
i = N-1
numFiles = length(list.files(paste0('/ihme/scratch/users/', user_name, '/base_results')))
while(numFiles<i) { 
  print(paste0(numFiles, ' of ', i, ' jobs complete, waiting 60 seconds...'))
  numFiles = length(list.files(paste0('/ihme/scratch/users/', user_name, '/base_results')))
  Sys.sleep(60)
}
#------------------------------------

#------------------------------------
# once all files are done, collect all output into one data table
#------------------------------------
fullData = data.table()

for (j in seq(N)) {
  tmp = read.fst(paste0('/ihme/scratch/users/', user_name, '/base_results/base_output', j, '.fst'), as.data.table = TRUE)
  if(j==1) fullData = tmp
  if(j>1) fullData = rbind(fullData, tmp)
  cat(paste0('\r', j))
  flush.console() 
}

# save full data
saveRDS(fullData, outFile)
#------------------------------------

#------------------------------------
# clean up parallel files
#------------------------------------
if (cleanup==TRUE) { 
  system(paste0('rm /ihme/scratch/users/', user_name, '/qr_results/*'))
  system(paste0('rm /ihme/scratch/users/', user_name, '/quantreg_output/*'))
  system(paste0('rm /ihme/scratch/users/', user_name, '/array_table_for_qr.csv'))
  system(paste0('rm /ihme/scratch/users/', user_name, '/data_for_qr.fst'))
}
#------------------------------------
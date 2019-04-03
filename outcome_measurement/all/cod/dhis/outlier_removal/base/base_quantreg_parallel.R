# Prep & remove outliers from the COD DHIS2 PNLS Viral Load data 
# Impute missing data in DHIS2 SIGL data
# ----------------------------------------------
# Caitlin O'Brien-Carelli / Audrey Batzel (3-7-19)
#
# 4/1/2019
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
# cd /ihme/code/ccarelli/gf/

# print the contents
# ls

# once you navigate to the directory, git pull R
# make sure you have pushed from github desktop

# then call R

# then source this script (located in your working directory)
# source('./outcome_measurement/all/cod/dhis/outlier_removal/base/base_quantreg_parallel.R')

#------------------------------------

#------------------------------------
# set directories, switchs, arguments  
#------------------------------------
# detect if operating on windows or on the cluster 
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

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
dt = readRDS(paste0('/ihme/scratch/users/', user_name, '/base_to_screen.rds'))
dt = data.table(dt)

# sort dt so indexing works correctly when retrieving data using fst
dt = setorder(dt, org_unit_id)

# make array table to set up for submitting an array job
array_table = data.table(expand.grid(unique(dt$org_unit_id)))
setnames(array_table, "Var1", "org_unit_id")
array_table[ ,org_unit_id:=as.character(org_unit_id)]

array_table = array_table[1:10,]

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
setwd('/ihme/code/ccarelli/gf/')
system(paste0('qsub -e ', PATH, ' -o ', PATH,' -N base_jobs -cwd -t 1:', N, ' ./core/r_shell.sh ./outcome_measurement/all/cod/dhis/outlier_removal/base_script.r'))

#------------------------------------

#------------------------------------
# wait for files to be done
#------------------------------------

i = N-1
numFiles = length(list.files(paste0('/ihme/scratch/users/', user_name, '/base_results')))
while(numFiles<i) { 
  print(paste0(numFiles, ' of ', i, ' jobs complete, waiting 5 seconds...'))
  numFiles = length(list.files(paste0('/ihme/scratch/users/', user_name, '/base_results')))
  Sys.sleep(5)
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
  system(paste0('rm /ihme/scratch/users/', user_name, '/base_results/*'))
  system(paste0('rm /ihme/scratch/users/', user_name, '/base_output/*'))
  system(paste0('rm /ihme/scratch/users/', user_name, '/array_table_for_qr.csv'))
  system(paste0('rm /ihme/scratch/users/', user_name, '/data_for_qr.fst'))
}
#------------------------------------
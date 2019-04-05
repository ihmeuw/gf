# Prep & remove outliers from the COD DHIS2 PNLS Viral Load data 
# Impute missing data in DHIS2 SIGL data
# ----------------------------------------------
# Caitlin O'Brien-Carelli / Audrey Batzel (3-7-19)
#
# 4/1/2019
# The current working directory should be the root of this repository
# This code must be run on the cluster

# ----------------------------------------------

# --------------------
# Manual set up on the cluster
#---------------------

# --------------------
# make sure qr_results exists
# cd /ihme/scratch/users/ccarelli/

# to delete files in the directory
# rm quantreg/parallel_files/*
# rm quantreg/errors_output/*

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

# --------------------
# Set up R
# --------------------
rm(list=ls())
library(data.table)
library(quantreg)
library(fst) # to save data tables as .fst for faster read/write and full random access

user_name = Sys.info()[['user']]
# --------------------

#------------------------------------
# set directories, switchs, arguments  
#------------------------------------
# detect if operating on windows or on the cluster 
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set the directory for input and output
dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/')
scratchDir = paste0('/ihme/scratch/users/', user_name, '/quantreg/')
parallelDir = paste0(scratchDir, 'parallel_files/')
if (!file.exists(scratchDir)) dir.create(scratchDir)
if (!file.exists(parallelDir)) dir.create(parallelDir)

# place for cluster output/error files
oeDir = paste0(scratchDir, 'errors_output/')
if (!file.exists(oeDir)) dir.create(oeDir)

# input file and location to copy it to
inFile = paste0(dir, 'outliers/base_to_screen.rds')
scratchInFile = paste0(scratchDir, 'data_for_qr.fst')

# interim files
arrayFile = paste0(scratchDir, 'array_table_for_qr.csv')

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
dt = readRDS(inFile)

# convert date to a character vector as a separate tracker
dat[ , date_track:=as.character(date)]

# sort dt so indexing works correctly when retrieving data using fst
dt = setorder(dt, org_unit_id)

# make array table to set up for submitting an array job
array_table = data.table(expand.grid(unique(dt$org_unit_id)))
setnames(array_table, "Var1", "org_unit_id")
array_table[ ,org_unit_id:=as.character(org_unit_id)]

# for testing, subset to ten rows
array_table = array_table[1:10, ]

# save the array table and the data with IDs to /ihme/scratch/
write.csv(array_table, arrayFile)
write.fst(dt, scratchInFile)
#------------------------------------

#------------------------------------
# run quantregScript.r as separate qsubs for each subset of date, org_unit, element, and variable.
#------------------------------------
# array job
N = nrow(array_table)

system(paste0('qsub -e ', oeDir, ' -o ', oeDir,' -N base_jobs -cwd -t 1:', N, ' ./core/r_shell.sh ./outcome_measurement/all/cod/dhis/outlier_removal/base_script.R'))
#------------------------------------


#------------------------------------
# wait for files to be done
#------------------------------------

i = N-1
numFiles = length(list.files(parallelDir))
while(numFiles<i) { 
  print(paste0(numFiles, ' of ', i, ' jobs complete, waiting 5 seconds...'))
  numFiles = length(list.files(parallelDir))
  Sys.sleep(5)
}
#------------------------------------

#------------------------------------
# once all files are done, collect all output into one data table
#------------------------------------
fullData = data.table()

for (j in seq(N)) {
  tmp = read.fst(paste0(parallelDir, 'quantreg_output', j, '.fst'), as.data.table=TRUE)
  if(j==1) fullData = tmp
  if(j>1) fullData = rbind(fullData, tmp)
  fullData[ , date:=as.Date(date, origin = "1970-01-01")]
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
  system(paste0('rm /ihme/scratch/users/', user_name, '/quantreg/parallel_files/*'))
  system(paste0('rm /ihme/scratch/users/', user_name, '/quantreg/errors_output/*'))
  system(paste0('rm /ihme/scratch/users/', user_name, '/quantreg/array_table_for_qr.csv'))
  system(paste0('rm /ihme/scratch/users/', user_name, '/quantreg/data_for_qr.fst'))
}
#------------------------------------
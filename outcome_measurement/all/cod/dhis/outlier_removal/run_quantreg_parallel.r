# Prep & remove outliers from the COD DHIS2 PNLS Viral Load data 
# Impute missing data in DHIS2 SIGL data
#------------------------------------
# Caitlin O'Brien-Carelli / Audrey Batzel (3-7-19)
#
# 4/1/2019
# The current working directory should be the root of this repository
# This code must be run on the cluster
#------------------------------------

#------------------------------------
# Manual set up on the cluster
#------------------------------------
# make sure qr_results exists
# cd /ihme/scratch/users/ccarelli/

# set the working directory in the qlogin by navigating to it
# cd /ihme/code/ccarelli/gf/
# cd /ihme/code/abatzel/gf/

# print the contents
# ls

# once you navigate to the directory, git pull R
# make sure you have pushed from github desktop

# then call R
#------------------------------------

#------------------------------------
# Set up R
#------------------------------------
rm(list=ls())
library(data.table)
library(quantreg)
library(fst) # to save data tables as .fst for faster read/write and full random access

# detect the user operating on the cluster
user = Sys.info()[['user']]

# choose the data set you want to load
set = 'base'
#------------------------------------

#------------------------------------
# set directories, switchs, arguments  
#------------------------------------
# detect if operating on windows or on the cluster 
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set the directory for input and output
dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/')
scratchDir = paste0('/ihme/scratch/users/', user, '/quantreg/')
parallelDir = paste0(scratchDir, 'parallel_files/')
if (!file.exists(scratchDir)) dir.create(scratchDir)
if (!file.exists(parallelDir)) dir.create(parallelDir)

# input data file to be copied to the cluster
scratchInFile = paste0(scratchDir, 'data_for_qr.fst')

# place for cluster output/error files
oeDir = paste0(scratchDir, 'errors_output/')
if (!file.exists(oeDir)) dir.create(oeDir)

# set arguments and interim files to use on the cluster
arrayFile = paste0(scratchDir, 'array_table_for_qr.csv')

# switches
cleanup_start = TRUE # whether or not to delete all files from parallel runs at the beginning
cleanup_end = FALSE # "" /end
impute = 'TRUE' # whether or not to impute missing data as part of the qr
cat_files = TRUE # whether or not to concatenate all of the files at the end
#------------------------------------

#------------------------------------
# clean up parallel files at the start
#------------------------------------
if (cleanup_start == TRUE){
  # before starting the process, delete the existing files on the cluster
  # this allows us to avoid duplication or aggregating old files 
  system(paste0('rm -r /ihme/scratch/users/', user, '/quantreg/*')) # removes all files and folders within the directory
}
#------------------------------------

#------------------------------------
# input file and location to copy it to
# initial file is read off of j 
# output file is the aggregate of the files from /ihme/scratch

if (set=='sigl') {inFile = paste0(dir, 'prepped/sigl/sigl_for_qr.rds') 
outFile = paste0(dir, 'sigl_quantreg_imputation_results.rds') }

if (set=='base') {inFile = paste0(dir, 'outliers/base_to_screen.rds')
outFile = paste0(dir, 'outliers/base_quantreg_results.rds')}

if (set=='pnlp') {inFile = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLP/pnlp_for_qr.rds')
outFile = paste0(dir, 'outliers/pnlp_quantreg_results.rds')}
#------------------------------------

#------------------------------------
# read in the data and create the array table

dt = readRDS(inFile)

# format the pnlp data in the same format as the base data
# this assigns an element id to each variable and refered to the health zone as an org_unit
if (set=='pnlp') setnames(dt, 'health_zone', 'org_unit_id')
if (set=='pnlp') dt[, element_id:=.GRP, by='variable'] 

if (set=='sigl') dt[, element_id:=.GRP, by='drug']
if (set=='sigl') dt[, variable_id:=.GRP, by='variable']

# sort the data table so the indexing works correctly when retrieving data using fst
dt = setorder(dt, org_unit_id)
dt[ ,date:=as.Date(date)] # regression only runs with date as a date variable

# make array table to set up for submitting an array job
array_table = data.table(expand.grid(unique(dt$org_unit_id)))
setnames(array_table, "Var1", "org_unit_id")
array_table[ ,org_unit_id:=as.character(org_unit_id)]

# for testing, subset to a few rows
array_table = array_table[1:10, ]

# save the array table and the data with IDs to /ihme/scratch/
write.csv(array_table, arrayFile)
write.fst(dt, scratchInFile)
#------------------------------------

#------------------------------------
# run quantregScript.r as separate qsubs
#------------------------------------
# file pathways are now relative to the root of the repository

# determine the number of rows in the array job
N = nrow(array_table)

# base data set: run value~date on each org_unit and element
system(paste0('qsub -e ', oeDir, ' -o ', oeDir,' -N quantreg_jobs -cwd -t 1:', N, ' ./core/r_shell.sh ./outcome_measurement/all/cod/dhis/outlier_removal/quantregScript.R')) 

# # base data set: run value~date on each org_unit and element
# system(paste0('qsub -e ', oeDir, ' -o ', oeDir,' -N quantreg_jobs -cwd -t 1:', N, ' ./core/r_shell.sh ./outcome_measurement/all/cod/dhis/outlier_removal/quantregScript.R -l m_mem_free=1G -l fthread=2 -P proj_pce -q all')) 
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
if (cat_files == TRUE){
  # fullData = data.table()
  # 
  # for (k in seq(N)) {
  #   tmp = read.fst(paste0(parallelDir, 'quantreg_output', k, '.fst'), as.data.table=TRUE)
  #   if(k==1) fullData = tmp
  #   if(k>1) fullData = rbind(fullData, tmp)
  #   cat(paste0('\r', j))
  #   flush.console()
  # }
  # 
  # # change the name of the health zones back to 'health_zone'
  # if (set=='pnlp') setnames(dt, 'org_unit_id', 'health_zone')
  # 
  # # save full data
  # saveRDS(fullData, outFile)
  # scratchDir = paste0('/ihme/scratch/users/', user, '/quantreg/')
  # parallelDir = paste0(scratchDir, 'parallel_files/')

  # faster binding:
  system(paste0('cat /ihme/scratch/users/', user, '/quantreg/parallel_files/* > ', outFile))
}
#------------------------------------

#------------------------------------
# end cleanup
#------------------------------------
if (cleanup_end==TRUE) { 
  system(paste0('rm /ihme/scratch/users/', user_name, '/qr_results/*'))
  system(paste0('rm /ihme/scratch/users/', user_name, '/quantreg_output/*'))
  system(paste0('rm /ihme/scratch/users/', user_name, '/array_table_for_qr.csv'))
  system(paste0('rm /ihme/scratch/users/', user_name, '/data_for_qr.fst'))
}
#------------------------------------

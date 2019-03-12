# Prep & remove outliers from the COD DHIS2 PNLS Viral Load data 
# Impute missing data in DHIS2 SIGL data
# ----------------------------------------------
# Caitlin O'Brien-Carelli / Audrey Batzel (3-7-19)
#
# 10/1/2018
# The current working directory should be the same as this script
# This code must be run on the cluster. 
# ----------------------------------------------

# --------------------
# Set up R
# --------------------
rm(list=ls())
library(data.table)
library(quantreg)
library(fst) # to save data tables as .fst for faster read/write and full random access

user_name = 'abatzel'
# --------------------

# --------------------
# Manual set up on the cluster
#---------------------
# make sure qr_results exists
# cd /ihme/scratch/users/user_name/

# navigate to directory on the cluster where your code repo is:
# cd /ihme/code/abatzel/gf
# then git pull (make sure you have pushed code for this script and quantregScript.r)

# set the working directory to be the root of the repo:
# cd /ihme/code/user_name/gf/

# print the contents
# ls

# then call R

# then source this script (located in your working directory)
# source('run_quantreg_parallel.r')
#------------------------------------

#------------------------------------
# set directories, switchs, arguments  <---- CHANGE THESE FOR YOUR OWN DATA
#------------------------------------
# detect if operating on windows or on the cluster 
root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set the directory for input and output
dir <- paste0(root, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/prepped/')

# files:
inFile = paste0(dir, 'sigl_for_qr.rds') # read off j at the beginning
outFile = paste0(dir, 'sigl_quantreg_imputation_results.rds') # at the very end, once all of the files are aggregated from /ihme/scratch/

# whether or not to resubmit jobs that have completed already
resubmitAll = TRUE
# whether or not to delete all files from parallel runs at the end
cleanup = TRUE
# whether or note to impute missing data as part of the quantile regression (set as a character TRUE/FALSE so it can be read as a command arg)
impute = "TRUE"
#------------------------------------

#------------------------------------
# read in and set up the data
#------------------------------------
# data set with equality constraints checked and an entry for both tests/undetectable
dt <- readRDS(inFile)

# remove new cases (not of interest for outlier detection)
if (inFile == paste0(dir, 'viral_load_pnls_interim.rds')) { 
  dt = dt[case=='Old']
  dt[ , case:=NULL]
}

# make variable ids
if (inFile == paste0(dir, 'sigl_for_qr.rds')) { dt[, variable_id:=.GRP, by='drug']}
dt[, element_id:=.GRP, by='variable']

# sort dt so indexing works correctly when retrieving data using fst
dt <- setorder(dt, org_unit_id)

# make array table to set up for submitting an array job
array_table = expand.grid(unique(dt$org_unit_id))
setnames(array_table, "Var1", "org_unit_id")

# save the array table and the data with IDs to /ihme/scratch/
write.csv(array_table, paste0('/ihme/scratch/users/', user_name, '/array_table_for_qr.csv'))
write.fst(dt, paste0('/ihme/scratch/users/', user_name, '/data_for_qr.fst'))
#------------------------------------

#------------------------------------
# run quantregScript.r as separate qsubs for each subset of date, org_unit, element, and variable.
#------------------------------------
# array job
N = nrow(array_table)
PATH = paste0('/ihme/scratch/users/', user_name, '/quantreg_output')
system(paste0('qsub -e ', PATH, ' -o ', PATH,' -N all_quantreg_jobs -cwd -t 1:', N, ' ./core/r_shell.sh ./outcome_measurement/all/cod/dhis/outlier_removal/quantregScript.r'))
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
numFiles = length(list.files(paste0('/ihme/scratch/users/', user_name, '/qr_results')))
while(numFiles<i) { 
  print(paste0(numFiles, ' of ', i, ' jobs complete, waiting 60 seconds...'))
  numFiles = length(list.files(paste0('/ihme/scratch/users/', user_name, '/qr_results')))
  Sys.sleep(60)
}
#------------------------------------

#------------------------------------
# once all files are done, collect all output into one data table
#------------------------------------
fullData = data.table()

for (j in seq(N)) {
  tmp = read.fst(paste0('/ihme/scratch/users/', user_name, '/qr_results/quantreg_output', j, '.fst'), as.data.table = TRUE)
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
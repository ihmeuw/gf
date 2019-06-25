# Run quantile regression to identify outliers on COD DHIS2 and program data 
#------------------------------------
# Caitlin O'Brien-Carelli / Audrey Batzel
#
# 6/24/2019
# The current working directory should be the root of this repository
# This code must be run on the cluster
#------------------------------------

#------------------------------------
# Manual set up on the cluster
#------------------------------------
# 1) set the working directory in the qlogin by navigating to it
  # cd /ihme/code/ccarelli/gf/
  # cd /ihme/code/abatzel/gf/

# 2) once you have navigated to the directory, 
  # git pull (make sure you have pushed from github desktop)

# 3) start R with: singularity exec /share/singularity-images/health_fin/forecasting/best_new.img R
#------------------------------------

#------------------------------------
# Set up R
#------------------------------------
rm(list=ls())
library(data.table)

# detect the user operating on the cluster
user = Sys.info()[['user']]

# these libraries must be loaded locally from scratch if using an ide
library(fst, lib.loc=paste0('/ihme/scratch/users/', user, '/packages'))
library(SparseM, lib.loc=paste0('/ihme/scratch/users/', user, '/packages')) 
library(quantreg, lib.loc=paste0('/ihme/scratch/users/', user, '/packages'))

# set the working directory because this is america
setwd(paste0('/ihme/code/', user, '/gf/'))

# choose the data set you want to load
set = 'pnls'
#------------------------------------

#------------------------------------
# switches

cleanup_start = FALSE # whether or not to delete all files from parallel runs at the beginning
cleanup_end = TRUE # "" /end; default to FALSE
# impute = 'TRUE' # whether or not to impute missing data as part of the qr
# cat_files = TRUE # whether or not to concatenate all of the files at the end
agg_to_DPS = FALSE # whether or not to aggregate the data to DPS level before running QR. 
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

# place for cluster output/error files
oeDir = paste0(scratchDir, 'errors_output/')
if (!file.exists(oeDir)) dir.create(oeDir)

# input data file to be copied to the cluster
scratchInFile = paste0(scratchDir, 'data_for_qr.fst')

# set arguments and interim files to use on the cluster
arrayFile = paste0(scratchDir, 'array_table_for_qr.fst')
#------------------------------------

#------------------------------------
# input file and location to copy it to
# initial file is read off of j 
# output file is the aggregate of the files from /ihme/scratch/users/(user_name)/quantreg/parallel_files/

if (set=='sigl') {inFile = paste0(dir, 'prepped/sigl/sigl_for_qr.rds') 
outFile = paste0(dir, 'sigl_quantreg_imputation_results.rds') }

if (set=='base') {inFile = paste0(dir, 'outliers/base/base_to_screen.rds')
outFile = paste0(dir, 'outliers/base_quantreg_results.rds')}

if (set=='pnlp') {inFile = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLP/outliers/pnlp_for_qr.rds')
  if (agg_to_DPS ==TRUE){
    outFile = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLP/outliers/pnlp_quantreg_results_dpsLevel.rds')
  } else {
    outFile = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLP/outliers/pnlp_quantreg_results.rds')
  }
}

if(set=='pnls') inFile = paste0(dir, 'pre_prep/merged/pnls_subset_2017_01_01_2019_04_01.rds')
if(set=='pnls') outFile = paste0(dir, 'outlier_screened/pnls_subset_2017_01_01_2019_04_01_screened.rds')
#------------------------------------

#------------------------------------
# read in and set up the data
#------------------------------------
dt = readRDS(inFile)
dt = data.table(dt)

# format the variable types for the regressions
dt[ , date:=as.Date(date)] # regression only runs with date as a date variable
dt[, org_unit_id:=as.character(org_unit_id)]

# format the pnlp data in the same format as the base data
# this assigns an element id to each variable and refers to the health zone as an org_unit for pnlp
if (set=='pnlp') {dt[, org_unit_id := paste(dps, health_zone, sep = "_")]
                  dt[, element_id:=.GRP, by='variable']}
if (set=='sigl') {dt[, variable := as.character(variable)]
                  dt[, drug := as.character(drug)]
                  dt[, drug_id:=.GRP, by='drug']
                  dt[, variable_id:=.GRP, by='variable']
                  dt[, completely_missing := NULL]}

if (set=='pnls') setnames(dt, 'element_id', 'id')
if (set=='pnls') dt[ ,element_id:=.GRP, by='id']


# aggregate to DPS level before running (if agg_to_DPS is TRUE)
if (agg_to_DPS == TRUE){
  # if all hzs at a particular date (within DPS and variable) are NA we want it to be NA (not 0, like what happens with na.rm = TRUE)
  # dt_dps = dt[, .(value = sum(value, na.rm=TRUE)), by = .(dps, date, variable, element_id )]
  
  dt[, value_dps := ifelse(!all(is.na(value)), sum(value, na.rm=TRUE), sum(value)), by = .(dps, date, variable, element_id )]
  dt = unique(dt[, .(dps, date, variable, element_id, value_dps)])
  setnames(dt, "dps", "org_unit_id")
  setnames(dt, "value_dps", "value")
}

# check that unique identifiers uniquely identify data:
if (set == 'base') {if ( nrow(unique(dt[, .(org_unit_id, date, element, element_id, category)])) != nrow(dt)) stop( "check unique identifiers...")}
if (set == 'sigl') {if ( nrow(unique(dt[, .(org_unit_id, date, variable_id, drug_id)])) != nrow(dt)) stop( "check unique identifiers...")}
#------------------------------------

#------------------------------------
# make array table to submit an array job and saved
#------------------------------------
# make array table to set up for submitting an array job
# array_table = data.table(expand.grid(unique(dt$org_unit_id)))
# setnames(array_table, "Var1", "org_unit_id")
# array_table[ ,org_unit_id:=as.character(org_unit_id)]
if (set != 'sigl'){
  array_table = data.table(expand.grid(unique(dt$element_id)))
  setnames(array_table, "Var1", "element_id")
} else {
  drugs = unique(dt$drug_id)
  vars = unique(dt$variable_id)
  array_table = data.table(expand.grid( drugs, vars ))
  names(array_table) = c("drug_id", "variable_id")
}

# save the array table and the data with IDs to /ihme/scratch/users/(user_name)/quantreg/
write.fst(array_table, arrayFile)
write.fst(dt, scratchInFile)
#------------------------------------

#------------------------------------
# run quantregScript.r as separate qsubs
#------------------------------------
# file pathways are now relative to the root of the repository

# determine the number of rows in the array job
N = nrow(array_table)

# set the working directory even if david does not want you to
setwd(paste0('/ihme/code/', user, '/gf/'))

source('./outcome_measurement/all/cod/dhis/outlier_removal/doit.R')

# FOR NEW CLUSTER:
# run quantregScript for each org_unit (submit one array job, with the array by org_unit)
if (set == 'sigl'){
  system(paste0('qsub -e ', oeDir, ' -o ', oeDir,' -q all.q -P proj_pce -N quantreg_jobs -l m_mem_free=20G -l fthread=1 -l h_rt=02:00:00 -cwd -t 1:', N, ' ./core/r_shell.sh ./outcome_measurement/all/cod/dhis/outlier_removal/quantregScript_sigl.r')) 
} else {
  system(paste0('qsub -e ', oeDir, ' -o ', oeDir,' -q all.q -P proj_pce -N quantreg_jobs  -l m_mem_free=20G -l fthread=1 -l h_rt=04:00:00 -cwd -t 1:', N, ' ./core/r_shell.sh ./outcome_measurement/all/cod/dhis/outlier_removal/quantregScript2.R')) 
} # audrey - note this sources an identical but new script, because the naming was messed up on the cluster
#------------------------------------

#------------------------------------
# wait for files to be done
#------------------------------------
i = N-1
numFiles = length(list.files(parallelDir))
while(numFiles<i) {
  print(paste0(numFiles, ' of ', i, ' jobs complete, waiting 5 seconds...'))
  numFiles = length(list.files(parallelDir))
  Sys.sleep(5) }
#------------------------------------

#---------------------------------------
# old code to concatenate files - leave as k since i and j are already used

for (k in seq(N)) {
  file = paste0(parallelDir, 'quantreg_output_', k, '.fst')
  if (file.exists(file)==TRUE) tmp = read.fst(paste0(parallelDir, 'quantreg_output_', k, '.fst'), as.data.table=TRUE)
  if (file.exists(file)==FALSE) print(paste0("File ", k, " does not exist."))
  if (file.exists(file)==FALSE) next
  if(k==1) fullData = tmp
  if(k>1) fullData = rbind(fullData, tmp)
  cat(paste0('\r', k)) }

# save the resulting file
saveRDS(fullData, outFile)
#------------------------------------

#------------------------------------
# end cleanup
#------------------------------------
# removes all files and folders within the directory
if (cleanup_end == TRUE){
  system(paste0('rm -r /ihme/scratch/users/', user, '/quantreg/*')) }
#------------------------------------







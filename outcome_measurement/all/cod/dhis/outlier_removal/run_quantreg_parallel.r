# Prep & remove outliers from the COD DHIS2 PNLS Viral Load data 

# ----------------------------------------------
# Caitlin O'Brien-Carelli / Audrey Batzel (3-7-19)
#
# 10/1/2018
# The current working directory should be the same as this script
# ----------------------------------------------

# --------------------
# Set up R
# --------------------
rm(list=ls())
library(data.table)
library(quantreg)

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
# set directories and arguments
#------------------------------------
# detect if operating on windows or on the cluster 
root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set the directory for input and output
dir <- paste0(root, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/')

# output file
outFile <- paste0(root, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/viral_load/outlier_screen/quantreg_results.rds')

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
inFile <- 'sigl_for_qr.rds'

# data set with equality constraints checked and an entry for both tests/undetectable
dt <- readRDS(paste0(dir, 'prepped/', inFile))

# remove new cases (not of interest for outlier detection)
if (inFile=='viral_load_pnls_interim.rds') { 
  dt = dt[case=='Old']
  dt[ , case:=NULL]
}

# make variable ids
if (inFile=='sigl_for_qr.rds') { dt[, variable_id:=.GRP, by='drug']}
dt[, element_id:=.GRP, by='variable']
#------------------------------------

#------------------------------------
# run quantregScript.r as separate qsubs for each subset of date, org_unit, element, and variable.
#------------------------------------
# loop over elements and org units, run quantreg once per each
i=1
for (v in unique(dt$variable_id)) {
  for (e in unique(dt$element_id)) { 
    for(o in unique(dt$org_unit_id)) { 
      # skip if this job has already run and resubmitAll is FALSE
      if (resubmitAll==FALSE & file.exists(paste0('/ihme/scratch/users/', user_name, '/qr_results/quantreg_output', i, '.rds'))) { 
         i=i+1
         next
      } else {
        # run the quantile regression and list the residuals
        system(paste0('qsub -o /ihme/scratch/users/', user_name, '/quantreg_output -e /ihme/scratch/users/', user_name, '/quantreg_output -cwd -N quantreg_output_', i, ' ../../../../../core/r_shell.sh ./quantregScript.r ', e, ' ', o, ' ', i, ' ', inFile, ' ', impute, ' ', v ))
        i=i+1
      }
    }
  }
}

# wait for files to be done
i = i-1
numFiles = length(list.files('/ihme/scratch/users/', user_name, '/qr_results'))
while(numFiles<i) { 
  print(paste0(numFiles, ' of ', i, ' jobs complete, waiting 5 seconds...'))
  numFiles = length(list.files('/ihme/scratch/users/', user_name, '/qr_results'))
  Sys.sleep(5)
}
#------------------------------------

#------------------------------------
# collect all output into one data table
#------------------------------------
for (j in seq(i)) {
  tmp = readRDS(paste0('/ihme/scratch/users/', user_name, '/qr_results/quantreg_output', j, '.rds'))
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
  system('rm /ihme/scratch/users/', user_name, '/qr_results/*')
  system('rm /ihme/scratch/users/', user_name, '/quantreg_output/*')
}
#------------------------------------
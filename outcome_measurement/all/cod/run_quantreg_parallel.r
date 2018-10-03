# Prep & remove outliers from the COD DHIS2 PNLS Viral Load data 

# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 10/1/2018
# The current working directory should be the same as this script
# ----------------------------------------------

# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(quantreg)

# --------------------
# make sure qr_results exists
# cd /ihme/scratch/users/ccarelli/

# set the working directory in the qlogin

# cd /ihme/code/ccarelli/gf/outcome_measurement/all/cod

# print the contents
# ls

# once you navigate to the directory, git pull 
# then call R

# then source this script (located in your working directory)
# source('run_quantreg_parallel.r')

#------------------------------------
# set working directories

# detect if operating on windows or on the cluster 
root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set the directory for input and output
dir <- paste0(root, '/Project/Evaluation/GF/outcome_measurement/cod/dhis/')

# output file
outFile <- paste0(root, '/Project/Evaluation/GF/outcome_measurement/cod/dhis/viral_load/quantreg_results.rds')

# whether or not to resubmit jobs that have completed already
resubmitAll = FALSE

# whether or not to delete all files from parallel runs at the end
cleanup = TRUE

#-----------------------------------
# read in the subset of PNLS data specific to viral load 

# data set with equality constraints checked and an entry for both tests/undetectable
vl <- readRDS(paste0(dir, 'prepped/viral_load_pnls_interim.rds'))

# make variable ids
vl[, element_id:=.GRP, by='variable']

# loop over elements and org units, run quantreg once per each
i=1
for (e in unique(vl$element_id)) {
  for(o in unique(vl$org_unit_id)) { 
    
    # skip if this job has already run and resubmitAll is FALSE
    if (resubmitAll==FALSE & file.exists(paste0('/ihme/scratch/users/ccarelli/qr_results/quantreg_output', i, '.rds'))) { 
       i=i+1
       next
    } else {
      # run the quantile regression and list the residuals
      system(paste0('qsub -o /ihme/scratch/users/ccarelli/quantreg_output -e /ihme/scratch/users/ccarelli/quantreg_output -cwd -N  quantreg_output_', i, ' ../../../core/r_shell.sh ./quantregScript.r ', e, ' ', o, ' ', i))
      i=i+1
    }
  }
}

# wait for files to be done
i = i-1
numFiles = length(list.files('/ihme/scratch/users/ccarelli/qr_results'))
while(numFiles<i) { 
  print(paste0(numFiles, ' of ', i, ' jobs complete, waiting 5 seconds...'))
  numFiles = length(list.files('/ihme/scratch/users/ccarelli/qr_results'))
  Sys.sleep(5)
}


# collect all output into one data table
for (j in seq(i)) {
  tmp = readRDS(paste0('/ihme/scratch/users/ccarelli/qr_results/quantreg_output', j, '.rds'))
  if(j==1) fullData = tmp
  if(j>1) fullData = rbind(fullData, tmp)
  cat(paste0('\r', j))
  flush.console() 
}

# save full data
saveRDS(fullData, outFile)

# clean up parallel files
if (cleanup==TRUE) { 
  system('rm /ihme/scratch/users/ccarelli/qr_results/*')
  system('rm /ihme/scratch/users/ccarelli/quantreg_output/*')
}

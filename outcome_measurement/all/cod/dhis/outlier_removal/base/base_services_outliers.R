# ----------------------------------------------
# Caitlin O'Brien-Carelli
# Run quantile regression on the ARVs data from PNLS
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

# to delete files in the directory
# rm qr_results/*
# rm quantreg_output/*

# set the working directory in the qlogin by navigating to it
# cd /ihme/code/ccarelli/gf/outcome_measurement/all/cod/dhis/outlier_removal/base

# print the contents
# ls

# once you navigate to the directory, git pull R
# make sure you have pushed from github desktop

# then call R

# then source this script (located in your working directory)
# source('base_services_outliers.R')

#------------------------------------
# set directories

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

#-----------------------------------
# read in the subset of PNLS data specific to viral load 

# data set with equality constraints checked and an entry for both tests/undetectable
dt = readRDS(paste0(dir, 'outliers/base_to_screen.rds'))

# loop over elements and org units, run quantreg once per each
i=1
for (e in unique(dt$element_id)) {
  for(o in unique(dt$org_unit_id)) { 
    
    # skip if this job has already run and resubmitAll is FALSE
    if (resubmitAll==FALSE & file.exists(paste0('/ihme/scratch/users/ccarelli/qr_results/quantreg_output', i, '.rds'))) { 
      i=i+1
      next
    } else {
      # run the quantile regression and list the residuals
      system(paste0('qsub -o /ihme/scratch/users/ccarelli/quantreg_output -e /ihme/scratch/users/ccarelli/quantreg_output -cwd -N quantreg_output_', i, ' ../../../../../../core/r_shell.sh ./quantregScript_base.r ', e, ' ', o, ' ', i))
      i=i+1
    }
  }
}

# wait for files to be done
i = i-1
numFiles = length(list.files('/ihme/scratch/users/ccarelli/qr_results'))
while(numFiles<i) { 
  print(paste0(numFiles, ' of ', i, ' jobs complete, waiting 2 seconds...'))
  numFiles = length(list.files('/ihme/scratch/users/ccarelli/qr_results'))
  Sys.sleep(2)
}

# 
# # collect all output into one data table
# for (j in seq(i)) {
#   tmp = readRDS(paste0('/ihme/scratch/users/ccarelli/qr_results/quantreg_output', j, '.rds'))
#   if(j==1) fullData = tmp
#   if(j>1) fullData = rbind(fullData, tmp)
#   cat(paste0('\r', j))
#   flush.console() 
# }
# 
# # save full data
# saveRDS(fullData, outFile)
# 
# # clean up parallel files
# if (cleanup==TRUE) { 
#   system('rm /ihme/scratch/users/ccarelli/qr_results/*')
#   system('rm /ihme/scratch/users/ccarelli/quantreg_output/*')
# }

#--------------------------------------------------------
# alternate code to rbind files in a cluster IDE
# do not source! run separately in an IDDE (for speed and connection)

# rbind the files together and save to the j drive 
numFiles = length(list.files('/ihme/scratch/users/ccarelli/qr_results'))
numFiles
i = 1

for (j in seq(i:numFiles)) {
  tmp = readRDS(paste0('/ihme/scratch/users/ccarelli/qr_results/quantreg_output', j, '.rds'))
  if(j==1) fullData = tmp
  if(j>1) fullData = rbind(fullData, tmp)
  cat(paste0('\r', j))
  flush.console() 
  i = i+1
}

# save the rds file to the j drive
saveRDS(fullData, paste0('/home/j/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/outliers/base_quantreg_results.rds'))

# clean up parallel files
system('rm /ihme/scratch/users/ccarelli/qr_results/*')
system('rm /ihme/scratch/users/ccarelli/quantreg_output/*')

#--------------------------------------------------------

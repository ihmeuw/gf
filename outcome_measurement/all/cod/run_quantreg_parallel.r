# Prep & remove outliers from the COD DHIS2 PNLS Viral Load data 

# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 9/19/2018
# The current working directory should be the same as this script
# ----------------------------------------------

# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(jsonlite)
library(httr)
library(ggplot2)
library(dplyr)
library(openxlsx)
library(stringr) 
library(quantreg)
# --------------------
# shell script to 
# sh /share/singularity-images/rstudio/shells/rstudio_qsub_script.sh -p 1327 -s 20 -P snis_prep

#------------------------------------
# set working directories

# detect if operating on windows or on the cluster 
root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set the directory for input and output
dir <- paste0(root, '/Project/Evaluation/GF/outcome_measurement/cod/dhis/')

# whether or not to resubmit jobs that have completed already
resubmitAll = FALSE

# whether or not to delete all files from parallel runs at the end
cleanup = TRUE

#-----------------------------------
# install quant reg and load

# install.packages("quantreg", lib=paste0(dir, 'quantreg_5.36'))
# library(SparseM, lib.loc=paste0(dir, '/quantreg_5.36/'))
# library(quantreg, lib.loc=paste0(dir, '/quantreg_5.36/'))
# ?rq

#-------------------------------------
# read in the subset of PNLS data specific to viral load 

#vl <- readRDS(paste0(dir, 'prepped/viral_load_pnls.rds'))


# interim data set
vl <- readRDS(paste0(dir, 'prepped/viral_load_pnls_interim.rds'))

# make variable ids
vl[, element_id:=.GRP, by='variable']

# loop over elements and org units, run quantreg once per each
i=1
for (e in unique(vl$element_id)) {
  for(o in unique(vl$org_unit_id)) { 
    
    # skip if this job has already run and resubmitAll is FALSE
    if (resubmitAll==FALSE & file.exists(paste0('/ihme/scratch/users/ccarelli/quantreg_output', i))) { 
       i=i+1
       next
    } else {
      # run the quantile regression and list the residuals
      system(paste0('qsub -o . -e . -cwd -N  quantreg_output_', i, ' ../../../core/r_shell.sh ./quantregScript.r ', e, ' ', o, ' ', i))
      i=i+1
    }
  }
}

# wait for files to be done
i = i-1
numFiles = length(list.files('/ihme/scratch/users/ccarelli/'))
while(numFiles<i) { 
  print(paste0(numFiles, ' of ', i, ' jobs complete, waiting 5 seconds...'))
  numFiles = length(list.files('/ihme/scratch/users/ccarelli/'))
  Sys.sleep(5)
}


# collect all output into one data table
for (j in seq(i)) {
  tmp = readRDS(paste0('/ihme/scratch/users/ccarelli/quantreg_output', j))
  if(j==1) fullData = tmp
  if(j>1) fullData = rbind(fullData, tmp)
  cat(j)
}

# save full data
saveRDS(fullData, '')

# clean up parallel files
if (cleaup==TRUE) { 
  for (j in seq(i)) {
    print(paste0('Deleting file /ihme/scratch/users/ccarelli/quantreg_output', j))
    file.remove(paste0('/ihme/scratch/users/ccarelli/quantreg_output', j))
  }
  outputFiles = list.files(pattern='quantreg_output*')
  for(f in outputFiles) { 
    print(paste0('Deleting file /ihme/scratch/users/ccarelli/quantreg_output', j))
    file.remove(paste0(f))
  }
}

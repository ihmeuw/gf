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


# loop over elements and org units, run quantreg once per each
i=1
for (v in unique(vl$variable)) {
  for(o in unique(vl$org_unit)) { 
    
    # run the quantile regression and list the residuals
    system(paste0('qsub -o. -e. -cwd -N  quantreg_', i, ' ./r_shell.sh ./quantregScript.r ', v, ' ', o, ' ', i))
    i=i+1
  }
}

# wait for files to be done
i = i-1
numFiles = length(list.files('/ihme/scratch/users/ccarelli/'))
while(numFiles<i) { 
  Sys.sleep(5)
  print(paste0(numFiles, ' of ', i, 'jobs complete, waiting 5 seconds...'))
  numFiles = length(list.files('/ihme/scratch/users/ccarelli/'))
}


# collect all output into one dataa table
j=1
for (e in unique(subset$element)) {
  for(p in unique(subset$dps)) { 
    tmp = readRDS(paste0('/share/temp/quantreg_output/file',e,p,'.rds'))
    if(j==1) fulLData = tmp
    if(j>1) fullData = rbind(fullData, tmp)
    j=j+1
  }
}


# Merge the Base Services and SIGL data downloaded from DHIS

# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 8/27/2018
#
# Upload the RDS data from DHIS2 and merge with the meta data 
# prep the data sets for analysis and the Tableau Dashboard

# ----------------------------------------------

# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2)
library(dplyr)
library(stringr) 
# --------------------
# merge on the cluster
# files take a long time to load - merge in a cluster IDE

# sh /share/singularity-images/rstudio/shells/rstudio_qsub_script.sh -p 1247 -s 10 -P snis_download  

# --------------------
# set working directories

# detect if operating on windows or on the cluster 
root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set the directory for input and output
dir <- paste0(root, '/Project/Evaluation/GF/outcome_measurement/cod/dhis/pre_prep/')

#--------------------
# Merge the base servives data sets you have downloaded
# Import base services data set and convert to a data table

# original download - 2015 to april 2017 - leave commented out
# base1 <- readRDS(paste0(dir, 'base/base_services_drc_01_2015_04_2018.rds'))
# base1 <- data.table(base1)

# input the file name of the most recently merged data set
# base1 <- readRDS(most_recent_download.rds)
# base1 <- data.table(base1)

# load the newest set of data 
base2 <- readRDS(paste0(dir, 'base/base_services_drc_05_2018_07_2018.rds'))
base2 <- data.table(base2)

# merge the previously downloaded set with the new download
base <- rbind(base1, base2)

# save the merged data 
# alter the file name to include all included dates
saveRDS(base, paste0(dir, 'merged/base_services_drc_01_2015_07_2018.rds'))

#----------------------------------------------
# merge the SIGL data 

# input the name of the most recently merged data set
sigl1 <- readRDS(paste0(dir, 'sigl/sigl_drc_01_2015_05_2018.rds'))
sigl1 <- data.table(sigl1)

# load the newest data set
sigl2 <- readRDS(paste0(dir, 'sigl/sigl_drc_05_2018_07_2018.rds'))
sigl2 <- data.table(sigl2)

# merge the previously downloaded data set with the new download
sigl <- rbind(sigl1, sigl2)

# save the merged data
# alter the file name to include all included dates
saveRDS(base, paste0(dir, 'merged/base_services_drc_01_2015_07_2018.rds'))










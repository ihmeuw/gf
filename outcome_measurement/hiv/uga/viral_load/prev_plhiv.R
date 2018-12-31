# Calculate PLHIV and prevalence
# Caitlin O'Brien-Carelli
# Prep UVL data for analysis
# 12/31/2018
#
# ----------------------------------------------

# --------------------
# Set up R
rm(list=ls())
library(ggplot2)
library(stringr) 
library(plyr)
library(data.table)
# --------------------

# --------------------
# detect if operating on windows or on the cluster 

root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set working directory
dir = paste0(root, '/Project/Evaluation/GF/outcome_measurement/uga/lbd_prev')
setwd(dir)

#--------------------------------------------------------
# import the mmortality and dalys data from gbd 

dt = fread(paste0(dir, '/gbd_deaths_dalys.csv'), stringsAsFactors = FALSE)
tri_sex = c('#bd0026', '#74c476', '#3182bd')